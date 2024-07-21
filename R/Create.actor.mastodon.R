#' @title Create mastodon actor network
#'
#' @description Creates a mastodon actor network from posts. Mastodon users who have posted are actor nodes. The
#'   created network is directed with edges representing replies.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"mastodon"} class names.
#' @param type Character string. Type of network to be created, set to \code{"actor"}.
#' @param subtype Character string. Subtype of actor network to be created. Can be set to \code{"server"}. Default is
#'   \code{NULL}.
#' @param inclMentions Logical. Create edges for users mentioned or tagged in posts. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' @param writeToFile Logical. Write data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a mastodon actor network
#' actor_net <- mastodon_data |> Create("actor")
#'
#' # create a mastodon server relations network
#' actor_net <- mastodon_data |> Create("actor", "server")
#' }
#'
#' @export
Create.actor.mastodon <-
  function(datasource,
           type,
           subtype = NULL,
           inclMentions = TRUE,
           ...,
           writeToFile = FALSE,
           verbose = TRUE) {
    
    msg("Generating mastodon actor network...\n")
    
    if (check_df_n(datasource$posts) < 1) stop("Datasource invalid or empty.", call. = FALSE)
    
    check_chr(subtype, param = "subtype", accept = "server", null.ok = TRUE)
    
    relations <- datasource$posts |>
      dplyr::select(
        post.id = "id",
        post.created_at = "created_at",
        "account",
        "in_reply_to_account_id",
        "mentions"
      ) |>
      tidyr::hoist(
        "account",
        user.id = "id",
        user.acct = "acct",
        user.username = "username",
        user.displayname = "display_name",
        user.url = "url",
        user.avatar = "avatar"
      ) |>
      dplyr::select(-"account") |>
      dplyr::mutate(edge_type = "reply")
    
    edges <- relations |>
      dplyr::select(
        from = "user.id",
        to = "in_reply_to_account_id",
        "post.id",
        "post.created_at",
        "edge_type")
    
    nodes <- relations |>
      dplyr::select(dplyr::starts_with("user.")) |>
      dplyr::distinct(.data$user.id, .keep_all = TRUE) |>
      dplyr::mutate(type = "author")

    if (inclMentions) {
      mentions <- relations |>
        tidyr::hoist(
          "mentions",
          mention.user.id = list("id"),
          mention.user.acct = list("acct"),
          mention.user.username = list("username"),
          mention.user.url = list("url")
        ) |>
        tidyr::unnest_longer(
          c(
            "mention.user.id",
            "mention.user.acct",
            "mention.user.username",
            "mention.user.url"
          )
        ) |>
        dplyr::filter(.data$mention.user.id != .data$in_reply_to_account_id) |> # mention is already a direct reply
        dplyr::mutate(edge_type = "mention")
      
      edges <- edges |>
        dplyr::bind_rows(
          mentions |>
            dplyr::select(
              from = "user.id",
              to = "mention.user.id",
              "post.id",
              "post.created_at",
              "edge_type"
            )
        )
      
      nodes <- nodes |>
        dplyr::bind_rows(
          mentions |>
            dplyr::select(dplyr::starts_with("mention.user.")) |>
            dplyr::rename_with(~ gsub("mention.", "", .x, fixed = TRUE)) |>
            dplyr::mutate(type = "mention")
        ) |>
        dplyr::arrange("user.id", "type") |>
        dplyr::distinct(.data$user.id, .keep_all = TRUE)
    }

    # add referenced actors absent from nodes
    nodes <- add_absent_nodes(nodes, edges, id = "user.id")
    
    if (!is.null(subtype)) {
      servers <- nodes |>
        dplyr::select("user.id", "user.url") |>
        dplyr::filter(!is.na(.data$user.url)) |>
        dplyr::mutate(user.server = purrr::map_chr(.data$user.url, .f = function(.x) httr2::url_parse(.x)$hostname))
      
      edges <- edges |>
        dplyr::left_join(servers |> dplyr::select("user.id", from.server = "user.server"), by = c("from" = "user.id")) |>
        dplyr::left_join(servers |> dplyr::select("user.id", to.server = "user.server"), by = c("to" = "user.id")) |>
        dplyr::select(from = "from.server", to = "to.server", "edge_type") |>
        dplyr::filter(!is.null(.data$to) & .data$to != "NULL")
      
      nodes <- servers |>
        dplyr::group_by(.data$user.server) |>
        dplyr::summarise(n = dplyr::n())
    }
    
    edges <- edges |> dplyr::filter(!is.na(.data$to))
    
    net <- list("edges" = edges, "nodes" = nodes)
    class(net) <- append(c("network", "actor", switch(!is.null(subtype), "server", NULL), "mastodon"), class(net))
    
    if (writeToFile) write_output_file(net, "rds", "MastodonActorNet", verbose = verbose)
    
    msg("Done.\n")
    
    net
  }
