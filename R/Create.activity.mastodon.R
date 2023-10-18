#' @title Create mastodon activity network
#'
#' @description Creates a mastodon activity network from collected posts. Nodes are posts and directed edges represent
#'   the relationship of posts to one another.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"mastodon"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param verbose Logical. Output additional information. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a mastodon activity network
#' activity_net <- mastodon_data |> Create("activity")
#' }
#'
#' @export
Create.activity.mastodon <-
  function(datasource,
           type,
           verbose = FALSE,
           ...) {
    
    msg("Generating mastodon activity network...\n")
    
    if (check_df_n(datasource$posts) < 1) {
      stop("Datasource invalid or empty.", call. = FALSE)
    }
    
    if (!"tags" %in% names(datasource$posts)) datasource$posts$tags <- NA
    
    edges <- datasource$posts |>
      dplyr::select(from = .data$id, to = .data$in_reply_to_id, .data$created_at) |>
      dplyr::filter(!is.na(.data$to)) |>
      dplyr::mutate(edge_type = "reply")

    nodes <- datasource$posts |>
      dplyr::select(.data$id, .data$created_at, .data$visibility, .data$account, "tags",
                    .data$reblogs_count, .data$favourites_count, .data$replies_count, .data$url) |>
      tidyr::hoist(.data$account, account.id = "id", account.username = "username",
                   account.acct = "acct", account.displayname = "display_name") |>
      tidyr::hoist(.data$tags, tag = list("name"), tag_url = list("url")) |>
      dplyr::select(-.data$account) |>
      dplyr::mutate(node_type = "post")
    
    # add referenced posts absent from nodes
    nodes <- add_absent_nodes(nodes, edges)
    
    net <- list("nodes" = nodes, "edges" = edges)
    class(net) <- append(c("network", "activity", "mastodon"), class(net))
    msg("Done.\n")
    
    net
  }
