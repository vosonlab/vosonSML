#' @title Create mastodon actor network
#'
#' @description Creates a mastodon actor network from posts. Mastodon users who have posted are actor nodes. The
#'   created network is directed with edges representing replies.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"mastodon"} class names.
#' @param type Character string. Type of network to be created, set to \code{"actor"}.
#' @param inclMentions Logical. Create edges for users mentioned or tagged in posts. Default is \code{TRUE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a mastodon actor network
#' actor_net <- mastodon_data |> Create("actor")
#' }
#'
#' @export
Create.actor.mastodon <-
  function(datasource,
           type,
           inclMentions = TRUE,
           verbose = FALSE,
           ...) {
    
    msg("Generating mastodon actor network...\n")
    
    if (check_df_n(datasource$posts) < 1) {
      stop("Datasource invalid or empty.", call. = FALSE)
    }
    
    relations <- datasource$posts |>
      dplyr::select(post.id = .data$id, .data$account, .data$in_reply_to_account_id, .data$mentions,
                    .data$created_at) |>
      tidyr::hoist(.data$account,
                   account.id = "id",
                   account.acct = "acct",
                   account.username = "username",
                   account.displayname = "display_name") |>
      dplyr::select(-.data$account) |>
      dplyr::mutate(edge_type = "reply")
    
    edges <- relations |>
      dplyr::select(from = .data$account.id, to = .data$in_reply_to_account_id, .data$post.id, .data$created_at,
                    .data$edge_type)
    
    if (inclMentions) {
      mentions <- relations |>
        dplyr::select(.data$post.id, .data$account.id, .data$in_reply_to_account_id, .data$mentions,
                      .data$created_at) |>
        tidyr::hoist(.data$mentions,
                     mention.account.id = list("id"),
                     mention.account.acct = list("acct"),
                     mention.account.username = list("username"),
                     mention.account.displayname = list("display_name")) |>
        tidyr::unnest_longer(
          c(.data$mention.account.id,
            .data$mention.account.acct,
            .data$mention.account.username,
            .data$mention.account.displayname)
        ) |>
        dplyr::filter(.data$mention.account.id != .data$in_reply_to_account_id) |> # mention is already a direct reply
        dplyr::mutate(edge_type = "mention")
      
      edges <- edges |>
        dplyr::bind_rows(
          mentions |>
            dplyr::select(from = .data$account.id,
                          to = .data$mention.account.id,
                          .data$post.id,
                          .data$created_at,
                          .data$edge_type)
        )
    }
    
    edges <- edges |> dplyr::filter(!is.na(.data$to))
    
    nodes <- datasource$users |> dplyr::mutate(node_type = "user")
    
    # add referenced actors absent from nodes
    nodes <- add_absent_nodes(nodes, edges)
    
    net <- list("edges" = edges, "nodes" = nodes)
    class(net) <- append(c("network", "actor", "mastodon"), class(net))
    msg("Done.\n")
    
    net
  }
