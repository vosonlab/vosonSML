#' @title Create twitter activity network
#'
#' @description Creates a twitter activity network from collected tweets. Nodes are tweets and directed edges represent
#'   the relationship of tweets to one another. For example, there is a directed edge from a quote tweet towards the
#'   tweet that was quoted. Stand-alone tweets that are not replies, retweets or quote tweets have no relation to others
#'   and will be isolates.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param rmNodeTypes Character vector. List of tweet types to remove from network. Options are \code{"tweet"},
#'   \code{"retweet"}, \code{"reply"} and \code{"quote"}. Default is \code{NULL}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a twitter activity network with retweets removed
#' activity_net <- twitter_data %>%
#'   Create("activity2", rmNodeTypes = c("retweet"))
#'
#' # network nodes and edges
#' # activity_net$nodes
#' # activity_net$edges
#' }
#'
#' @export
Create.activity2.twitter <-
  function(datasource,
           type,
           rmNodeTypes = NULL,
           verbose = TRUE,
           ...) {

    cat("Generating twitter activity network...")
    if (verbose) { cat("\n") }
    df_stats <- network_stats(NULL, "collected tweets", nrow(datasource))

    # select data columns
    datasource <- datasource %>%
      dplyr::select(
        dplyr::ends_with("status_id"),
        .data$user_id,
        dplyr::starts_with("is_"),
        dplyr::ends_with("created_at")
      )

    # classify edges
    edges <- datasource %>%
      dplyr::mutate(
        type = data.table::fcase(
          .data$is_retweet == TRUE,
          "retweet",
          !is.na(.data$reply_to_status_id) & .data$is_quote == TRUE,
          "reply,quote",
          !is.na(.data$reply_to_status_id) & .data$is_quote == FALSE,
          "reply",
          is.na(.data$reply_to_status_id) & .data$is_quote == TRUE,
          "quote",
          default = "tweet"
        )
      )

    # remove edge type list
    types <- c("tweet", "retweet", "reply", "quote")
    rmNodeTypes <- rmNodeTypes[trimws(tolower(rmNodeTypes)) %in% types]

    edges <- edges %>%
      tidyr::separate_rows(type, sep = ",", convert = FALSE) %>%
      dplyr::filter(!(.data$type %in% rmNodeTypes))

    edges <- edges %>%
      dplyr::mutate(
        to = data.table::fcase(
          .data$type == "tweet",
          .data$status_id,
          .data$type == "retweet",
          .data$retweet_status_id,
          .data$type == "quote",
          .data$quoted_status_id,
          .data$type == "reply",
          .data$reply_to_status_id,
          default = NA_character_
        ),
        to_created_at = data.table::fcase(
          .data$type == "tweet",
          .data$created_at,
          .data$type == "retweet",
          .data$retweet_created_at,
          .data$type == "quote",
          .data$quoted_created_at
        )
      )

    # edge stats
    edge_stats <-
      edges %>% dplyr::group_by(.data$type) %>% dplyr::tally() %>% dplyr::arrange(dplyr::desc(.data$type))
    for (row in 1:nrow(edge_stats)) {
      df_stats <-
        network_stats(df_stats, edge_stats[row, "type"], edge_stats[row, "n"], TRUE)
    }

    nodes <- dplyr::bind_rows(
      edges %>% dplyr::select(.data$status_id,
                              .data$created_at),
      edges %>% dplyr::select(status_id = .data$to,
                              created_at = .data$to_created_at)
    ) %>% dplyr::distinct(.data$status_id, .keep_all = TRUE)

    edges <- edges %>%
      dplyr::select(
        from = .data$status_id,
        .data$to,
        .data$user_id,
        .data$created_at,
        edge_type = .data$type
      )

    # print stats
    if (verbose) {
      df_stats <- network_stats(df_stats, "nodes", nrow(nodes))
      df_stats <- network_stats(df_stats, "edges", nrow(edges))
      network_stats(df_stats, print = TRUE)
    }

    network <- list("nodes" = nodes, "edges" = edges)
    class(network) <-
      append(c("network", "activity", "twitter"), class(network))
    cat("Done.\n")
    network
  }
