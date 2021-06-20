#' @title Create twitter actor network
#'
#' @description Creates a twitter actor network from tweets returned from the twitter search query. Twitter users who
#'   have tweeted, retweeted or been mentioned in a tweet are actor nodes. The created network is directed with edges of
#'   different types representing retweets, quote tweets, mentions and replies to other users. Users who have tweeted
#'   without ties to other users will appear in the network graph as nodes with self-loops.
#'
#' @note When creating twitter actor networks, a network with additional user information can be generated using the
#'   \code{\link{AddUserData}} function. Additional calls can be made to the twitter API to get information about users
#'   that were identified as nodes during network creation but did not tweet (meaning no user profile information was
#'   initially collected for them).
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"actor"}.
#' @param rmEdgeTypes Character vector. List of edge types to remove from network. Options are \code{"tweet"},
#'   \code{"retweet"}, \code{"reply"} and \code{"quote"}. Default is \code{NULL}.
#' @param inclMentions Logical. Create edges for users mentioned or tagged in tweets. Default is \code{FALSE}.
#' @param inclRtMentions Logical. Create edges for users mentioned or tagged in retweets. For tweet types other than
#'   retweets the collected tweet author has created the mention, for retweets the original tweet author has created the
#'   mention not the retweeter. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a twitter actor network excluding retweet, quote tweets and mention edges
#' actor_net <- twitter_data %>%
#'   Create("actor2", rmEdgeTypes = c("retweet", "quote"), inclMentions = TRUE)
#'
#' # network nodes and edges
#' names(actor_net)
#' # "nodes", "edges"
#' names(actor_net$nodes)
#' # "user_id", "screen_name"
#' names(actor_net$edges)
#' # "from", "to", "status_id", "created_at", "edge_type"
#'
#' }
#'
#' @export
Create.actor2.twitter <-
  function(datasource,
           type,
           rmEdgeTypes = NULL,
           inclMentions = FALSE,
           inclRtMentions = FALSE,
           verbose = TRUE,
           ...) {
    cat("Generating twitter actor network...")
    if (verbose) {
      cat("\n")
    }
    df_stats <-
      network_stats(NULL, "collected tweets", nrow(datasource))

    # select data columns
    datasource <- datasource %>%
      dplyr::select(
        .data$status_id,
        .data$screen_name,
        dplyr::starts_with("is_"),
        dplyr::ends_with("user_id"),
        dplyr::ends_with("screen_name"),
        dplyr::starts_with("reply_"),
        dplyr::ends_with("created_at"),
        dplyr::starts_with("mentions")
      )

    # classify edges
    edges <- datasource %>%
      dplyr::mutate(
        type = data.table::fcase(
          .data$is_retweet == TRUE,
          "retweet",
          !is.na(.data$reply_to_status_id) &
            .data$is_quote == TRUE,
          "reply,quote",
          !is.na(.data$reply_to_status_id) &
            .data$is_quote == FALSE,
          "reply",
          is.na(.data$reply_to_status_id) &
            .data$is_quote == TRUE,
          "quote",
          default = "tweet"
        )
      )

    types <- c("tweet", "retweet", "reply", "quote", "reply,quote")
    rmEdgeTypes <-
      rmEdgeTypes[trimws(tolower(rmEdgeTypes)) %in% types]

    edges <- edges %>% dplyr::filter(!(.data$type %in% rmEdgeTypes))

    # extract mentions
    # reply,quote mentions are treated as reply mentions
    edges_mentions <- NULL
    if (inclMentions) {
      tweet_mentions <-
        retweet_mentions <- reply_mentions <- quote_mentions <- NULL

      # unnest mentions for tweets that have them
      edges_mentions_unnest <-
        edges %>% dplyr::filter(!is.na(.data$mentions_user_id)) %>%
        tidyr::unnest(cols = c("mentions_user_id", "mentions_screen_name"))

      # mentions in the tweet
      if (!("tweet" %in% rmEdgeTypes)) {
        tweet_mentions <-
          edges_mentions_unnest %>% dplyr::filter(.data$type == "tweet") %>%
          dplyr::mutate(type = "tweet mention")
      }

      # mentions in the retweet
      if (!("retweet" %in% rmEdgeTypes) && inclRtMentions) {
        retweet_mentions <-
          edges_mentions_unnest %>% dplyr::filter(.data$type == "retweet") %>%
          dplyr::mutate(
            type = data.table::fifelse(
              .data$retweet_user_id != .data$mentions_user_id,
              "retweet mention",
              "retweet"
            )
          ) %>%
          dplyr::filter(.data$type != "retweet")
      }

      # mentions in the reply - reply,quote mentions are reply mentions
      if (!("reply" %in% rmEdgeTypes)) {
        reply_mentions <-
          edges_mentions_unnest %>%
          dplyr::filter(.data$type == "reply" |
                          .data$type == "reply,quote") %>%
          dplyr::mutate(
            type = data.table::fifelse(
              .data$reply_to_user_id != .data$mentions_user_id,
              "reply mention",
              "reply"
            )
          ) %>%
          dplyr::filter(.data$type != "reply")
      }

      # mentions in the tweet containing the quoted tweet
      if (!("quote" %in% rmEdgeTypes)) {
        quote_mentions <-
          edges_mentions_unnest %>%
          dplyr::filter(.data$type == "quote") %>%
          dplyr::mutate(type = "quote mention")
      }

      edges_mentions <-
        dplyr::bind_rows(tweet_mentions,
                         retweet_mentions,
                         reply_mentions,
                         quote_mentions)

      # mentions from to edge list
      if (nrow(edges_mentions)) {
        edges_mentions <-
          dplyr::select(
            edges_mentions,
            .data$status_id,
            from = .data$user_id,
            from_screen_name = .data$screen_name,
            to = .data$mentions_user_id,
            to_screen_name = .data$mentions_screen_name,
            .data$created_at,
            .data$type
          )
      }
    }

    # separate reply,quote tweets
    edges_rq <- edges %>%
      dplyr::filter(type == "reply,quote") %>%
      tidyr::separate_rows(type, sep = ",", convert = FALSE)

    edges_rq <-
      edges_rq %>% dplyr::filter(!(.data$type %in% rmEdgeTypes))

    # from to edge list
    edges <- edges %>%
      dplyr::filter(type != "reply,quote") %>%
      dplyr::bind_rows(edges_rq) %>%
      dplyr::mutate(
        to = data.table::fcase(
          .data$type == "tweet",
          .data$user_id,
          .data$type == "retweet",
          .data$retweet_user_id,
          .data$type == "quote",
          .data$quoted_user_id,
          .data$type == "reply",
          .data$reply_to_user_id,
          default = NA_character_
        ),
        to_screen_name = data.table::fcase(
          .data$type == "tweet",
          .data$screen_name,
          .data$type == "retweet",
          .data$retweet_screen_name,
          .data$type == "quote",
          .data$quoted_screen_name,
          .data$type == "reply",
          .data$reply_to_screen_name,
          default = NA_character_
        )
      ) %>%
      dplyr::select(
        .data$status_id,
        from = .data$user_id,
        from_screen_name = .data$screen_name,
        to = .data$to,
        to_screen_name = .data$to_screen_name,
        .data$created_at,
        .data$type
      )

    # add mentions edges
    edges <- dplyr::bind_rows(edges, edges_mentions)

    # edge stats
    edge_stats <-
      edges %>% dplyr::group_by(.data$type) %>% dplyr::tally() %>% dplyr::arrange(dplyr::desc(.data$type))
    for (row in 1:nrow(edge_stats)) {
      df_stats <-
        network_stats(df_stats, edge_stats[row, "type"], edge_stats[row, "n"], TRUE)
    }

    # get nodes from edge list
    nodes <- dplyr::bind_rows(
      dplyr::select(edges, user_id = "from", screen_name = "from_screen_name"),
      dplyr::select(edges, user_id = "to", screen_name = "to_screen_name")
    ) %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$user_id) %>% # if multiple screen names then merge
      dplyr::mutate(screen_name = paste0(.data$screen_name, collapse = ", ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE)

    edges <-
      edges %>% dplyr::select(.data$from,
                              .data$to,
                              .data$status_id,
                              .data$created_at,
                              edge_type = .data$type)

    # print stats
    if (verbose) {
      df_stats <- network_stats(df_stats, "nodes", nrow(nodes))
      df_stats <- network_stats(df_stats, "edges", nrow(edges))
      network_stats(df_stats, print = TRUE)
    }

    network <- list("edges" = edges, "nodes" = nodes)
    class(network) <-
      append(c("network", "actor", "twitter"), class(network))
    cat("Done.\n")
    network
  }
