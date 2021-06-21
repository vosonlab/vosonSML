#' @title Create reddit activity network
#'
#' @description Creates a reddit activity network from subreddit thread comments. Nodes are comments and initial thread
#' posts, edges form the discussion structure and signify to which comment or post a comment has been made to.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"reddit"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a reddit activity network graph
#' activityNetwork <- redditData %>% Create("activity")
#'
#' # network
#' # activityNetwork$nodes
#' # activityNetwork$edges
#' }
#'
#' @export
Create.activity.reddit <-
  function(datasource, type, verbose = TRUE, ...) {
    cat("Generating reddit activity network...")
    if (verbose) {
      cat("\n")
    }

    # df <- tibble::as_tibble(datasource)
    class(datasource) <- rm_collect_cls(class(datasource))

    df_stats <-
      network_stats(NULL, "collected reddit comments", nrow(datasource))

    # would be better with the unique comment fullname ids
    # comment id format <thread_id>.<structure>
    datasource %<>% dplyr::mutate(comment_id = paste0(.data$thread_id, ".", .data$structure))

    # edges
    df_relations <- datasource %>%
      dplyr::rename("from" = .data$comment_id) %>%
      dplyr::mutate(to = ifelse(
        !grepl("_", .data$structure),
        paste0(.data$thread_id, ".0"),
        gsub("_\\d+$", "", .data$from)
      )) %>%
      dplyr::mutate(edge_type = "comment") %>%
      dplyr::select(.data$from, .data$to, .data$edge_type)

    # nodes
    df_nodes <-
      datasource %>% dplyr::select(
        .data$comment_id,
        .data$thread_id,
        .data$comm_id,
        .data$comm_date,
        .data$comm_date_unix,
        .data$subreddit,
        .data$user
      ) %>%
      dplyr::rename(
        "id" = .data$comment_id,
        "datetime" = .data$comm_date,
        "ts" = .data$comm_date_unix
      ) %>%
      dplyr::mutate(node_type = "comment")

    # add thread posts to nodes
    thread_ids <-
      datasource %>% dplyr::select(
        .data$subreddit,
        .data$author,
        .data$thread_id,
        .data$post_date,
        .data$post_date_unix
      ) %>%
      dplyr::mutate(id = paste0(.data$thread_id, ".0")) %>%
      dplyr::distinct(.data$subreddit, .data$id, .keep_all = TRUE) %>%
      dplyr::rename(
        "user" = .data$author,
        "datetime" = .data$post_date,
        "ts" = .data$post_date_unix
      ) %>%
      dplyr::select(.data$id,
                    .data$thread_id,
                    .data$datetime,
                    .data$ts,
                    .data$subreddit,
                    .data$user) %>%
      dplyr::mutate(node_type = "thread")

    df_nodes <-
      dplyr::bind_rows(df_nodes, dplyr::anti_join(thread_ids, df_nodes, by = c("id", "subreddit")))

    df_stats <- network_stats(df_stats, "threads", nrow(thread_ids))
    df_stats <- network_stats(df_stats, "comments", nrow(datasource))
    df_stats <- network_stats(df_stats, "nodes", nrow(df_nodes))
    df_stats <- network_stats(df_stats, "edges", nrow(df_relations))

    # print stats
    if (verbose) {
      network_stats(df_stats, print = TRUE)
    }

    func_output <- list("nodes" = df_nodes,
                        "edges" = df_relations)

    class(func_output) <-
      append(class(func_output), c("network", "activity", "reddit"))
    cat("Done.\n")

    return(func_output)
  }
