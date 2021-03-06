#' @title Create twitter 2-mode network
#'
#' @description Creates a 2-mode network from tweets returned from the twitter search query. In this network there are
#' two types of nodes, twitter users who authored or were mentioned in collected tweets and hashtags found within
#' tweets. Network edges represent a users tweets that contain hashtags or mention users screen names.
#'
#' The creation of twitter 2-mode networks requires text processing and the tokenization of tweets. As such
#' this function requires the additional installation of the \pkg{tidytext} package to achieve this.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"twomode"}.
#' @param removeTermsOrHashtags Character vector. Users or hashtags to remove from the twomode network. For example,
#' this parameter could be used to remove the user or hashtag that was used to collect the data by removing any
#' nodes with matching name. Default is \code{NULL} to remove none.
#' @param weighted Logical. Add weights to network edges. If set to \code{FALSE} tweet \code{status_id} and
#' \code{created_at} fields will be preserved for edges in the dataframe. Default is \code{TRUE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # twitter 2-mode network creation additionally requires the tidytext package
#' # for working with text data
#' install.packages("tidytext")
#'
#' # create a twitter 2-mode network graph removing the hashtag '#auspol' as it was used in
#' # the twitter search query
#' twomodeNetwork <- twitterData %>%
#'                   Create("twomode", removeTermsOrHashtags = c("#auspol"), verbose = TRUE)
#'
#' # network
#' # twomodeNetwork$nodes
#' # twomodeNetwork$edges
#' }
#'
#' @export
Create.twomode.twitter <-
  function(datasource,
           type,
           removeTermsOrHashtags = NULL,
           weighted = TRUE,
           verbose = TRUE,
           ...) {
    rlang::check_installed(c("tidytext"), "for Create.twomode.twitter")
    stop_req_pkgs(c("tidytext"), "Create.twomode.twitter")

    cat("Generating twitter 2-mode network...")
    if (verbose) {
      cat("\n")
    }

    if (verbose) {
      df_stats <-
        network_stats(NULL, "collected tweets", nrow(datasource))
    }

    # datasource <- tibble::as_tibble(datasource)
    class(datasource) <- rm_collect_cls(class(datasource))

    datasource <-
      datasource %>% dplyr::select(
        .data$status_id,
        .data$user_id,
        .data$screen_name,
        .data$text,
        .data$created_at,
        .data$is_retweet,
        .data$is_quote
      )
    datasource$text = textutils::HTMLdecode(datasource$text)

    capture.output(
      tokens_df <-
        datasource %>% tidytext::unnest_tokens(
          .data$word,
          .data$text,
          token = "tweets",
          to_lower = TRUE
        )
      ,
      type = "output"
    )
    tokens_df %<>% dplyr::mutate(at_name = paste0("@", tolower(.data$screen_name)))

    if (!is.null(removeTermsOrHashtags) &&
        length(removeTermsOrHashtags) > 0) {
      removeTermsOrHashtags <-
        unlist(lapply(removeTermsOrHashtags, tolower))
      token_count <- nrow(tokens_df)
      if (verbose) {
        cat(paste0(
          "Removing terms and hashtags: ",
          paste0(as.character(removeTermsOrHashtags), collapse = ", "),
          "\n"
        ))
      }
      tokens_df %<>% dplyr::filter(
        !(.data$word %in% removeTermsOrHashtags) &
          !(.data$user_id %in% removeTermsOrHashtags) &
          !(tolower(.data$screen_name) %in% removeTermsOrHashtags) &
          !(.data$at_name %in% removeTermsOrHashtags)
      )

      if (verbose) {
        df_stats <-
          network_stats(df_stats,
                        "removed specified",
                        token_count - nrow(tokens_df),
                        FALSE)
      }
    }

    tokens_df %<>% dplyr::mutate(type = dplyr::if_else(
      grepl("^#.*", .data$word),
      "hashtag",
      dplyr::if_else(grepl("^@.*", .data$word), "user", "term")
    )) %>%
      dplyr::filter(.data$type %in% c("hashtag", "user") &
                      .data$at_name != .data$word)

    if (verbose) {
      df_stats <-
        network_stats(df_stats,
                      "users",
                      nrow(tokens_df %>% dplyr::filter(.data$type == "user")),
                      FALSE)
      df_stats <-
        network_stats(df_stats,
                      "hashtags",
                      nrow(tokens_df %>% dplyr::filter(.data$type == "hashtag")),
                      FALSE)
    }

    edges <-
      tokens_df %>% dplyr::mutate(from = .data$at_name, to = .data$word) %>%
      dplyr::select(
        .data$from,
        .data$to,
        .data$status_id,
        .data$created_at,
        .data$is_retweet,
        .data$is_quote
      )

    if (weighted) {
      edges %<>% dplyr::count(.data$from, .data$to, name = "weight")
    }

    nodes <-
      dplyr::distinct(tibble::tibble(name = c(edges$to, edges$from))) %>%
      dplyr::left_join(
        tokens_df %>% dplyr::select(.data$at_name, .data$user_id) %>%
          dplyr::distinct(),
        by = c("name" = "at_name")
      )
    if (verbose) {
      df_stats <- network_stats(df_stats, "nodes", nrow(nodes))
      df_stats <- network_stats(df_stats, "edges", nrow(edges))

      network_stats(df_stats, print = TRUE)
    }

    func_output <- list("nodes" = nodes,
                        "edges" = edges)

    class(func_output) <-
      append(class(func_output), c("network", "twomode", "twitter"))
    cat("Done.\n")

    func_output
  }
