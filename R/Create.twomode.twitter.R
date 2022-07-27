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
#' @param rmRetweets Logical. Do not process retweets in the input data. Default is \code{TRUE}.
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
#' # create a twitter 2-mode network graph with the hashtag '#auspol' removed
#' twomodeNetwork <- twitterData |>
#'   Create("twomode",
#'          removeTermsOrHashtags = c("#auspol"), verbose = TRUE)
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
           rmRetweets = TRUE,
           verbose = TRUE,
           ...) {
    rlang::check_installed(c("tidytext"), "for Create.twomode.twitter")
    stop_req_pkgs(c("tidytext"), "Create.twomode.twitter")

    msg("Generating twitter 2-mode network...\n")

    df_stats <-
      network_stats(NULL, "collected tweets", nrow(datasource))

    class(datasource) <- rm_collect_cls(class(datasource))

    datasource <-
      datasource |> dplyr::select(
        .data$status_id,
        .data$user_id,
        .data$screen_name,
        .data$full_text,
        .data$created_at,
        .data$is_retweet,
        .data$is_quote,
        .data$is_reply
      )

    if (rmRetweets) {
      datasource <- datasource |>
        dplyr::filter(!.data$is_retweet)
    }

    datasource$text <- textutils::HTMLdecode(datasource$full_text)

    x <- suppressMessages(
      capture.output(
        tokens_df <-
          datasource |>
          tidytext::unnest_tweets(
            .data$word,
            .data$text,
            strip_url = TRUE
          )
        , type = "output"))

    tokens_df <- tokens_df |>
      dplyr::mutate(at_name = paste0("@", tolower(.data$screen_name)))

    # classification of tokens
    tokens_df <- tokens_df |>
      dplyr::mutate(
        type = dplyr::if_else(
          stringr::str_detect(.data$word, "^#.*"), "hashtag",
            dplyr::if_else(stringr::str_detect(.data$word, "^@.*"), "user", "term")
        )) |>
      dplyr::filter(.data$type %in% c("hashtag", "user") & .data$at_name != .data$word)

    if (!is.null(removeTermsOrHashtags) && length(removeTermsOrHashtags) > 0) {
      removeTermsOrHashtags <- unlist(lapply(removeTermsOrHashtags, tolower))
      token_count <- nrow(tokens_df)

      msg(paste0("Removing terms and hashtags: ",
        paste0(as.character(removeTermsOrHashtags), collapse = ", "),
        "\n"))

      tokens_df <- tokens_df |> dplyr::filter(
        !(.data$word %in% removeTermsOrHashtags) &
        !(.data$user_id %in% removeTermsOrHashtags) &
        !(.data$at_name %in% removeTermsOrHashtags)
      )

      df_stats <- network_stats(
        df_stats, "removed specified",
        token_count - nrow(tokens_df),
        FALSE
      )
    }

    df_stats <- network_stats(
      df_stats, "users",
      nrow(tokens_df |> dplyr::filter(.data$type == "user")),
      FALSE
    )

    df_stats <- network_stats(
      df_stats, "hashtags",
      nrow(tokens_df |> dplyr::filter(.data$type == "hashtag")),
      FALSE
    )

    edges <-
      tokens_df |> dplyr::mutate(from = .data$at_name, to = .data$word) |>
      dplyr::select(
        .data$from,
        .data$to,
        .data$status_id,
        .data$created_at,
        .data$is_retweet,
        .data$is_quote,
        .data$is_reply
      )

    edges <- edges |>
      dplyr::group_by(.data$from, .data$to) |>
      dplyr::mutate(weight = dplyr::n()) |>
      dplyr::ungroup()

    nodes <-
      dplyr::distinct(tibble::tibble(name = c(edges$to, edges$from))) |>
      dplyr::left_join(
        tokens_df |> dplyr::select(.data$at_name, .data$user_id) |>
          dplyr::distinct(),
        by = c("name" = "at_name")
      )

    df_stats <- network_stats(df_stats, "nodes", nrow(nodes))
    df_stats <- network_stats(df_stats, "edges", nrow(edges))
    msg(network_stats(df_stats, print = TRUE))

    network <- list("nodes" = nodes, "edges" = edges)
    class(network) <-
      append(c("network", "twomode", "twitter"), class(network))
    msg("Done.\n")

    network
  }
