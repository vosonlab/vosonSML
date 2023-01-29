#' @title Create twitter 2-mode network
#'
#' @description Creates a 2-mode network from tweets returned from the twitter search query. In this network there are
#'   two types of nodes, twitter users who authored or were mentioned in collected tweets and hashtags found within
#'   tweets. Network edges represent a users tweets that contain hashtags or mention users screen names.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"twomode"}.
#' @param removeTermsOrHashtags Character vector. Users or hashtags to remove from the twomode network. For example,
#'   this parameter could be used to remove the user or hashtag that was used to collect the data by removing any nodes
#'   with matching name. Default is \code{NULL} to remove none.
#' @param rmRetweets Logical. Do not process retweets in the input data. Default is \code{TRUE}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a twitter 2-mode network graph with the hashtag "#auspol" removed
#' net_2mode <- collect_tw |>
#'   Create("twomode", removeTermsOrHashtags = c("#auspol"), verbose = TRUE)
#'
#' # network
#' # net_2mode$nodes
#' # net_2mode$edges
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

    prompt_and_stop(c("stringi", "vctrs"), "Create.twomode.twitter")
    check_stri_icu_version()
    
    msg("Generating twitter 2-mode network...\n")

    datasource <- datasource$tweets
    if (check_df_n(datasource) < 1) {
      stop("Datasource invalid or empty.", call. = FALSE)
    }

    df_stats <-
      network_stats(NULL, "collected tweets", nrow(datasource))

    datasource <-
      datasource |> dplyr::select(
        .data$status_id,
        .data$user_id,
        .data$screen_name,
        .data$full_text,
        .data$created_at,
        .data$is_retweet,
        .data$is_quote,
        .data$is_reply,
        .data$rts
      )

    if (rmRetweets) {
      datasource <- datasource |>
        dplyr::filter(!.data$is_retweet) |>
        dplyr::select(-.data$rts)
    } else {
      datasource <- retweet_full_text(datasource)
    }

    datasource$text <- textutils::HTMLdecode(datasource$full_text)

    df_tokens <-
      datasource |>
      unnest_tweets(
        .data$word,
        .data$text,
        strip_url = TRUE
      )

    df_tokens <- df_tokens |>
      dplyr::mutate(at_name = paste0("@", tolower(.data$screen_name)))

    # classification of tokens
    df_tokens <- df_tokens |>
      dplyr::mutate(
        type = data.table::fcase(
          stringr::str_detect(.data$word, "^#.*"),
          "hashtag",
          stringr::str_detect(.data$word, "^@.*"),
          "user",
          default = "term"
        )
      ) |>
      dplyr::filter(.data$type %in% c("hashtag", "user") & .data$at_name != .data$word)

    if (!is.null(removeTermsOrHashtags) && length(removeTermsOrHashtags) > 0) {
      removeTermsOrHashtags <- unlist(lapply(removeTermsOrHashtags, tolower))
      token_count <- nrow(df_tokens)

      msg(paste0("Removing terms and hashtags: ",
        paste0(as.character(removeTermsOrHashtags), collapse = ", "),
        "\n"))

      df_tokens <- df_tokens |> dplyr::filter(
        !(.data$word %in% removeTermsOrHashtags) &
        !(.data$user_id %in% removeTermsOrHashtags) &
        !(.data$at_name %in% removeTermsOrHashtags)
      )

      df_stats <- network_stats(
        df_stats, "removed specified",
        token_count - nrow(df_tokens),
        FALSE
      )
    }

    df_stats <- network_stats(
      df_stats, "users",
      nrow(df_tokens |> dplyr::filter(.data$type == "user")),
      FALSE
    )

    df_stats <- network_stats(
      df_stats, "hashtags",
      nrow(df_tokens |> dplyr::filter(.data$type == "hashtag")),
      FALSE
    )

    edges <- df_tokens |>
      dplyr::mutate(from = .data$at_name, to = .data$word, type_from = "user") |>
      dplyr::select(
        .data$from,
        .data$to,
        type_to = .data$type,
        .data$type_from,
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

    nodes <- dplyr::bind_rows(
      edges |> dplyr::select(name = .data$from, type = .data$type_from),
      edges |> dplyr::select(name = .data$to, type = .data$type_to)
    ) |> dplyr::distinct()

    edges <- edges |> dplyr::select(-.data$type_from, -.data$type_to)

    nodes <- nodes |>
      dplyr::left_join(
        df_tokens |> dplyr::select(.data$at_name, .data$user_id, .data$screen_name) |> dplyr::distinct(),
        by = c("name" = "at_name")
      ) |>
      dplyr::mutate(
        screen_name = data.table::fifelse(
          (.data$type == "user" & is.na(.data$screen_name)),
          substring(.data$name, 2),
          tolower(.data$screen_name)
        )
      )

    df_stats <- network_stats(df_stats, "nodes", nrow(nodes))
    df_stats <- network_stats(df_stats, "edges", nrow(edges))
    msg(network_stats(df_stats, print = TRUE))

    net <- list("nodes" = nodes, "edges" = edges)
    class(net) <- append(c("network", "twomode", "twitter"), class(net))
    msg("Done.\n")

    net
  }
