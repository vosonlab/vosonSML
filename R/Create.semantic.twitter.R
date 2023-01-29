#' @title Create twitter semantic network
#'
#' @description Creates a semantic network from tweets returned from the twitter search query. Semantic networks
#'   describe the semantic relationships between concepts. In this network the concepts are significant words and
#'   hashtags extracted from the tweet text. Network edges are weighted and represent occurrence of words and hashtags
#'   in the same tweets.
#'
#' @note The words and hashtags passed to the function in the \code{removeTermsOrHashtags} parameter are removed before
#'   word frequencies are calculated and are therefore excluded from top percentage of most frequent terms completely
#'   rather than simply filtered out of the final network.
#'
#'   The top percentage of frequently occurring hashtags \code{hashtagFreq} and words \code{termFreq} are calculated to
#'   a minimum frequency and all terms that have an equal or greater frequency than the minimum are included in the
#'   network as nodes. For example, of unique hashtags of varying frequencies in a dataset the top 50% of total
#'   frequency or most common hashtags may calculate to being the first 20 hashtags. The frequency of the 20th hashtag
#'   is then used as the minimum and all hashtags of equal or greater frequency are included as part of the top 50% most
#'   frequently occurring hashtags. So the number of top hashtags may end up being greater than 20 if there is more than
#'   one hashtag that has frequency matching the minimum. The exception to this is if the minimum frequency is 1 and the
#'   \code{hashtagFreq} is set to less than 100, in this case only the first 20 hashtags will be included.
#'
#'   Hashtags and words in the top percentages are included in the network as isolates if there are no instances of them
#'   occurring in tweet text with other top percentage frequency terms.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"semantic"}.
#' @param removeRetweets Logical. Removes detected retweets from the tweet data. Default is \code{TRUE}.
#' @param removeTermsOrHashtags Character vector. Words or hashtags to remove from the semantic network. For example,
#'   this parameter could be used to remove the search term or hashtag that was used to collect the data by removing any
#'   nodes with matching name. Default is \code{NULL} to remove none.
#' @param stopwords Logical. Removes stopwords from the tweet data. Default is \code{TRUE}.
#' @param stopwordsLang Character string. Language of stopwords to use. Refer to the \pkg{stopwords} package for further
#'   information on supported languages. Default is \code{"en"}.
#' @param stopwordsSrc Character string. Source of stopwords list. Refer to the \pkg{stopwords} package for further
#'   information on supported sources. Default is \code{"smart"}.
#' @param removeNumbers Logical. Removes whole numerical tokens from the tweet text. For example, a year value such as
#'   \code{2020} will be removed but not mixed values such as \code{G20}. Default is \code{TRUE}.
#' @param removeUrls Logical. Removes twitter shortened URL tokens from the tweet text. Default is \code{TRUE}.
#' @param termFreq Numeric integer. Specifies the percentage of most frequent words to include. For example,
#'   \code{termFreq = 20} means that the 20 percent most frequently occurring \code{words} will be included in the
#'   semantic network as nodes. A larger percentage will increase the number of nodes and therefore the size of graph.
#'   The default value is \code{5}, meaning the top 5 percent most frequent words are used.
#' @param hashtagFreq Numeric integer. Specifies the percentage of most frequent \code{hashtags} to include. For
#'   example, \code{hashtagFreq = 20} means that the 20 percent most frequently occurring hashtags will be included in
#'   the semantic network as nodes. The default value is \code{50}.
#' @param assoc Character string. Association of nodes. A value of \code{"limited"} includes only edges between most
#'   frequently occurring hashtags and terms. A value of \code{"full"} includes ties between most frequently occurring
#'   hashtags and terms, hashtags and hashtags, and terms and terms. Default is \code{"limited"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # twitter semantic network creation additionally requires the stopwords
#' # package for working with text data
#' # install.packages("stopwords")
#'
#' # create a twitter semantic network graph removing the hashtag "#auspol"
#' # and using the top 2% frequently occurring words and 10% most frequently
#' # occurring hashtags as nodes
#' net_semantic <- collect_tw |>
#'   Create("semantic", removeTermsOrHashtags = c("#auspol"),
#'          termFreq = 2, hashtagFreq = 10, verbose = TRUE)
#'
#' # network
#' # net_semantic$nodes
#' # net_semantic$edges
#' }
#'
#' @export
Create.semantic.twitter <-
  function(datasource,
           type,
           removeRetweets = TRUE,
           removeTermsOrHashtags = NULL,
           stopwords = TRUE,
           stopwordsLang = "en",
           stopwordsSrc = "smart",
           removeNumbers = TRUE,
           removeUrls = TRUE,
           termFreq = 5,
           hashtagFreq = 50,
           assoc = "limited",
           verbose = TRUE,
           ...) {

    prompt_and_stop(c("stringi", "vctrs", "stopwords"), "Create.twomode.twitter")
    check_stri_icu_version()
    
    msg("Generating twitter semantic network...\n")

    datasource <- datasource$tweets
    if (check_df_n(datasource) < 1) {
      stop("Datasource invalid or empty.", call. = FALSE)
    }

    termFreq <- check_perc(termFreq, "termFreq")
    hashtagFreq <- check_perc(hashtagFreq, "hashtagFreq")

    assoc <- check_chr(assoc, param = "assoc", accept = c("limited", "full"))

    if (is.null(removeTermsOrHashtags)) {
      removeTermsOrHashtags <- c()
    } else {
      if (!is.vector(removeTermsOrHashtags) && !is.list(removeTermsOrHashtags)) {
        stop("removeTermsOrHashtags must be a vector.", call. = FALSE)
      }
    }

    removeRetweets <- check_lgl(removeRetweets, "removeRetweets")
    stopwords <- check_lgl(stopwords, "stopwords")
    removeNumbers <- check_lgl(removeNumbers, "removeNumbers")
    removeUrls <- check_lgl(removeUrls, "removeUrls")

    stopwordsLang <- check_chr(stopwordsLang, param = "stopwordsLang")
    stopwordsSrc <- check_chr(stopwordsSrc, param = "stopwordsSrc")

    class(datasource) <- rm_collect_cls(class(datasource))

    data <- datasource |>
      dplyr::select(
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

    df_stats <- network_stats(NULL, "collected tweets", nrow(data))
    df_stats <- network_stats(NULL, "retweets", nrow(data |> dplyr::filter(.data$is_retweet)))

    if (removeRetweets) {
      data <- data |>
        dplyr::filter(!.data$is_retweet) |>
        dplyr::select(-.data$rts)
    } else {
      data <- retweet_full_text(data)
    }

    data$text <- textutils::HTMLdecode(data$full_text)

    tokens_df <-
      data |>
      unnest_tweets(
        .data$word,
        .data$text
      )

    df_stats <- network_stats(df_stats, "tokens", nrow(tokens_df), FALSE)

    tokens_df <- tokens_df |>
      dplyr::mutate(at_name = paste0("@", tolower(.data$screen_name)))

    if (stopwords) {
      rlang::check_installed(c("stopwords"), "for Create.semantic.twitter")
      stopwords_ <- tibble::tibble(
        word = stopwords::stopwords(language = stopwordsLang, source = stopwordsSrc),
        lexicon = stopwordsSrc
      )

      tokens_df <- tokens_df |>
        dplyr::anti_join(stopwords_, by = c("word" = "word"))
    }

    if (!is.null(removeTermsOrHashtags) && length(removeTermsOrHashtags) > 0) {

      removeTermsOrHashtags <- unlist(lapply(removeTermsOrHashtags, tolower))

      token_count <- nrow(tokens_df)

      msg(
        paste0("Removing terms and hashtags: ",
        paste0(as.character(removeTermsOrHashtags), collapse = ", "), "\n")
      )

      tokens_df <- tokens_df |> dplyr::filter(!.data$word %in% removeTermsOrHashtags)

      df_stats <- network_stats(
        df_stats, "removed specified",
        token_count - nrow(tokens_df),
        FALSE
      )
    }

    freq_df <- tokens_df |> dplyr::count(.data$word, sort = TRUE)

    # classification of tokens
    freq_df <- freq_df |>
      dplyr::mutate(
        type = data.table::fcase(
          stringr::str_detect(.data$word, "^#.+$"),
          "hashtag",
          stringr::str_detect(.data$word, "^@.+$"),
          "user",
          stringr::str_detect(.data$word, "^\\d+$"),
          "number",
          stringr::str_detect(.data$word, "^http(s)?.+"),
          "url",
          default = "term"
        ))

    freq_type_df <- freq_df |> dplyr::group_by(.data$type) |> dplyr::tally(.data$n)

    n_unique_hashtags <- freq_df |> dplyr::filter(.data$type == "hashtag") |> nrow()

    # filter out types or reclassify to term
    filter_by_type <- function(x, type, rm = TRUE) {
      if (rm) {
        x <- x |> dplyr::filter(.data$type != "{{ type }}")
      } else {
        x <- x |> dplyr::mutate(type = ifelse(.data$type == "{{ type }}", "term", .data$type))
      }
      x
    }

    freq_df <- freq_df |> filter_by_type("user", rm = TRUE)
    freq_df <- freq_df |> filter_by_type("number", rm = removeNumbers)
    freq_df <- freq_df |> filter_by_type("url", rm = removeUrls)

    # verbose output
    if (verbose) {
      n_users <- freq_type_df |> dplyr::filter(.data$type == "user")

      n_hashtags <- freq_df |> dplyr::filter(.data$type == "hashtag") |> dplyr::pull(.data$n) |> sum()
      u_hashtags <- freq_df |> dplyr::filter(.data$type == "hashtag") |> nrow()

      n_terms <- freq_df |> dplyr::filter(.data$type == "term") |> dplyr::pull(.data$n) |> sum()
      u_terms <- freq_df |> dplyr::filter(.data$type == "term") |> nrow()


      if (nrow(n_users) > 0) {
        df_stats <- network_stats(df_stats, "removed users", n_users |> dplyr::pull(.data$n), FALSE)
      }

      if (u_hashtags > 0) {
        df_stats <- network_stats(df_stats, "hashtag count", n_hashtags, FALSE)
        df_stats <- network_stats(df_stats, "hashtags unique", u_hashtags, FALSE)
      }

      if (u_terms > 0) {
        df_stats <- network_stats(df_stats, "term count", n_terms, FALSE)
        df_stats <- network_stats(df_stats, "terms unique", u_terms, FALSE)
      }
    }

    # proportions of hashtags and terms
    props <- list(list(type = "hashtag", p = hashtagFreq),
                  list(type = "term", p = termFreq))

    get_prop <- function(x, prop) {
      x <- x |>
        dplyr::filter(.data$type == prop$type) |>
        dplyr::arrange(dplyr::desc(.data$n))

      top_n <- x |>
        dplyr::slice_head(prop = prop$p / 100)

      floor_n <- top_n |>
        dplyr::slice_tail(n = 1) |>
        dplyr::pull(.data$n)

      if (length(floor_n) < 1) return(NULL)

      x <- x |> dplyr::filter(.data$n >= !!floor_n) |>
        dplyr::mutate(floor_n = !!floor_n)

      x
    }

    freq_df <- purrr::map_dfr(props, get_prop, x = freq_df)

    # verbose output
    if (verbose) {
      hashtag_floor_n <- freq_df |> dplyr::filter(.data$type == "hashtag") |> dplyr::distinct(.data$floor_n) |>
        dplyr::pull()

      df_stats <- network_stats(
        df_stats, paste0("top ", hashtagFreq, "% hashtags n (>=", hashtag_floor_n, ")"),
        nrow(freq_df |> dplyr::filter(.data$type == "hashtag")),
        FALSE
      )

      term_floor_n <- freq_df |> dplyr::filter(.data$type == "term") |> dplyr::distinct(.data$floor_n) |>
        dplyr::pull()

      df_stats <- network_stats(
        df_stats, paste0("top ", termFreq, "% terms n (>=", term_floor_n, ")"),
        nrow(freq_df |> dplyr::filter(.data$type == "term")),
        FALSE
      )
    }

    nodes <- tokens_df |> dplyr::left_join(freq_df, by = c("word" = "word")) |>
      dplyr::filter(!is.na(.data$type)) |>
      dplyr::select(.data$status_id, .data$word, .data$type, .data$n)

    edges <- nodes |> dplyr::select(-.data$n)
    nodes <- nodes |> dplyr::select(-.data$status_id) |> dplyr::distinct()

    edges <- edges |>
      dplyr::left_join(edges, by = "status_id") |>
      dplyr::group_by(.data$status_id) |>
      dplyr::filter(.data$word.x != .data$word.y) |>
      dplyr::ungroup()

    edges <- edges |>
      dplyr::rowwise() |>
      dplyr::mutate(word.a = sort(c(.data$word.x, .data$word.y))[1],
                    word.b = sort(c(.data$word.x, .data$word.y))[2])

    edges <- edges |>
      dplyr::distinct(.data$status_id, .data$word.a, .data$word.b, .keep_all = TRUE)

    if (assoc == "limited") {
      edges <- edges |> dplyr::filter(.data$type.x != .data$type.y)
    }

    nodes <- edges |> dplyr::select(word = .data$word.x) |>
      dplyr::bind_rows(edges |> dplyr::select(word = .data$word.y)) |>
      dplyr::distinct() |>
      dplyr::left_join(nodes, by = "word")

    edges <- edges |>
      dplyr::select(from = .data$word.x,
                    to = .data$word.y,
                    from.type = .data$type.x,
                    to.type = .data$type.y,
                    .data$status_id #,
                    #.data$word.a, .data$word.b
      )

    df_stats <- network_stats(df_stats, "nodes", nrow(nodes))
    df_stats <- network_stats(df_stats, "edges", nrow(edges))
    msg(network_stats(df_stats, print = TRUE))

    net <- list("edges" = edges, "nodes" = nodes)
    class(net) <- append(c("network", "semantic", "twitter"), class(net))
    msg("Done.\n")

    net
  }
