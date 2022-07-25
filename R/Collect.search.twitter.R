#' @title Collect tweet data from twitter search
#'
#' @description This function collects tweet data based on search terms and structures the data into a dataframe with
#'   the class names \code{"datasource"} and \code{"twitter"}.
#'
#'   The twitter Standard search API sets a rate limit of 180 requests every 15 minutes. A maximum of 100 tweets can be
#'   collected per search request meaning the maximum number of tweets per operation is 18000 / 15 minutes. More tweets
#'   can be collected by using \code{retryOnRateLimit = TRUE} parameter which will cause the collection to pause if the
#'   rate limit is reached and resume when the rate limit resets (in approximately 15 minutes). Alternatively the
#'   twitter API parameter \code{since_id} can be used in a later session to resume a twitter search collection from the
#'   last tweet previously collected as tweet status id's are sequential. The Standard API only returns tweets for the
#'   last 7 days.
#'
#'   All of the search query operators available through the twitter API can be used in the \code{searchTerm} field. For
#'   example, to search for tweets containing the term \code{"love"} or \code{"hate"} the \code{"OR"} operator can be
#'   used in the term field: \code{searchTerm = "love OR hate"}. For more information refer to the twitter API
#'   documentation for query operators:
#'   \url{https://developer.twitter.com/en/docs/twitter-api/v1/tweets/search/guides/standard-operators}.
#'
#' @note Additional parameters passed to this function in the ellipsis \code{...} will also be passed to the Twitter
#'   search API request. Most parameters have been covered but a complete list can be found here:
#'   \url{https://developer.twitter.com/en/docs/twitter-api/v1/tweets/search/api-reference/get-search-tweets} A useful
#'   additional parameter is \code{language} allowing the user can restrict tweets returned to a particular language
#'   using an ISO 639-1 code. For example, to restrict a search to tweets in English the value \code{language = "en"}
#'   can be passed to this function.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"twitter"}.
#' @param endpoint API endpoint.
#' @param searchTerm Character string. Specifies a twitter search term. For example, \code{"Australian politics"} or the
#'   hashtag \code{"#auspol"}.
#' @param searchType Character string. Returns filtered tweets as per search type \code{recent}, \code{mixed} or
#'   \code{popular}. Default type is \code{recent}.
#' @param numTweets Numeric. Specifies how many tweets to be collected. Defaults is \code{100}.
#' @param includeRetweets Logical. Specifies if the search should filter out retweets. Defaults is \code{TRUE}.
#' @param retryOnRateLimit Logical. Default is \code{FALSE}.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{FALSE}.
#' @inheritDotParams rtweet::search_tweets -token -q -n -type -include_rts -retryonratelimit -verbose
#'
#' @return A data.frame object with class names \code{"datasource"} and \code{"twitter"}.
#'
#' @examples
#' \dontrun{
#' # search and collect 100 recent tweets for the hashtag #auspol
#' myTwitterData <- twitterAuth |>
#'   Collect(searchTerm = "#auspol", searchType = "recent", numTweets = 100, verbose = TRUE,
#'           includeRetweets = FALSE, retryOnRateLimit = TRUE, writeToFile = TRUE)
#' }
#'
#' @export
Collect.search.twitter <-
  function(credential,
           endpoint,
           searchTerm = "",
           searchType = "recent",
           numTweets = 100,
           includeRetweets = TRUE,
           retryOnRateLimit = FALSE,
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {
    rlang::check_installed("rtweet", "for Collect.twitter")
    stop_req_pkgs(c("rtweet"), "Collect.twitter")

    msg("Collecting tweets for search query...\n")

    authToken <- credential$auth

    if (!("Token" %in% class(authToken))) {
      stop(
        "OAuth token missing. Please use the Authenticate function to create and supply a token.",
        call. = FALSE
      )
    }

    searchTerm <- trimws(searchTerm)
    if (searchTerm != "") {
      msg(paste0("Search term: ", searchTerm, "\n"))
    }

    rtlimit <- NULL
    tryCatch({
      rtlimit <- rtweet::rate_limit("search/tweets", token = authToken)
    }, error = function(e) {
      msg("Unable to determine rate limit.\n")
    })

    if (!is.null(rtlimit)) {
      remaining <- rtlimit[["remaining"]]

      if (!is.null(remaining) &&
          is.numeric(remaining) && (remaining > 0)) {
        # 100 tweets returned per api request
        remaining <- remaining * 100
        msg(
          paste0(
            "Requested ",
            numTweets,
            " tweets of ",
            remaining,
            " in this search rate limit.\n"
          )
        )

        if (retryOnRateLimit == TRUE & numTweets < remaining) {
          msg("Less tweets requested than remaining limit retryOnRateLimit set to FALSE.\n")
          retryOnRateLimit <- FALSE
        }
      }
      msg(paste0("Rate limit reset: ", rtlimit$reset_at, "\n"))

    } else {
      msg(paste0("Requested ", numTweets, " tweets.\n"))
    }

    search_params <- list()
    search_params[['token']] <- authToken

    search_params['q'] <- searchTerm
    search_params['type'] <- searchType
    search_params['n'] <- numTweets
    search_params['include_rts'] <- includeRetweets
    search_params['retryonratelimit'] <- retryOnRateLimit
    search_params['verbose'] <- verbose
    search_params['parse'] <- FALSE

    # additional twitter api params
    dots <- substitute(...())
    search_params <- append(search_params, dots)

    tweets_df <- do.call(rtweet::search_tweets, search_params)

    # modified parsing step
    # so created_at is not converted to local time
    tweets <- lapply(tweets_df, "[[", "statuses")
    tweets_df <- rtweet::tweets_with_users(tweets)

    users_df <- attr(tweets_df, "users", exact = TRUE)
    user_names <- NULL
    if (!is.null(users_df)) {
      users_df <- users_df |>
        dplyr::rename(user_id = .data$id_str)

      attr(tweets_df, "users") <- NULL

      user_names <- users_df |>
        dplyr::select(.data$user_id, .data$screen_name) |>
        dplyr::rename_with(function(x) paste0("u.", x))
    }

    tweets_df <- tweets_df |> modify_tweet_data(users = user_names)

    if (is.null(tweets_df)) { tweets_df <- tibble::tibble() }

    # summary
    if (nrow(tweets_df) > 0) {

      first_tweets <- tweets_df |>
        dplyr::slice_head(n = 1) |>
        dplyr::mutate(tweet = "Latest Obs")

      last_tweets <- tweets_df |>
        dplyr::slice_tail(n = 1) |>
        dplyr::mutate(tweet = "Earliest Obs")

      results_df <- dplyr::bind_rows(first_tweets, last_tweets) |>
        dplyr::mutate(created = as.character(.data$created_at)) |>
        dplyr::select(.data$tweet,
                      .data$status_id,
                      .data$created)

      msg("\n")
      msg(print_summary(results_df))
    }
    msg(paste0("Collected ", nrow(tweets_df), " tweets.\n"))

    class(tweets_df) <-
      append(c("datasource", "twitter"), class(tweets_df))

    attr(tweets_df, "users") <- users_df

    if (writeToFile) {
      write_output_file(tweets_df, "rds", "TwitterData")
    }

    msg("Done.\n")

    tweets_df
  }
