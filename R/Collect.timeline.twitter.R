#' @title Collect tweet data from twitter timelines
#'
#' @description This function collects user timeline tweets and structures the data into a dataframe with the class
#'   names \code{"datasource"} and \code{"twitter"}. The Twitter API limits collection to a maximum of 3,200 of the most
#'   recent timeline tweets per user.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"twitter"}.
#' @param endpoint API endpoint.
#' @param users Character vector. Specifies one or more twitter users. Can be user names, user ids or a mixture.
#' @param numTweets Numeric vector. Specifies how many tweets to be collected per user. Defaults to single value of \code{100}.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{FALSE}.
#' @inheritDotParams rtweet::get_timeline -token -user -n -max_id -home
#'
#' @return A data.frame object with class names \code{"datasource"} and \code{"twitter"}.
#'
#' @export
Collect.timeline.twitter <-
  function(credential,
           endpoint,
           users = c(),
           numTweets = c(100),
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {
    rlang::check_installed("rtweet", "for Collect.timeline.twitter")
    stop_req_pkgs(c("rtweet"), "Collect.timeline.twitter")

    cat("Collecting timeline tweets for users...\n")

    authToken <- credential$auth

    if (!("Token" %in% class(authToken))) {
      stop(
        "OAuth token missing. Please use the Authenticate function to create and supply a token.",
        call. = FALSE
      )
    }

    if (!is.vector(users) || length(users) < 1) {
      stop("Please provide a vector of one or more users.",
           call. = FALSE)
    }

    if (!is.numeric(numTweets) || numTweets < 1 || numTweets > 3200) {
      stop("Please provide a value for numTweets between 1 and 3200.",
           call. = FALSE)
    }

    tl_params <- list()
    tl_params[['token']] <- authToken

    tl_params[['user']] <- users
    tl_params[['n']] <- numTweets
    tl_params['verbose'] <- verbose

    # additional twitter api params
    dots <- substitute(...())
    tl_params <- append(tl_params, dots)

    tweets_df <- do.call(rtweet::get_timeline, tl_params)

    # summary
    if (nrow(tweets_df) > 0) {

      n_show <- 1
      if (nrow(tweets_df) >= 2) {
        n_show <- 2
      }

      first_tweets <- tweets_df %>%
        dplyr::slice_head(n = n_show) %>%
        dplyr::mutate(tweet = c("Latest Obs", "")[1:n_show])

      last_tweets <- tweets_df %>%
        dplyr::slice_tail(n = n_show) %>%
        dplyr::mutate(tweet = c("", "Earliest Obs")[1:n_show])

      results_df <- dplyr::bind_rows(first_tweets, last_tweets) %>%
        dplyr::mutate(created = as.character(.data$created_at)) %>%
        dplyr::select(.data$tweet,
                      .data$status_id,
                      .data$created,
                      .data$screen_name)

      cat("\n")
      print_summary(results_df)
    }
    cat(paste0("Collected ", nrow(tweets_df), " tweets.\n"))

    class(tweets_df) <-
      append(c("datasource", "twitter"), class(tweets_df))
    if (writeToFile) {
      write_output_file(tweets_df, "rds", "TwitterData")
    }

    cat("Done.\n")

    tweets_df
  }
