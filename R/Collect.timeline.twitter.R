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

    msg("Collecting timeline tweets for users...\n")

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

    tl_params[['n']] <- numTweets
    tl_params['verbose'] <- verbose
    tl_params['parse'] <- TRUE

    # additional twitter api params
    dots <- substitute(...())
    tl_params <- append(tl_params, dots)

    # looped because of user attribute
    get_tls <- function() {
      tw_df <- tw_df_users <- NULL
      for (u in users) {
        tl_params[['user']] <- u

        df <- do.call(rtweet::get_timeline, tl_params)
        df_users <- attr(df, "users", exact = TRUE)

        if (!is.null(tw_df)) {
          tw_df_users <- attr(tw_df, "users", exact = TRUE)
          tw_df <- tw_df |> dplyr::bind_rows(df)
          tw_df_users <- dplyr::bind_rows(tw_df_users, df_users)

          attr(tw_df, "users") <- tw_df_users
        } else {
          tw_df <- df
        }
      }

      tw_df
    }

    tweets_df <- get_tls()

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

    tweets_df <- tweets_df |> modify_tweet_data(users = user_names, rtweet_created_at = TRUE)

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
