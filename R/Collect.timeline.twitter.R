#' @title Collect tweet data from twitter timelines
#'
#' @description This function collects user timeline tweets and structures the data into a dataframe with the class
#'   names \code{"datasource"} and \code{"twitter"}. The Twitter API limits collection to a maximum of 3,200 of the most
#'   recent timeline tweets per user.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"twitter"}.
#' @param endpoint API endpoint.
#' @param users Character vector. Specifies one or more twitter users. Can be user names, user ids or a mixture.
#' @param numTweets Numeric vector. Specifies how many tweets to be collected per user. Defaults to single value of
#'   \code{100}.
#' @param retryOnRateLimit Logical. When the API rate-limit is reached should the collection wait and resume when it
#'   resets. Default is \code{TRUE}.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{FALSE}.
#' @inheritDotParams rtweet::get_timeline -token -user -n -max_id -home
#'
#' @return A tibble object with class names \code{"datasource"} and \code{"twitter"}.
#'
#' @export
Collect.timeline.twitter <-
  function(credential,
           endpoint,
           users = c(),
           numTweets = 100,
           retryOnRateLimit = TRUE,
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {

    prompt_and_stop("rtweet", "Collect.timeline.twitter")

    msg("Collecting timeline tweets for users...\n")

    auth_token <- credential$auth

    if (!("Token" %in% class(auth_token)) && !("rtweet_bearer" %in% class(auth_token))) {
      stop(
        "OAuth token missing. Please use the Authenticate function to create and supply a token.",
        call. = FALSE
      )
    }

    invisible({
      check_chr(users, param = "users", min = 1)
      check_num(numTweets, param = "numTweets")
      check_lgl(retryOnRateLimit, param = "retryOnRateLimit")
      check_lgl(writeToFile, param = "writeToFile")
      check_lgl(verbose, param = "verbose")
    })

    if (any(numTweets < 1)) {
      # max 3200
      stop("Please provide values for numTweets between 1 and Inf.", call. = FALSE)
    }

    if (length(numTweets) == 1) numTweets <- rep(numTweets, length(users))

    f_params <- list()
    f_params[["token"]] <- auth_token
    f_params["retryonratelimit"] <- retryOnRateLimit
    f_params["verbose"] <- verbose
    f_params["parse"] <- TRUE

    dots <- substitute(...())
    f_params <- append(f_params, dots)

    # get timelines looped because of parsing issue
    get_tls <- function() {
      df_tweets <- df_users <- NULL

      for (i in seq_along(users)) {
        df_tweets_i <- df_users_i <- NULL

        f_params[["user"]] <- users[i]
        f_params[["n"]] <- numTweets[i]

        df_tweets_i <- do.call(rtweet::get_timeline, f_params)
        df_users_i <- attr(df_tweets_i, "users", exact = TRUE)

        if (!is.null(df_tweets)) {
          df_tweets <- df_tweets |> dplyr::bind_rows(df_tweets_i)
          df_users <- dplyr::bind_rows(
            attr(df_tweets, "users", exact = TRUE),
            df_users_i
          )
          attr(df_tweets, "users") <- df_users
        } else {
          df_tweets <- df_tweets_i
        }
      }

      df_tweets
    }

    rate_limit <- NULL
    tryCatch({
      rate_limit <- rtweet::rate_limit("statuses/user_timeline", token = auth_token)
    }, error = function(e) {
      msg("Unable to determine rate limit.\n")
    })

    if (!is.null(rate_limit)) {
      remaining <- rate_limit[["remaining"]]

      if (!is.null(remaining) &&
          is.numeric(remaining) && (remaining > 0)) {
        # 100 tweets returned per api request
        remaining <- remaining * 100
        msg(
          paste0(
            "Requested ",
            sum(numTweets),
            " tweets of ",
            remaining,
            " in this search rate limit.\n"
          )
        )
      }
      msg(paste0("Rate limit reset: ", rate_limit$reset_at, "\n"))

    } else {
      msg(paste0("Requested ", sum(numTweets), " tweets.\n"))
    }

    tl_tweets <- get_tls()

    df_tweets <- tl_tweets |> import_rtweet_(rtweet_created_at = TRUE)
    n_tweets <- check_df_n(df_tweets$tweets)

    # summary
    if (n_tweets > 0) {
      tweet_first <- df_tweets$tweets |>
        dplyr::slice_head(n = 1) |>
        dplyr::mutate(tweet = "Latest Obs")

      tweet_last <- df_tweets$tweets |>
        dplyr::slice_tail(n = 1) |>
        dplyr::mutate(tweet = "Earliest Obs")

      df_summary <- dplyr::bind_rows(tweet_first, tweet_last) |>
        dplyr::mutate(created = as.character(.data$created_at)) |>
        dplyr::select(.data$tweet, .data$status_id, .data$created)

      msg("\n")
      msg(print_summary(df_summary))
    }

    msg(paste0("Collected ", n_tweets, " tweets.\n"))

    if (writeToFile) {
      write_output_file(df_tweets, "rds", "TwitterData", verbose = verbose)
    }

    msg("Done.\n")

    df_tweets
  }
