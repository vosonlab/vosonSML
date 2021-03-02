#' @title Collect tweet data from twitter conversation
#'
#' @description This function collects conversation tweets.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"twitter"}.
#' @param tweet_id Numeric. Tweet id of any tweet in the twitter conversation to collect.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A data.frame object with class names \code{"datasource"} and \code{"twitter_convo"}.
#'
#' @note bearer rate-limits (per app):
#'   300 tweet lookup requests (300*100 = 30,000 tweets) per 15 min window
#'   450 recent search requests (450*100 = 45,000 tweets) per 15 min window
#'
#' @export
Collect.twitter_convo <-
  function(credential,
           tweet_id,
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {
    cat("Collecting tweets for conversation...\n")
    if (is.null(credential$bearer)) {
      stop("Please provide a twitter bearer token.", call. = FALSE)
    }
    token <- httr::add_headers(Authorization = paste0("Bearer ", credential$bearer))

    # get conversation id for tweet
    url <- paste0("https://api.twitter.com/2/tweets/", tweet_id, "?tweet.fields=conversation_id")
    resp <- httr::GET(url, token)
    if (resp$status != 200) {
      stop(paste0("Tweets API response error code: ", resp$status, ".\n",
                  "tweet_id: ", tweet_id, "\n"), call. = FALSE)
    }

    resp_obj <- resp_data(resp)
    convo_id <- resp_obj$data$conversation_id

    if (is.null(convo_id)) {
      stop("Failed to get conversation id.", call. = FALSE)
    }

    # tweet fields to collect
    fields <- paste0(c("in_reply_to_user_id",
                       "author_id",
                       "created_at",
                       "conversation_id",
                       "referenced_tweets",
                       "source"), collapse = ",")

    # get conversation starter tweet - not returned by search
    url <- paste0("https://api.twitter.com/2/tweets/", convo_id, "?tweet.fields=", fields)
    resp <- httr::GET(url, token)
    resp_obj <- resp_data(resp)
    if (resp$status != 200) {
      cat(paste0("Tweets API response error code: ", resp$status, ".\n",
                 "conversation_id: ", convo_id, "\n"))
    }

    # create dataframe
    convo_df <- tibble::as_tibble(resp_obj$data)

    # initial search tweets
    search_url <- paste0("https://api.twitter.com/2/tweets/search/recent?query=conversation_id:", convo_id,
                         "&tweet.fields=", fields, "&max_results=100")

    # search tweets and add results to dataframe
    resp <- httr::GET(search_url, token)
    resp_obj <- resp_data(resp)

    if (resp$status == 200) {
      convo_df <- dplyr::bind_rows(resp_obj$data, convo_df)
    } else {
      cat(paste0("Search API response error code: ", resp$status, ".\n",
                 "conversation_id: ", convo_id, " next_token: -\n"))
    }

    next_token <- resp_obj$meta$next_token

    # if more pages of results
    while (!is.null(next_token)) {
      url <- paste0(search_url, "&next_token=", next_token)
      resp <- httr::GET(url, token)

      if (resp$status == 200) {
        resp_obj <- resp_data(resp)
        if (!is.null(resp_obj$data)) {
          convo_df <- dplyr::bind_rows(convo_df, resp_obj$data)
        }
        next_token <- resp_obj$meta$next_token
      } else {
        cat(paste0("Search API response error code: ", resp$status, ".\n",
                   "conversation_id: ", convo_id, " next_token: ", next_token, "\n"))
      }
    }

    # unnest referenced tweets - move to network creation
    # convo_df <- convo_df %>%
    #   dplyr::mutate(status_id = .data$id, id = NULL)
    # convo_df <- convo_df %>%
    #   tidyr::unnest(cols = c("referenced_tweets"), keep_empty = TRUE) %>%
    #   dplyr::mutate(ref_tweet_id = .data$id,
    #                 ref_tweet_type = .data$type,
    #                 id = NULL, type = NULL)

    class(convo_df) <-
      append(c("datasource", "twitter.convo"), class(convo_df))

    if (writeToFile) {
      write_output_file(convo_df, "rds", "TwitterConvoData")
    }

    cat("Done.\n")

    convo_df
  }

# httr response json text into a dataframe
resp_data <- function(r) {
  if (length(httr::content(r)$error)) {
    err <- sapply(httr::content(r)$error, function(x) cat("*", x$message, "\n"))
    return(NULL)
  }
  json <- httr::content(r, as = "text", encoding = "UTF-8")
  resp <- tryCatch({
    jsonlite::fromJSON(json)
  }, error = function(e) {
    # cat(json, file = "error.txt", append = TRUE, sep = "\n")
    return(NULL)
  })
}
