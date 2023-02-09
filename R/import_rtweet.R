#' @title Import rtweet collected data
#'
#' @description Imports \pkg{rtweet} collected data from \code{rda} or \code{rds} saved object file or from an rtweet
#'   dataframe. Ensures \code{datasource} and specified \code{socialmedia} type are set so data is usable by
#'   \code{\link{Create}} functions. Not required if collected data was collected by \code{vosonSML} and saved as an
#'   \code{rds} file, use \code{\link{readRDS}} instead.
#'
#' @note Only supports \pkg{rtweet} data collected using the \code{\link[rtweet]{search_tweets}} or
#'   \code{\link[rtweet]{get_timeline}} functions.
#'
#' @param data Character string or dataframe. File path to or tibble of collected data from \pkg{rtweet}.
#'
#' @return A dataframe suitable for input into twitter network \code{\link{Create}} functions.
#'
#' @examples
#' \dontrun{
#' # import rtweet collected data from dataframe
#' collect_tw <- ImportRtweet(rtweet_search_data)
#'
#' # import rtweet collected data from file
#' collect_tw <- ImportRtweet("./rtweet_search_n100.rds")
#' }
#'
#' @aliases ImportRtweet
#' @name ImportRtweet
#' @export
ImportRtweet <- function(data) {
  if (missing(data)) {
    stop("Please provide file path or dataframe to import.", call. = FALSE)
  }

  if (check_df_n(data) > 0) return(import_rtweet_(data))

  data <- check_chr(data, param = "data")

  if (!file.exists(data)) {
    stop("Import file not found.", call. = FALSE)
  }

  type <- gsub(
    ".*\\.([A-Za-z]{3})$",
    "\\1",
    data,
    ignore.case = TRUE,
    perl = TRUE
  )

  df <- tryCatch({
    switch(
      tolower(type),
      "rda" = load(file = data),
      "rds" = readRDS(file = data)
    )
  }, error = function(e) {
    stop(paste0(
      "Could not read: ",
      data,
      ".\n",
      gsub("^Error:\\s", "", paste0(e))
    ), call. = FALSE)
  })

  import_rtweet_(df)
}

# modify rtweet v1.0 format tweet data
import_rtweet_ <- function(data, rtweet_created_at = FALSE) {

  if (check_df_n(data) < 1) return(NULL)

  # rename tweet id to status_id
  data <- data |>
    dplyr::rename(status_id = .data$id_str) |>
    dplyr::relocate(.data$status_id)

  # extract users
  df_users <- attr(data, "users", exact = TRUE)

  # add author user_id and screen_name to tweets
  if (is.null(df_users) ||
      !is.data.frame(df_users) || nrow(df_users) < 1) {
    data <- data |>
      dplyr::mutate(u.user_id = NA_character_,
                    u.screen_name = NA_character_)
  } else {
    if (nrow(data) != nrow(df_users)) {
      stop(paste0("Number of tweet observations does not match number of users. ",
                  nrow(data), " != ", nrow(df_users)), call. = FALSE)
    }
    data <- data |>
      dplyr::bind_cols(
        df_users |> dplyr::select(
          u.user_id = .data$id_str,
          u.screen_name = .data$screen_name
        )
      )
  }

  api_dt_fmt <- twitter_api_dt_fmt()

  df_tweets <- data |>
    dplyr::mutate_at(dplyr::vars(dplyr::contains(c("_id", "created_at"))), as.character) |>

    # unnest necessary retweet and quote metadata
    dplyr::rename(rts = .data$retweeted_status, qs = .data$quoted_status) |>
    dplyr::mutate_at(dplyr::vars("rts", "qs"), as.list) |>
    tidyr::hoist(
      .col = .data$rts,
      rts.id = "id_str",
      rts.created_at = "created_at",
      rts.user_id = list("user", "id_str"),
      rts.screen_name = list("user", "screen_name"),
      .remove = FALSE
    ) |>
    tidyr::hoist(
      .col = .data$qs,
      qs.id = "id_str",
      qs.created_at = "created_at",
      qs.user_id = list("user", "id_str"),
      qs.screen_name = list("user", "screen_name"),
      .remove = FALSE
    ) |>
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(c("rts.", "qs."))), as.character) |>

    dplyr::mutate(
      user_id = .data$u.user_id,
      screen_name = .data$u.screen_name,

      is_reply = ifelse(!is.na(.data$in_reply_to_status_id_str), TRUE, FALSE),
      reply_to_status_id = .data$in_reply_to_status_id_str,
      reply_to_user_id = .data$in_reply_to_user_id_str,
      reply_to_screen_name = .data$in_reply_to_screen_name,

      is_retweet = ifelse(!is.na(.data$rts.id), TRUE, FALSE),
      retweet_status_id = .data$rts.id,
      retweet_user_id = .data$rts.user_id,
      retweet_screen_name = .data$rts.screen_name,

      is_quote = ifelse(!is.na(.data$qs.id), TRUE, FALSE),
      is_quote_status.orig = .data$is_quote_status,
      quoted_status_id.orig = .data$quoted_status_id,
      quoted_status_id = .data$qs.id,
      quoted_user_id = .data$qs.user_id,
      quoted_screen_name = .data$qs.screen_name,

      retweet_created_at = ifelse(
        is.na(.data$rts.created_at), NA_character_,
        as.character(as.POSIXct(.data$rts.created_at, format = api_dt_fmt, tz = "UTC"))
      ),
      quoted_created_at = ifelse(
        is.na(.data$qs.created_at), NA_character_,
        as.character(as.POSIXct(.data$qs.created_at, format = api_dt_fmt, tz = "UTC"))
      )
    ) |>
    dplyr::select(-dplyr::starts_with(c("rts.", "qs."))) |>

    # clean up
    dplyr::select(-.data$id,
                  -dplyr::starts_with(c("in_reply_to_", "u."))) |>
    dplyr::relocate(.data$is_reply, .after = .data$status_id) |>
    dplyr::relocate(.data$is_quote, .after = .data$is_reply) |>
    dplyr::relocate(.data$is_retweet, .after = .data$is_quote)

  if (rtweet_created_at == FALSE) {
    df_tweets <- df_tweets |>
      dplyr::mutate(
        created_at = ifelse(
          is.na(.data$created_at), NA_character_,
          as.character(as.POSIXct(.data$created_at, format = api_dt_fmt, tz = "UTC"))
        )
      )
  }

  df_tweets <- df_tweets |>
    dplyr::mutate_at(dplyr::vars(dplyr::contains("created_at")),
                     lubridate::as_datetime, tz = "UTC")

  df_tweets <- df_tweets |> twitter_fix_col_types()
  
  data <- list(tweets = df_tweets, users = df_users)
  class(data) <- append(c("datasource", "twitter"), class(data))

  data
}
