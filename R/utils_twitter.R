# print current twitter search api rate limit and reset time for token
print_rate_limit <- function(token, endpoint = "search/tweets", out = "message") {
  rl <- ""
  rtlimit <- rtweet::rate_limit(endpoint, token = token)
  remaining <-
    rtlimit[["remaining"]] * 100  # 100 returned tweets per request
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")
  rl <- paste0("remaining search num: ", remaining, "\n")
  rl <- paste0(rl, "reset: ", reset, " secs\n")

  if (out == "cat") {
    cat(rl)
  } else {
    message(rl)
  }
}

# get remaining tweets in current search api rate limit
remaining_num_tweets <- function(token) {
  rtlimit <- rtweet::rate_limit("search/tweets", token = token)
  remaining <-
    rtlimit[["remaining"]] * 100 # 100 tweets returned per request

  remaining
}

# api datetime format in strings
twitter_api_dt_fmt <- function() {
  "%a %b %d %H:%M:%S +0000 %Y"
}

# modify rtweet 1.0 format tweet data
modify_tweet_data <- function(data, users = NULL, rtweet_created_at = FALSE) {

  if (is.null(data)) return(NULL)
  if (is.data.frame(data) && nrow(data) < 1) return(NULL)

  data <- data |>
    dplyr::rename(status_id = .data$id_str) |>
    dplyr::relocate(.data$status_id)

  if (is.null(users)) {
    data <- data |> dplyr::mutate(
      u.user_id = NA_character_, u.screen_name = NA_character_)
  } else {
    data <- data |> dplyr::bind_cols(users)
  }

  # -----

  api_dt_fmt <- twitter_api_dt_fmt()

  mod_data <- data |>
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
    mod_data <- mod_data |>
      dplyr::mutate(
        created_at = ifelse(
          is.na(.data$created_at), NA_character_,
          as.character(as.POSIXct(.data$created_at, format = api_dt_fmt, tz = "UTC"))
        )
      )
  }

  mod_data <- mod_data |>
    dplyr::mutate_at(dplyr::vars(dplyr::contains("created_at")),
                     lubridate::as_datetime, tz = "UTC")

  mod_data
}
