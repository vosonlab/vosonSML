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
