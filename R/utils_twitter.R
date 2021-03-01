# print current twitter search api rate limit and reset time for token
print_rate_limit <- function(token) {
  rtlimit <- rtweet::rate_limit(token, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100  # 100 returned tweets per request
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")
  cat(paste0("remaining search num: ", remaining, "\n"))
  cat(paste0("reset: ", reset, " secs\n"))
}

# get remaining tweets in current search api rate limit
remaining_num_tweets <- function(token) {
  rtlimit <- rtweet::rate_limit(token, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100 # 100 tweets returned per request
}
