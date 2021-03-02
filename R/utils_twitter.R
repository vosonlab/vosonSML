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

# application only authentication
# curl -u "$API_KEY:$API_SECRET_KEY" --data 'grant_type=client_credentials' 'https://api.twitter.com/oauth2/token'
# {"token_type":"bearer","access_token":"AAAAAA .. JcE3F"}
get_bearer_token <- function(consumer_key, consumer_secret) {
  rlang::check_installed("openssl", "for twitter get bearer token")
  saved_enc <- getOption("encoding")
  saved_ua <- getOption("HTTPUserAgent")
  on.exit({
    options(encoding = saved_enc)
    options(HTTPUserAgent = saved_ua)
  }, add = TRUE)
  options(encoding = "UTF-8")
  options(HTTPUserAgent = paste0("vosonSML v.", get_version(), " (R Package)"))

  app_keys <- openssl::base64_encode(paste0(consumer_key, ":", consumer_secret))
  resp <- httr::POST(
    "https://api.twitter.com/oauth2/token",
    httr::add_headers(Authorization = paste0("Basic ", app_keys)),
    body = list(grant_type = "client_credentials")
  )
  httr::stop_for_status(resp, "twitter get bearer token")

  # if successful return bearer token
  resp_content <- httr::content(resp, encoding = "UTF-8")
  resp_content$access_token

  # token_req <- httr::add_headers(Authorization = paste0("Bearer ", resp_content$access_token))
  # structure(token_req, bearer = resp_content, class = c("bearer", "list"))
}
