RemoveOddChars <-
function(df) {
  # Remove odd characters in tweets
  df$text <- sapply(df$text, function(x) TrimOddChar(x))
  return(df)
}
