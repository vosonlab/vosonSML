PreprocessTweets <-
function(df) {
  # Perform a few preprocessing tasks

  # removing odd characters
  df.new <- RemoveOddChars(df)
  # extract user info and add to df
  df.new <- ExtractUserInfo(df.new)
  # extract urls and add to df
  df.new <- ExtractUrls(df.new)

  return(df.new)
}
