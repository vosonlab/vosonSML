ExtractUserInfo <-
function(df) {
  # For each tweet, extract information related to users
  # such as to_user, rt_user...

  # extract to_user
  df$reply_to <- sapply(df$text, function(tweet)
    TrimHead(str_extract(tweet,"^((\\.)?(@[[:alnum:]_+]*))")))

  # extract any MENTIONS at all (inc. replies, mentions, etc) ### This is a completely new approach - it 'vacuums' up ANY mentions
  df$users_mentioned <- sapply(df$text, function(tweet)
  TrimHead(str_match_all(tweet,"@[[:alnum:]_+]*")[[1]]))

  # extract rt_user
  df$retweet_from <- sapply(df$text, function(tweet)
    TrimHead(str_extract(tweet,"^[RM]T (@[[:alnum:]_+]*)")))

  return(df)
}
