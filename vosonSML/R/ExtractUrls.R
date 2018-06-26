ExtractUrls <-
function(df) {
  # For each tweet, extract url, remove it from the tweet,
  # and put them separately in a new column
  # TODO: cannot deal with multiple urls in one tweet right now

  # EnsurePackage("stringr")
  # EnsurePackage("grid")

  # extracts links (quick and dirty)
  # wish to have something like http://daringfireball.net/2009/11/liberal_regex_for_matching_urls
  df$links <- sapply(df$text,function(tweet) str_extract(tweet,("http[^[:blank:]]+")))
  df$text <- sapply(df$text, function(x) TrimUrls(x))

  return(df)
}
