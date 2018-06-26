TrimUrls <-
function(x) {
  # remove urls in a tweet

  str_replace_all(x, 'http[^[:blank:]]+', '')
}
