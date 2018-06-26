TrimHashtags <-
function(x) {
  # remove hashtags, i.e. "#tag", in a tweet

  str_replace_all(x, '(#[[:alnum:]_]*)', '')
}
