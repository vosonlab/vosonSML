TrimUsers <-
function(x) {
  # remove users, i.e. "@user", in a tweet

  str_replace_all(x, '(@[[:alnum:]_]*)', '')
}
