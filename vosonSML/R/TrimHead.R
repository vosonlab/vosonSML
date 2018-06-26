TrimHead <-
function(x) {
  # remove starting @, .@, RT @, MT @, etc.

  sub('^(.*)?@', '', x)
}
