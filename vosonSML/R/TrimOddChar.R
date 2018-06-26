TrimOddChar <-
function(x) {
  # remove odd charactors
  iconv(x, to = 'utf-8')
}

TrimOddCharMac <-
function(x) {
  # remove odd charactors
  iconv(x, to = 'utf-8-mac')
}
