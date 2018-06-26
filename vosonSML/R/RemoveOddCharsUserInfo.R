RemoveOddCharsUserInfo <-
function(actorsInfoDF) {
  # Remove odd characters in the user information attributes
  # Odd characters is especially problematic for search queries that trawl non-English speaking users/collectives.
  actorsInfoDF$screenName <- sapply(actorsInfoDF$screenName, function(x) TrimOddChar(x))
  actorsInfoDF$description <- sapply(actorsInfoDF$description, function(x) TrimOddChar(x))
  actorsInfoDF$url <- sapply(actorsInfoDF$url, function(x) TrimOddChar(x))
  actorsInfoDF$name <- sapply(actorsInfoDF$name, function(x) TrimOddChar(x))
  actorsInfoDF$location <- sapply(actorsInfoDF$location, function(x) TrimOddChar(x))
  actorsInfoDF$lang <- sapply(actorsInfoDF$lang, function(x) TrimOddChar(x))
  actorsInfoDF$profileImageUrl <- sapply(actorsInfoDF$profileImageUrl, function(x) TrimOddChar(x))
  return(actorsInfoDF)
}
