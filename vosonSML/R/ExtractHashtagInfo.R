ExtractHashtagInfo <-
function(df) {
  # For each tweet, extract ANY hashtags that a user has used:

  df$hashtags_used <- sapply(df$text, function(tweet)

  # OLD WAY:
  # TrimHead(str_match_all(tweet,"#[[:alnum:]_+]*")[[1]])

  # NEW WAY:
  # This matches hashtags, but not if the hashtag is "cut off" at the end
  # of the tweet text, denoted by a 'trailing ellipsis' character.
  # This avoids the problem of picking up erroneous hashtags that are cut off,
  # e.g. "#ausp..." when it should be "#auspol"

    # horizontalEllipsis <- "\u2026"
    # horizontalEllipsisFixed <- stri_unescape_unicode(horizontalEllipsis)

    # patternRegex <- paste0("#[^#\\s]+(?!\\\u2026)\\b")
    # TrimHead(str_match_all(tweet,paste0("#[[:alnum:]_+^",horizontalEllipsis,"$]*"))[[1]])

    regmatches(tweet, gregexpr("#[^#\\s]+(?!\u2026)\\b", tweet, perl=T))

  )

  return(df)
}
