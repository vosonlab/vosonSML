# remove odd characters in tweets
RemoveOddChars <- function(df) {
  df$text <- sapply(df$text, function(x) TrimOddChar(x))
  
  return(df)
}

# extract information related to users from dataframe such as to_user, rt_user etc.
ExtractUserInfo <- function(df) {
  
  # extract to_user
  df$reply_to <- sapply(df$text, function(tweet) TrimHead(str_extract(tweet, "^((\\.)?(@[[:alnum:]_+]*))")))
  
  # extract any mentions at all (inc. replies, mentions, etc) 
  # this is a completely new approach - it 'vacuums' up ANY mentions
  df$users_mentioned <- sapply(df$text, function(tweet) TrimHead(str_match_all(tweet, "@[[:alnum:]_+]*")[[1]]))
  
  # extract rt_user
  df$retweet_from <- sapply(df$text, function(tweet) TrimHead(str_extract(tweet, "^[RM]T (@[[:alnum:]_+]*)")))
  
  return(df)
}

# extract any hashtags found in tweet text
ExtractHashtagInfo <- function(df) {
  df$hashtags_used <- sapply(df$text, function(tweet) regmatches(tweet, gregexpr("#[^#\\s]+(?!\u2026)\\b", 
                                                                                 tweet, perl = T)))
  return(df)
}

# for each tweet, extract url, remove it from the tweet, and put them separately in a new column
# todo: cannot deal with multiple urls in one tweet right now
ExtractUrls <- function(df) {
  # extracts links (quick and dirty)
  # wish to have something like http://daringfireball.net/2009/11/liberal_regex_for_matching_urls
  df$links <- sapply(df$text,function(tweet) str_extract(tweet,("http[^[:blank:]]+")))
  df$text <- sapply(df$text, function(x) TrimUrls(x))
  
  return(df)
}

# remove odd characters in the user information attributes
# odd characters are especially problematic for search queries that trawl non-english speaking users/collectives
RemoveOddCharsUserInfo <- function(actorsInfoDF) {
  
  actorsInfoDF$screenName <- sapply(actorsInfoDF$screenName, function(x) TrimOddChar(x))
  actorsInfoDF$description <- sapply(actorsInfoDF$description, function(x) TrimOddChar(x))
  actorsInfoDF$url <- sapply(actorsInfoDF$url, function(x) TrimOddChar(x))
  actorsInfoDF$name <- sapply(actorsInfoDF$name, function(x) TrimOddChar(x))
  actorsInfoDF$location <- sapply(actorsInfoDF$location, function(x) TrimOddChar(x))
  actorsInfoDF$lang <- sapply(actorsInfoDF$lang, function(x) TrimOddChar(x))
  actorsInfoDF$profileImageUrl <- sapply(actorsInfoDF$profileImageUrl, function(x) TrimOddChar(x))
  
  return(actorsInfoDF)
}

# remove twitter users from text e.g @user
TrimUsers <- function(x) {
  str_replace_all(x, "(@[[:alnum:]_]*)", "")
}

# remove urls from text
TrimUrls <- function(x) {
  str_replace_all(x, "http[^[:blank:]]+", "")
}

# remove odd charactors
TrimOddChar <- function(x) {
  iconv(x, to = "utf-8")
}

# remove odd charactors on macos
TrimOddCharMac <- function(x) {
  iconv(x, to = "utf-8-mac")
}

# remove leading @ part from tweet text e.g ".@", "RT @", "MT @" etc.
TrimHead <- function(x) {
  sub("^(.*)?@", "", x)
}

# remove hashtags from text e.g #tag
TrimHashtags <- function(x) {
  str_replace_all(x, "(#[[:alnum:]_]*)", "")
}

# remove @ from text
TrimAt <- function(x) {
  sub("@", "", x)
}

# batch perform tweet preprocessing tasks
PreprocessTweets <- function(df) {
  # removing odd characters
  df_new <- RemoveOddChars(df)
  
  # extract user info and add to df
  df_new <- ExtractUserInfo(df_new)
  
  # extract urls and add to df
  df_new <- ExtractUrls(df_new)
  
  return(df_new)
}

# accepts a df to add or increment a field value with count
# if param print is TRUE then prints formatted field values
networkStats <- function(df, field, count, edge = FALSE, print = FALSE) {
  if (print == TRUE) {
    if (!is.null(df) & nrow(df) > 0) {
      lf <- lc <- 0
      for (i in 1:nrow(df)) {
        lf <- ifelse(nchar(df$field[i]) > lf, nchar(df$field[i]), lf)
        lc <- ifelse(nchar(df$count[i]) > lc, nchar(df$count[i]), lc)
      }
      
      for (i in 1:nrow(df)) {
        lfm <- lf
        if (nchar(df$field[i]) != lf) {
          lfm <-lf + 1
        }
        line <- paste0(df$field[i], paste0(replicate(lfm - nchar(df$field[i]), ""), collapse = " "), " | ")
        line <- paste0(line, df$count[i], paste0(replicate(lc - nchar(df$count[i]), ""), collapse = " "), "\n")
        cat(line)
      }
    }
    
    return(TRUE)
  }
  
  if (is.null(df)) {
    df  <- data.frame("field" = character(0), "count" = character(0), "edge_count" = character(0), 
                      stringsAsFactors = FALSE)
  }
  df <- rbind(df, list(field = field, count = count, edge_count = edge), stringsAsFactors = FALSE)
  
  return(df)
}

# print twitter api rate limit and reset time for tweet search
printTwitterRateLimit <- function(token) {
  rtlimit <- rtweet::rate_limit(token, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100
  reset <- rtlimit[["reset"]]
  reset <- as.numeric(reset, "secs")
  cat(paste0("remaining search num / 15 mins: ", remaining, "\n"))
  cat(paste0("reset: ", reset, " secs\n"))
}

# get remaining twitter api rate limit for tweet search
getRemainingSearchNum <- function(token) {
  rtlimit <- rtweet::rate_limit(token, "search/tweets")
  remaining <- rtlimit[["remaining"]] * 100
}
