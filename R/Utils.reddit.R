reddit_tid_from_url <- function(url, desc = FALSE) {
  if (desc) { extract <- "\\2 (\\3)" } else { extract <- "\\3" }
  gsub("^(.*)?/(r/.+)/comments/([0-9A-Za-z]{6})?/.*?(/)?$", extract, url, ignore.case = TRUE, 
       perl = TRUE, useBytes = TRUE)  
}

get_json <- function(req_url, ua = NULL) {
  res <- list(status = NULL, msg = NULL, data = NULL)
  req_headers <- c("Accept-Charset" = "UTF-8",
                   "Cache-Control" = "no-cache")
  
  if (!is.null(ua)) { req_headers <- append(req_headers, c("User-Agent" = ua)) }
  
  resp <- httr::GET(req_url, add_headers(.headers = req_headers))
  res$status <- resp$status
  
  if (httr::http_error(resp) | as.numeric(resp$status) != 200) {
    res$msg <- "URL request error"
    return(res)
  } 
  
  if (httr::http_type(resp) == "application/json") {
    res$data <- tryCatch({
      res$msg <- "URL JSON ok"
      jsonlite::fromJSON(content(resp, as = "text"), simplifyVector = FALSE)
    }, error = function(e) {
      res$msg <- e
      NULL
    })
  } else {
    res$msg <- "URL not JSON"
  }
  
  res
}

CleanRedditText <- function(comments) {

  # json encoding issues should be tackled upstream
  
  # xml 1.0
  # allowed #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
  # [\x00-\x1F] ^\xE000-\xFFFD^\x10000-\x10FFFF
  # [^\x09^\x0A^\x0D^\x20-\xD7FF^\xE000-\xFFFD]
  # [\u0000-\u0008,\u000B,\u000C,\u000E-\u001F]
  
  # decode html encoding as not required
  # df$vosonTxt_comment <- textutils::HTMLdecode(df$vosonTxt_comment)
  
  # take care of a few known encoding issues
  comments <- gsub("([\u0019])", "'", comments, perl = TRUE, useBytes = TRUE)
  comments <- gsub("([\u0023])", "#", comments, perl = TRUE, useBytes = TRUE)
  comments <- gsub("([&#x200B;])", " ", comments, perl = TRUE, useBytes = TRUE)
  
  # replace chars outside of allowed xml 1.0 spec
  comments <- gsub("([\u0001-\u0008\u000B\u000C\u000E-\u001F])", "", comments, perl = TRUE, useBytes = TRUE)
}

FullCleanText <- function(sentences) {
  # remove any characters that are not in punctuation, alphanumeric classes or spaces
  sentences <- gsub("[^[:punct:]^[:alnum:]^\\s^\\n]", "", sentences, perl = TRUE, useBytes = TRUE)
}
