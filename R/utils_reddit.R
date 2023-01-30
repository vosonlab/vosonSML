# get the thread id from a reddit thread url
get_thread_id <- function(url, desc = FALSE) {
  if (desc) {
    extract <- "\\2 (\\3)"
  } else {
    extract <- "\\3"
  }
  gsub(
    "^(.*)?/(r/.+)/comments/([0-9A-Za-z]{2,})?/.*?(/)?$",
    extract,
    url,
    ignore.case = TRUE,
    perl = TRUE,
    useBytes = TRUE
  )
}

# get request json from url address
get_json <- function(req_url, ua = NULL) {
  res <- list(status = NULL,
              msg = NULL,
              data = NULL)
  req_headers <- c("Accept-Charset" = "UTF-8",
                   "Cache-Control" = "no-cache")

  if (!is.null(ua)) {
    req_headers <- append(req_headers, c("User-Agent" = ua))
  }

  resp <-
    httr::GET(req_url, httr::add_headers(.headers = req_headers))
  res$status <- resp$status

  if (httr::http_error(resp) || as.numeric(resp$status) != 200) {
    res$msg <- "http request error"
    return(res)
  }

  if (httr::http_type(resp) == "application/json") {
    res$data <- tryCatch({
      res$msg <- "http response json"
      jsonlite::fromJSON(httr::content(resp, as = "text"), simplifyVector = FALSE)
    }, error = function(e) {
      res$msg <- e
      NULL
    })
  } else {
    res$msg <- "http response not json"
  }

  res
}

# remove known reddit problem characters for xml
xml_clean_reddit <- function(comments) {
  # json encoding issues should be tackled upstream
  # xml 1.0
  # allowed #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
  # [\x00-\x1F] ^\xE000-\xFFFD^\x10000-\x10FFFF
  # [^\x09^\x0A^\x0D^\x20-\xD7FF^\xE000-\xFFFD]
  # [\u0000-\u0008,\u000B,\u000C,\u000E-\u001F]

  # take care of a few known encoding issues
  comments <-
    gsub("([\u0019])",
         "'",
         comments,
         perl = TRUE,
         useBytes = TRUE)
  comments <-
    gsub("([\u0023])",
         "#",
         comments,
         perl = TRUE,
         useBytes = TRUE)
  comments <-
    gsub("([&#x200B;])",
         " ",
         comments,
         perl = TRUE,
         useBytes = TRUE)

  # replace chars outside of allowed xml 1.0 spec
  comments <-
    gsub(
      "([\u0001-\u0008\u000B\u000C\u000E-\u001F])",
      "",
      comments,
      perl = TRUE,
      useBytes = TRUE
    )
}

# remove all characters outside of standard types
clean_full <- function(sentences) {
  # remove not in punctuation, alphanumeric classes or spaces
  sentences <-
    gsub(
      "[^[:punct:]^[:alnum:]^\\s^\\n]",
      "",
      sentences,
      perl = TRUE,
      useBytes = TRUE
    )
}
