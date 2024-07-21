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

# build a reddit comment thread url for json
create_thread_url <- function(url, sort = NA) {
  # trailing slash
  if (!grepl("/$", url)) url <- paste0(url, "/")
  
  # format /r/xxxxx/comments/xxxxxxx/xxx_x_xxxxxx/
  if (grepl("^/r/.+?/comments/.+?/.+?/$", url, ignore.case = TRUE)) {
    url <- paste0("https://www.reddit.com", url)
  }
  
  if (!grepl("^https?://(.*)", url)) url <- paste0("https://www.", gsub("^.*(reddit\\..*$)", "\\1", url))
  
  if (!is.na(sort)) {
    if (sort == "best") sort <- "confidence"
    sort <- paste0("sort=", sort, "&")
  } else {
    sort <- ""
  }
  
  # message(paste0(url, ".json?", sort, "&limit=500&raw_json=1"))
  
  paste0(url, ".json?", sort, "&limit=500&raw_json=1")
}

# build a subreddit thread listing url for json
create_listing_url <- function(subreddit, sort, period, qs = NULL) {
  if (!is.null(period) & sort == "top") qs <- c(paste0("t=", period), qs)
  if (!is.null(qs)) qs <- paste0("?", paste0(qs, collapse = "&"))
  
  paste0("https://www.reddit.com/r/", trimws(subreddit), "/", sort, "/.json", qs)
}

# get request for json with url
get_json <- function(req_url, ua = NULL, alt = FALSE) {
  res <- list(status = NULL, msg = NULL, data = NULL)
  
  req_headers <- c(
    "Accept-Charset" = "UTF-8",
    "Cache-Control" = "no-cache",
    "Accept" = "application/json, text/*, */*"
  )
  
  if (!is.null(ua)) req_headers <- append(req_headers, c("User-Agent" = ua))
  
  url_conn <- tryCatch({
    base::url(req_url, headers = req_headers)
  }, error = function(e) {
    list(url_conn_error = e)
  })
  
  if (inherits(url_conn, "list") && !is.null(url_conn$url_conn_error)) {
    return(list(status = -1, msg = url_conn$url_conn_error, data = NULL))
  }
  
  read_data <- tryCatch({
    if (!alt) {
      jsonlite::fromJSON(url_conn, simplifyVector = FALSE)
    } else {
      jsonlite::fromJSON(url_conn)
    }
  }, error = function(e) {
    list(read_data_error = e)
  })
  
  if (inherits(read_data, "list") && !is.null(read_data$read_data_error)) {
    return(list(status = -1, msg = read_data$read_data_error, data = NULL))
  }
  
  res$status <- 1
  res$data <- read_data
  
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

# build a wait time range in seconds for reddit data requests
# reddit rate-limit is 10 requests per minute
check_wait_range_secs <- function(x, param = "value", def_min = 6, def_max = 10) {
  fail_msg <- paste0("Please provide a numeric range as vector c(min, max) for ", param, ". Min must be >= 6 secs.")
  
  if (!is.numeric(x)) stop(fail_msg, call. = FALSE)

  x <- ceiling(x)
  
  if (min(x) < 6) stop(fail_msg, call. = FALSE)
  
  if (length(x) == 1) x <- c(def_min, x[1])
  if (length(x) != 2) x <- c(x[1], x[2])
  
  if (x[1] < def_min) x[1] <- def_min
  if (x[1] >= x[2]) x[2] <- x[1] + 1
  
  x[1]:x[2]
}
