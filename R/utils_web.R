# get site robots.txt file
get_domain_robots <- function(url, verbose = TRUE) {
  suppressMessages(suppressWarnings({
    r <- tryCatch({
      robotstxt::robotstxt(domain = url)
    }, error = function(e) {
      if (verbose) {
        cat(paste0("get_domain_robots error: ", url, "\n", e, "\n"))
      }
      NULL
    })
  }))
  r
}

# get site crawl delay from robotstxt crawl_delay dataframe
get_crawl_delay <- function(crawl_delay = NULL,
                            use_delay = NULL) {
  rand_fast_delay <- runif(1, 0.5, 1.0)
  
  if (is.null(crawl_delay) && is.null(use_delay)) {
    return(rand_fast_delay)
  }
  
  if (!is.null(use_delay)) {
    if (is.numeric(use_delay) && use_delay > 0) {
      return(use_delay)
    }
  }
  
  get_ua_delay_value <- function(delay_df, ua) {
    if (ua == "*") {
      v <-
        dplyr::filter(delay_df, trimws(.data$useragent) == "*") # find entry for ua string
    } else {
      v <-
        dplyr::filter(delay_df, stringr::str_detect(tolower(.data$useragent), tolower(trimws(ua))))
    }
    v <- dplyr::distinct(v)
    if (nrow(v)) {
      return(as.numeric(v$value))
    }
    NULL
  }
  
  if (inherits(crawl_delay, "data.frame") && nrow(crawl_delay)) {
    for (ua in c(getOption("HTTPUserAgent"), "*")) {
      delay <- get_ua_delay_value(crawl_delay, ua)
      if (!is.null(delay) && is.numeric(delay)) {
        return(delay)
      }
    }
  }
  
  rand_fast_delay
}

# convert local links to full urls
# join url parts if required and prevent double slashes
local_to_full_url <- function(parent_page, link) {
  if (grepl("^(mailto|ldap|news|tel|telnet|urn|xsl):.*$",
            link,
            ignore.case = TRUE)) {
    return(link)
  }
  
  parent_page <- gsub("/$", "", parent_page) # remove trailing slash
  
  if (is.null(link) || is.na(link)) {
    return(parent_page)
  }
  if (trimws(link) == "") {
    return(parent_page)
  }
  if (grepl("^#$", trimws(link))) {
    return(parent_page)
  }
  
  # relative to root
  if (grepl("^/", link)) {
    u <- urltools::url_parse(parent_page) # parse page url
    return(paste0(u$scheme, "://", u$domain, link))
  } else {
    return(paste0(parent_page, "/", link))
  }
}

# read a web page and get a list of hyperlinks
get_ahrefs <- function(page) {
  hrefs <- xml2::read_html(page, options = c("NOWARNING")) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
}
