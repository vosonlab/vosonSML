#' @title Collect hyperlinks from web pages
#'
#' @description Collects hyperlinks from web pages and structures the data into a dataframe with the
#' class names \code{"datasource"} and \code{"web"}.
#' 
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"reddit"}.
#' @param pages List. List of web pages to crawl.
#' @param waitTime Numeric vector. Time range in seconds to select random wait from in-between url collection requests.
#' Minimum is 3 seconds. Default is \code{c(3, 10)} for a wait time chosen from between 3 and 10 seconds.
#' @param ua Character string. Override User-Agent string to use in web page requests. Default is
#' \code{option("HTTPUserAgent")} value as set by vosonSML.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return A \code{data.frame} object with class names \code{"datasource"} and \code{"reddit"}.
#' 
#' @examples
#' \dontrun{
#' webData <- webAuth %>%
#'   Collect(pages = list(), writeToFile = TRUE)
#' }
#' 
#' @export
Collect.web <- function(credential, pages, waitTime = c(3, 10), ua = getOption("HTTPUserAgent"),
                           writeToFile = FALSE, verbose = TRUE, ...) {
  
  cat("Collecting web page hyperlinks...\n")
  
  results <- crawl(pages)
  df_results <- map_dfr(results, bind_rows)
  
  class(df_results) <- append(c("datasource", "web"), class(df_results))
  if (writeToFile) { writeOutputFile(df_results, "rds", "WebData") }
  
  cat("Done.\n")
  
  df_results
}

get_delay <- function(crawl_delay = NULL) {
  max_sleep <- 3
  
  if (is.null(crawl_delay)) { return(sample(1:max_sleep, 1)) }
  
  get_value <- function(crawl_delay, ua) {
    v <- filter(crawl_delay, useragent == ua)
    if (nrow(v)) { return(v$value) }
    NULL
  }
  
  if (nrow(crawl_delay)) {
    delay <- get_value(crawl_delay, getOption("HTTPUserAgent"))
    if (!is.null(delay)) { return(delay) }
    
    delay <- get_value(crawl_delay, "*")
    if (!is.null(delay)) { return(delay) }
    
    return(sample(1:max_sleep, 1))
  } else {
    return(sample(1:max_sleep, 1))
  }
}

get_domain_robots <- function(url) {
  suppressMessages({
    r <- tryCatch({
      robotstxt(domain = url)
    }, error = function(e) {
      cat(paste0("- robots error: ", url, "\n"))
      NULL
    })
  })
  r
}

get_page_hrefs <- function(page) {
  # join url parts and prevent double slashes 
  local_to_full <- function(x, y) {
    x <- gsub("/$", "", x)
    u <- urltools::url_parse(x)
    
    if (is.null(y) || is.na(y)) {
      return(x)
    }
    
    if (trimws(y) == "") {
      return(x)
    }
    
    if (grepl("^#$", trimws(y))) {
      return(x)
    }
    
    if (grepl("^/", y)) {
      return(paste0(u$scheme, "://", u$domain, y))
    } else {
      return(paste0(x, "/", y)) 
    }
  }
  
  if (grepl(".*\\.pdf$", tolower(page))) {
    return(list())
  }
  
  urls <- tryCatch({
    u <- read_html(page) %>%
      html_nodes("a") %>% 
      html_attr("href")
    
    # page_domain <- domain(page)
    
    u <- url_decode(u)
    u <- u[!grepl("^mailto:.+", u, ignore.case = TRUE)]
    
    # if an internal link prepend page url to link
    u <- map_if(u,
                ~{!grepl("^(http|https)://.+", .x, ignore.case = TRUE)},
                ~{local_to_full(page, .x)})
    # u <- gsub("/$", "", u)
    u <- str_replace(u, "/$", "")
    u
  }, error = function(e) {
    cat(paste0("- error: ", page, " (", trimws(e), ")", "\n"))
    list("error", trimws(e))
  })
  
  urls
}

get_hlinks <- function(url, depth, max_depth, type) {
  robots <- list()
  got_urls <- list()
  
  # single page request that returns a df of urls
  go_get <- function(url) {
    df <- NULL
    
    u <- urltools::url_parse(url)
    d <- u$domain
    
    #browser()
    
    if (!d %in% names(robots)) {
      r <- get_domain_robots(paste0(u$scheme, "://", d))
      if (!is.null(r)) {
        cat("* new domain:", paste0(u$scheme, "://", d), "\n")
        robots[[d]] <<- r
      } else {
        cat("* no robots or error:", paste0(u$scheme, "://", d), "\n")
      }
    }
    
    b_url <- url
    # if (!is.na(u$fragment)) {
    #   b_url <- gsub(paste0("#", u$fragment, "$"), "", url)
    # }
    
    if (!b_url %in% got_urls) {
      r <- robots[[d]]
      
      if (is.null(r)) {
        delay <- get_delay(NULL)
        cat(paste0("+ ", b_url, " (", delay, " secs)\n"))
      } else if (r$check(u$path)) {
        delay <- get_delay(r$crawl_delay)
        cat(paste0("+ ", b_url, " (", delay, " secs)\n"))
      } else {
        cat("- disallowed:", url, "\n")
        return(df)
      }
      
      if (delay == 10) {
        delay <- 1
      }
      Sys.sleep(delay)
      
      hrefs <- get_page_hrefs(url)
      
      is_err <- FALSE
      if (length(hrefs) == 2) {
        if (hrefs[1] == "error") {
          is_err <- TRUE
          df <- tibble(url = as.character(url), n = 1, page_err = as.character(hrefs[2]))
        }
      }
      
      got_urls <<- append(got_urls, b_url)
      
      if (!is_err) {
        if (length(hrefs) > 0) {
          df <- tibble(url = as.character(hrefs)) %>% count(url)
          df$page_err <- NA
        }       
      }
    } else {
      cat("- already done:", url, "\n")
    }
    
    uu <- url
    uu <- str_replace(uu, "/$", "")
    if (!is.null(df)) { 
      df <- df %>% mutate(page = uu,
                          depth = depth,
                          max_depth = max_depth,
                          parse = urltools::url_parse(.data$url))
      
      # remove fragments
      df <- df %>% mutate(url = ifelse(!is.na(.data$parse$fragment),
                                       str_replace(.data$url, paste0("#", .data$parse$fragment, "$"), ""),
                                       #gsub(paste0("#", .data$parse$fragment, "$"), "", .data$url),
                                       .data$url))
    }
    
    df
  }
  
  # initial call and while loop for max depth
  cat(paste0("*** initial call to get urls - ", url, "\n"))
  url <- str_replace(url, "/$", "")
  df_total <- map_dfr(url, go_get)
  df_total$seed <- url
  df_total$type <- type
  
  init_url_dets <- urltools::url_parse(url)
  if (type == "int") {
    urls <- dplyr::filter(df_total, .data$parse$domain == init_url_dets$domain)
    urls <- urls$url
  } else if (type == "ext") {
    urls <- dplyr::filter(df_total, .data$parse$domain != init_url_dets$domain)
    urls <- urls$url    
  } else {
    urls <- df_total$url
  }
  
  urls <- na.omit(urls)
  urls <- str_replace(urls, "/$", "")
  
  cat(paste0("*** end initial call", "\n"))
  # sort sites into internal and external
  
  while (length(urls) > 0 & depth < max_depth) {
    cat(paste0("*** set depth: ", (depth + 1), "\n"))
    depth <- depth + 1
    
    cat(paste0("*** loop call to get urls - nrow: ", length(urls), " depth: ", depth, " max_depth: ", max_depth, "\n"))
    df <- map_dfr(urls, go_get)
    df$seed <- url
    df$type <- type
    df_total <- bind_rows(df_total, df)
    
    if (type == "int") {
      urls <- dplyr::filter(df, .data$parse$domain == init_url_dets$domain)
      urls <- urls$url
    } else if (type == "ext") {
      urls <- dplyr::filter(df, .data$parse$domain != init_url_dets$domain)
      urls <- urls$url
    } else {
      urls <- df$url
    }
    urls <- na.omit(urls)
    urls <- str_replace(urls, "/$", "")
    
    # cat(paste0("*** set depth: ", (depth + 1), "\n"))
    # depth <- depth + 1
  }
  
  df_total
}

crawl <- function(pages) {
  robots_opts <- getOption("robotstxt_warn")
  on.exit({ options(robotstxt_warn = robots_opts) }, add = TRUE)
  options(robotstxt_warn = FALSE)
  
  results <- list()
  
  for (i in 1:nrow(pages)) {
    row <- slice(pages, i)
    results[[row$page]] <- get_hlinks(row$page, 1, row$max_depth, row$type)
  }
  
  results
}
