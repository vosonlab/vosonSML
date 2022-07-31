#' @title Collect hyperlinks from web pages
#'
#' @description Collects hyperlinks from web pages and structures the data into a dataframe with the class names
#'   \code{"datasource"} and \code{"web"}.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"web"}.
#' @param pages Dataframe. Dataframe of web pages to crawl. The dataframe must have the columns \code{page} (character),
#'   \code{type} (character) and \code{max_depth} (integer). Each row is a seed web page to crawl, with the \code{page}
#'   value being the page URL. The \code{type} value is type of crawl as either \code{"int"}, \code{"ext"} or
#'   \code{"all"}, directing the crawler to follow only internal links, follow only external links (different domain to
#'   the seed page) or follow all links. The \code{max_depth} value determines how many levels of hyperlinks to follow
#'   from the seed site.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{data.frame} object with class names \code{"datasource"} and \code{"web"}.
#'
#' @examples
#' \dontrun{
#' pages <- data.frame(page = c("http://vosonlab.net",
#'                              "https://rsss.cass.anu.edu.au"),
#'                     type = c("int", "all"),
#'                     max_depth = c(2, 2))
#'
#' webData <- webAuth |>
#'   Collect(pages, writeToFile = TRUE)
#' }
#'
#' @export
Collect.web <-
  function(credential,
           pages = NULL,
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {

    prompt_and_stop(c("robotstxt", "rvest", "urltools", "xml2"), "Collect.web")

    msg("Collecting web page hyperlinks...\n")

    robots_opts <- getOption("robotstxt_warn")
    on.exit({
      options(robotstxt_warn = robots_opts)
    }, add = TRUE)
    options(robotstxt_warn = FALSE)

    df_results <- list()

    for (i in 1:nrow(pages)) {
      seed <- dplyr::slice(pages, i)
      df_results[[seed$page]] <-
        get_hyperlinks(seed$page,
                       1,
                       seed$max_depth,
                       seed$type,
                       seed$delay,
                       verbose = verbose)
    }

    df_results <- purrr::map_dfr(df_results, dplyr::bind_rows)

    class(df_results) <-
      append(c("datasource", "web"), class(df_results))
    if (writeToFile) {
      write_output_file(df_results, "rds", "WebData", verbose = verbose)
    }

    msg("Done.\n")

    df_results
  }

get_page_hrefs <- function(page, verbose = FALSE) {
  # ignore pdf files
  if (grepl(".*\\.pdf$", tolower(page))) {
    return(list())
  }

  urls <- tryCatch({
    hrefs <- xml2::read_html(page, options = c("NOWARNING")) |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    hrefs <- urltools::url_decode(hrefs)

    # if an internal link prepend page url to link to create full url
    hrefs <- purrr::map_if(hrefs,
                           ~ {
                             !grepl("^(http|https)://.+", .x, ignore.case = TRUE)
                           },
                           ~ {
                             local_to_full_url(page, .x)
                           })
    hrefs <- stringr::str_replace(hrefs, "/$", "")
    hrefs
  }, error = function(e) {
    msg(paste0("- error: ", page, " (", trimws(e), ")", "\n"))
    list("error", trimws(e))
  })

  urls
}

get_hyperlinks <-
  function(url,
           depth,
           max_depth,
           type,
           delay,
           verbose = FALSE) {
    robotstxt_list <- list() # keep a named list of robots.txt by domain
    visited_urls <- list() # keep a list of visited page urls

    # single page request that returns a df of urls
    process_page <- function(page_url, use_delay, verbose = FALSE) {
      df <- NULL

      if (!grepl("^(https|http)://.*$", page_url, ignore.case = TRUE)) {
        msg(paste0("- skipping uri:", page_url, "\n"))
        return(df)
      }

      url_obj <- urltools::url_parse(page_url)
      page_domain <- url_obj$domain

      if (!page_domain %in% names(robotstxt_list)) {
        base_url <- paste0(url_obj$scheme, "://", page_domain)
        robotstxt_obj <- get_domain_robots(base_url)

        if (!is.null(robotstxt_obj)) {
          msg(paste0("* new domain:", base_url, "\n"))
          robotstxt_list[[page_domain]] <<- robotstxt_obj
        } else {
          msg(paste0("* no robots or error:", base_url, "\n"))
        }
      }

      if (!page_url %in% visited_urls) {
        robotstxt_obj <- robotstxt_list[[page_domain]]

        # no robots.txt
        if (is.null(robotstxt_obj)) {
          delay <- get_crawl_delay(NULL, use_delay)
          msg(paste0("+ ", page_url, " (", round(delay, 2), " secs)\n"))

          # check if path allowed and get crawl delay
        } else if (robotstxt_obj$check(url_obj$path)) {
          delay <- get_crawl_delay(robotstxt_obj$crawl_delay, use_delay)
          msg(paste0("+ ", page_url, " (", round(delay, 2), " secs)\n"))

          # path disallowed
        } else {
          msg(paste0("- disallowed:", page_url, "\n"))
          return(df)
        }

        Sys.sleep(delay)

        hrefs <- get_page_hrefs(page_url, verbose)

        is_err <- FALSE
        if (length(hrefs) == 2) {
          if (hrefs[1] == "error") {
            is_err <- TRUE
            df <-
              tibble::tibble(
                url = as.character(page_url),
                n = 1,
                page_err = as.character(hrefs[2])
              )
          }
        }

        visited_urls <<- append(visited_urls, page_url)

        if (!is_err) {
          if (length(hrefs) > 0) {
            df <-
              tibble::tibble(url = as.character(hrefs)) |> dplyr::count(.data$url)
            df$page_err <- NA
          }
        }
      } else {
        msg(paste0("- already done:", page_url, "\n"))
      }

      if (!is.null(df)) {
        uu <- stringr::str_replace(page_url, "/$", "")
        df <- df |> dplyr::mutate(
          page = uu,
          depth = depth,
          max_depth = max_depth,
          parse = urltools::url_parse(.data$url)
        )

        # remove fragments or anchors
        df <- df |> dplyr::mutate(url = ifelse(
          !is.na(.data$parse$fragment),
          stringr::str_replace(.data$url, paste0("#", .data$parse$fragment, "$"), ""),
          .data$url
        ))
      }

      df
    } # end process_page

    # initial call and while loop for max depth
    msg(paste0("*** initial call to get urls - ", url, "\n"))

    url <- stringr::str_replace(url, "/$", "")
    df_total <- purrr::map_dfr(url, process_page, delay, verbose = verbose)
    df_total$seed <- url
    df_total$type <- type

    init_url_dets <- urltools::url_parse(url)
    if (type == "int") {
      urls <-
        dplyr::filter(df_total, .data$parse$domain == init_url_dets$domain)
      urls <- urls$url
    } else if (type == "ext") {
      urls <-
        dplyr::filter(df_total, .data$parse$domain != init_url_dets$domain)
      urls <- urls$url
    } else {
      urls <- df_total$url
    }

    urls <- na.omit(urls)
    urls <- stringr::str_replace(urls, "/$", "")

    msg(paste0("*** end initial call", "\n"))

    # sort sites into internal and external
    while (length(urls) > 0 && depth < max_depth) {
      msg(paste0("*** set depth: ", (depth + 1), "\n"))
      depth <- depth + 1

      msg(
        paste0(
          "*** loop call to get urls - nrow: ",
          length(urls),
          " depth: ",
          depth,
          " max_depth: ",
          max_depth,
          "\n"
        )
      )

      df <- purrr::map_dfr(urls, process_page, delay, verbose)
      df$seed <- url
      df$type <- type
      df_total <- dplyr::bind_rows(df_total, df)

      if (type == "int") {
        urls <-
          dplyr::filter(df, .data$parse$domain == init_url_dets$domain)
        urls <- urls$url
      } else if (type == "ext") {
        urls <-
          dplyr::filter(df, .data$parse$domain != init_url_dets$domain)
        urls <- urls$url
      } else {
        urls <- df$url
      }
      urls <- na.omit(urls)
      urls <- stringr::str_replace(urls, "/$", "")
    }

    df_total
  }
