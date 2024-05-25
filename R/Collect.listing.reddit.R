#' @title Collect reddit thread listings from subreddits
#'
#' @description Collects thread listings for one or more specified subreddits and structures the data into a dataframe.
#'
#' @note The reddit endpoint used for collection has maximum limit of 25 per listing.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"reddit"}.
#' @param endpoint API endpoint.
#' @param subreddits Character vector. Subreddit names to collect thread listings from.
#' @param sort Character vector. Listing thread sort order. Options are \code{"hot"}, \code{"top"}, \code{"new"}, and
#'   \code{"rising"}. Default is \code{"hot"}.
#' @param period Character vector. Listing top threads by time period. Only applicable to sort order by \code{"top"}.
#'   Options are \code{"hour"}, \code{"day"}, \code{"week"}, \code{"month"}, \code{"year"} and \code{"all"}. Default is
#'   \code{"all"}.
#' @param max Numeric vector. Maximum number of threads in listing to return. Default is \code{25}.
#' @param waitTime Numeric vector. Time range in seconds to select random wait from in-between url collection requests.
#'   Minimum is 3 seconds. Default is \code{c(6, 8)} for a wait time chosen from between 6 and 8 seconds.
#' @param ua Character string. Override User-Agent string to use in Reddit thread requests. Default is
#'   \code{option("HTTPUserAgent")} value as set by vosonSML.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{tibble} object with class names \code{"listing"} and \code{"reddit"}.
#'
#' @examples
#' \dontrun{
#' # subreddit url to collect threads from
#' subreddits  <- c("datascience")
#'
#' redditListing <- redditAuth |>
#'   Collect(endpoint = "listing", subreddits = subreddits, sort = "new", writeToFile = TRUE)
#' }
#'
#' @export
Collect.listing.reddit <-
  function(credential,
           endpoint,
           subreddits,
           sort = "hot",
           period = "all",
           max = 25,
           waitTime = c(6, 8),
           ua = getOption("HTTPUserAgent"),
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {

    msg("Collecting thread listing for subreddits...\n")

    if (missing(subreddits)) {
      stop("Please provide a vector of one or more subreddit names.", call. = FALSE)
    }

    invisible(check_chr(subreddits, param = "subreddits"))

    # check sort
    sort_opts <- c("hot", "top", "new", "rising")
    invisible(cmp_values(sort, sort_opts, param = "sort", n = length(subreddits)))
    
    if (length(sort) == 1) sort <- rep(sort, length(subreddits))
    sort <- tolower(sort)
    
    # check period
    if (any(sort == "top")) {
      if (!length(period) %in% c(1, length(subreddits))) {
        stop("Please provide a period parameter that is length 1 or ", length(subreddits), ".", call. = FALSE)
      }
      if (length(period) == 1) period <- rep(period, length(subreddits))
      period <- tolower(period)
      
      period_opts <- c("hour", "day", "week", "month", "year", "all")
      invisible(cmp_values(period[which(sort == "top")], period_opts, param = "period"))
    }
    
    # check max
    max <- check_num(max, param = "max", gte = 1)
    
    if (!length(max) %in% c(1, length(subreddits))) {
      stop(
        "Please provide a max parameter of numeric type that is length 1 or ", length(subreddits), ".",
        call. = FALSE
      )
    }
    
    if (length(max) == 1) max <- rep(max, length(subreddits))
    
    # some protection against spamming requests
    waitTime <- check_wait_range_secs(waitTime, param = "waitTime", def_min = 3, def_max = 10)

    msg(paste0("Waiting between ", waitTime[1], " and ", waitTime[length(waitTime)], " seconds per request.\n"))

    listing_df <- NULL

    tryCatch({
      listing_df <- reddit_build_listing_df(subreddits, sort = sort, period = period, max = max,
                                            wait_time = waitTime, ua = ua, verbose = verbose)
    }, error = function(e) {
      # stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
      msg(gsub("^Error:\\s", "", paste0(e)))
    })
    
    if (!is.null(listing_df)) {
      if (nrow(listing_df) > 0) {
        # summary
        results_df <- listing_df |>
          dplyr::group_by(.data$subreddit_id) |>
          dplyr::summarise(subreddit_id = paste0(unique(.data$subreddit_id), collapse = ","),
                           subreddit = paste0(unique(.data$subreddit), collapse = ","),
                           count = dplyr::n()) |>
          dplyr::ungroup()

        msg(print_summary(results_df))
        msg(paste0("Collected metadata for ", nrow(listing_df), " threads in listings.\n"))
      } else {
        msg(paste0("No listings were collected.\n"))
      }
    } else {
      msg(paste0("Collection dataframe is null.\n"))
    }

    class(listing_df) <- append(c("listing", "reddit"), class(listing_df))
    
    # meta_log <- c(collect_log, paste0(format(Sys.time(), "%a %b %d %X %Y")))
    meta_log <- NULL
    
    if (writeToFile) write_output_file(listing_df, "rds", "RedditListing", verbose = verbose, log = meta_log)

    msg("Done.\n")

    listing_df
  }

reddit_build_listing_df <- function(subreddits, sort, period, max, wait_time, ua, verbose) {
  msg <- f_verbose(verbose)
  
  results <- NULL
  
  for (i in seq_along(1:length(subreddits))) {
    subreddit_i <- subreddits[i]
    sort_i <- sort[i]
    period_i <- period[i]
    max_i <- max[i]
    
    msg(paste0("Request subreddit listing: ", subreddit_i, " (max items: ", max_i, ")"))
    
    max_iter <- ceiling(max_i/25)
    results_i <- NULL
    qs <- NULL
    
    for (j in seq_along(1:max_iter)) {
      msg(".")
      url <- create_listing_url(subreddit_i, sort_i, period_i, qs)
      resp <- get_json(url, ua = ua, alt = TRUE)
      
      if (is.null(resp$status) || as.numeric(resp$status) != 200) {
        msg(paste0("Failed: ", url, ifelse(is.null(resp$status), "", paste0(" (", resp$status, ")")), "\n"))
      }
      
      data <- resp$data$data
      
      df <- tibble::as_tibble(data$children$data)
      
      # nested_df <- df |> dplyr::select("id", dplyr::where(is.list))
      # tryCatch({
      #   nested_df <- nested_df |> tidyr::unnest_longer(col = dplyr::where(is.list), keep_empty = TRUE)
      # }, error = function(e) { 
      #   msg(paste0("Unable to flatten data. ", e, "\n"))  
      # })
      
      results_i <- dplyr::bind_rows(results_i, df)

      if (!is.null(data$after)) {
        qs <- paste0("after=", data$after)
        if (j != max_iter) {
          Sys.sleep(sample(wait_time, 1))
        } else {
          msg("\n")
        }
      } else {
        msg("\n")
        break
      }
    }
    
    results <- dplyr::bind_rows(results, results_i)
    if (i != length(subreddits)) Sys.sleep(sample(wait_time, 1))
  }
  
  results
}
