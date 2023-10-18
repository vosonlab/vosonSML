#' @title Collect comments data from reddit threads
#'
#' @description Collects comments made by users on one or more specified subreddit conversation threads and structures
#'   the data into a dataframe with the class names \code{"datasource"} and \code{"reddit"}.
#'
#' @note The reddit web endpoint used for collection has maximum limit of 500 comments per thread url.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"reddit"}.
#' @param endpoint API endpoint.
#' @param threadUrls Character vector. Reddit thread urls to collect data from.
#' @param sort Character vector. Reddit comment sort order. Options are \code{"best"}, \code{"top"}, \code{"new"},
#'   \code{"controversial"}, \code{"old"}, and \code{"qa"}. Default is \code{NA}.
#' @param waitTime Numeric vector. Time range in seconds to select random wait from in-between url collection requests.
#'   Minimum is 3 seconds. Default is \code{c(6, 8)} for a wait time chosen from between 6 and 8 seconds.
#' @param ua Character string. Override User-Agent string to use in Reddit thread requests. Default is
#'   \code{option("HTTPUserAgent")} value as set by vosonSML.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{tibble} object with class names \code{"datasource"} and \code{"reddit"}.
#'
#' @examples
#' \dontrun{
#' # subreddit url to collect threads from
#' threadUrls <- c("https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/")
#'
#' redditData <- redditAuth |>
#'   Collect(threadUrls = threadUrls, writeToFile = TRUE)
#' }
#'
#' @export
Collect.thread.reddit <-
  function(credential,
           endpoint,
           threadUrls,
           sort = NA,
           waitTime = c(6, 8),
           ua = getOption("HTTPUserAgent"),
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {

    msg("Collecting comment threads for reddit urls...\n")

    if (missing(threadUrls)) {
      stop("Please provide a vector of one or more reddit thread urls.", call. = FALSE)
    }

    invisible(check_chr(threadUrls, param = "threadUrls"))
    
    # check sort
    sort_opts <- c("best", "top", "new", "controversial", "old", "qa", NA)
    invisible(cmp_values(sort, sort_opts, param = "sort", n = length(threadUrls)))
    
    if (length(sort) == 1) sort <- rep(sort, length(threadUrls))
    sort <- tolower(sort)

    # some protection against spamming requests
    waitTime <- check_wait_range_secs(waitTime, param = "waitTime", def_min = 3, def_max = 10)

    msg(paste0("Waiting between ", waitTime[1], " and ", waitTime[length(waitTime)], " seconds per thread request.\n"))

    threads_df <- NULL

    tryCatch({
      threads_df <- reddit_build_df(threadUrls, sort, waitTime, ua, verbose)
    }, error = function(e) {
      # stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
      msg(gsub("^Error:\\s", "", paste0(e)))
    }, finally = {
      threads_df <- tibble::as_tibble(threads_df)
    })

    if (!is.null(threads_df)) {
      if (nrow(threads_df) > 0) {
        msg("HTML decoding comments.\n")
        threads_df$comment <- textutils::HTMLdecode(threads_df$comment)

        # summary
        results_df <- threads_df |>
          dplyr::group_by(.data$thread_id) |>
          dplyr::summarise(
            title = paste0(unique(.data$title), collapse = ","),
            subreddit = paste0(unique(.data$subreddit), collapse = ","),
            count = dplyr::n()
          ) |>
          dplyr::ungroup()

        results_df$title <-
          ifelse(nchar(results_df$title) > 42,
                 paste0(strtrim(results_df$title, 42), "..."),
                 results_df$title)
        msg(print_summary(results_df))
        msg(paste0("Collected ", nrow(threads_df), " total comments.\n"))
      } else {
        msg(paste0("No comments were collected.\n"))
      }
    } else {
      msg(paste0("Collection dataframe is null.\n"))
    }

    class(threads_df) <- append(c("datasource", "reddit"), class(threads_df))
    
    meta_log <- c(collect_log, paste0(format(Sys.time(), "%a %b %d %X %Y")))
    
    if (writeToFile) write_output_file(threads_df, "rds", "RedditData", verbose = verbose, log = meta_log)

    msg("Done.\n")

    threads_df
  }

reddit_build_df <- function(threadUrls, sort, waitTime, ua, verbose) {
  msg <- f_verbose(verbose)
  
  threads <- list()
  for (thread_i in seq_along(threadUrls)) {
    
    url <- threadUrls[thread_i]
    url_sort <- sort[thread_i]
    if (length(threadUrls) > 1 & (url != threadUrls[1])) {
      Sys.sleep(sample(waitTime, 1))
    }
    
    thread_json <- reddit_data(url, url_sort, wait_time = waitTime, ua = ua, verbose = verbose)
    branch_df <- reddit_content_plus(thread_json, url, verbose = verbose)

    # loop protection
    prev_value <- NULL

    extra_threads <- dplyr::filter(branch_df, grepl("Listing:", .data$comm_id))
    while (nrow(extra_threads) > 0) {
      row_i <- 1 # top row

      # loop protection
      if (!is.null(prev_value) && extra_threads[row_i, "comm_id"] == prev_value) {
        msg("Loop protection following continue threads. Exiting.\n")
        break
      }
      prev_value <- extra_threads[row_i, "comm_id"]

      Sys.sleep(sample(waitTime, 1))

      # get continue thread comment position info
      cont_index <- as.numeric(extra_threads[row_i, "id"])
      depth <- as.numeric(gsub(".*_(\\d)_\\d$", "\\1", extra_threads[row_i, "structure"]))
      struct <- gsub("_\\d_\\d$", "", extra_threads[row_i, "structure"])

      row_comm_id <- extra_threads[row_i, "comm_id"]
      cont_thread_id <- gsub("Listing:t1_", "", row_comm_id)
      
      # set continue thread comment rm flag to true
      branch_df <- branch_df |>
        dplyr::mutate(
          rm = ifelse(.data$comm_id %in% c(cont_thread_id, row_comm_id), TRUE, .data$rm)
        )

      # if trailing slash missing from url
      if (!grepl("/$", url)) url <- paste0(url, "/")
      
      # get continue thread
      cont_json <- reddit_data(
        paste0(url, cont_thread_id),
        url_sort,
        waitTime,
        ua,
        cont = cont_thread_id,
        verbose = verbose)
      
      cont_df <- reddit_content_plus(cont_json, url, depth = depth, verbose = verbose)

      # if comments returned
      if (nrow(cont_df)) {
        cont_df <- cont_df |> dplyr::mutate(structure = paste0(struct, "_", .data$structure)) # append structure

        # insert new comments into thread dataframe using position
        if (cont_index == 1) {
          branch_df <- dplyr::bind_rows(cont_df, branch_df)
        } else {
          pre_df <- dplyr::bind_rows(branch_df[1:cont_index - 1, ], cont_df)
          branch_df <- dplyr::bind_rows(pre_df, branch_df[cont_index:nrow(branch_df), ])
        }
      }

      extra_threads <- extra_threads[-row_i, ] # not needed
      extra_threads <- dplyr::filter(branch_df, grepl("Listing:", .data$comm_id), .data$rm == FALSE)
      
    } # end while

    if (!is.null(branch_df) && nrow(branch_df) > 0) {
      branch_df$thread_id <-
        gsub(
          "^(.*)?/comments/([0-9A-Za-z]{2,})?/{0,1}(.*)?/{0,1}$",
          "\\2",
          branch_df$url,
          ignore.case = TRUE,
          perl = TRUE,
          useBytes = TRUE
        ) # extract thread id
      
      branch_df <- branch_df |>
        dplyr::filter(.data$rm == FALSE) |> # remove continue thread entries
        dplyr::arrange(.data$thread_id, .data$id)

      branch_df$id <- seq_along(branch_df$id) # re-index
    }

    threads[[thread_i]] <- branch_df
  }
  
  threads_df <- dplyr::bind_rows(threads) |> dplyr::select(-.data$rm)

  threads_df
}

# based on method by @ivan-rivera RedditExtractoR
reddit_data <-
  function(url,
           url_sort,
           wait_time,
           ua,
           cont = NULL,
           verbose = FALSE) {

    msg <- f_verbose(verbose)
    
    if (is.null(url) || length(url) == 0 || !is.character(url)) {
      stop("invalid URL parameter")
    }
    
    req_url <- create_thread_url(url, url_sort)
    req_tid <- get_thread_id(req_url, TRUE)

    if (is.null(cont)) {
      msg(paste0("Request thread: ", req_tid, " - sort: ", url_sort, "\n"))
    } else {
      msg(paste0("Continue thread: ", req_tid, " - ", cont, "\n"))
    }

    req_data <- get_json(req_url, ua) # ua

    if (is.null(req_data$data)) {
      Sys.sleep(sample(wait_time, 1))

      if (is.null(cont)) {
        msg(paste0("Retry thread: ", req_tid, "\n"))
      } else {
        msg(paste0("Retry continue thread: ", req_tid, " - ", cont, "\n"))
      }

      req_data <- get_json(req_url, ua) # ua
    }

    if (is.null(req_data$status) || as.numeric(req_data$status) != 200) {
      msg(paste0("Failed: ", url, ifelse(is.null(req_data$status), "", paste0(" (", req_data$status, ")")), "\n"))
    }

    req_data$data
  }

# based on method by @ivan-rivera RedditExtractoR
reddit_values_list  <- function(node, feature) {
  attr <- node$data[[feature]]
  
  if (is.null(attr)) attr <- NA
  if (feature == "id") {
    if (attr == "_") {
      attr <- paste0("Listing:", node$data$parent_id)
    }
  }
  
  reply_nodes <- NULL
  replies <- node$data$replies
  if (is.list(replies)) reply_nodes <- replies$data$children

  attrs <- list(attr, lapply(reply_nodes, function(x) reddit_values_list(x, feature)))

  attrs
}

# based on method by @ivan-rivera RedditExtractoR
reddit_struct_list <- function(node, depth = 0) {
  if (is.null(node)) return(list())

  reply_nodes <- NULL
  replies <- node$data$replies
  if (is.list(replies)) reply_nodes <- replies$data$children

  # depth is converted to char to prevent col type to integer with no-depth threads
  structures <- list(
    as.character(depth),
    lapply(
      1:length(reply_nodes),
      function(x) reddit_struct_list(reply_nodes[[x]], paste0(depth, "_", x))
    )
  )

  structures
}

# based on method by @ivan-rivera RedditExtractoR
reddit_content_plus <- function(raw_data, req_url, depth = 0, verbose = FALSE) {
  msg <- f_verbose(verbose)
  
  data_extract <- tibble::tibble(
    id = numeric(),
    structure = character(),
    post_date = character(),
    post_date_unix = numeric(),
    comm_id = character(),
    comm_date = character(),
    comm_date_unix = numeric(),
    num_comments = numeric(),
    subreddit = character(),
    upvote_prop = numeric(),
    post_score = numeric(),
    author = character(),
    user = character(),
    comment_score = numeric(),
    controversiality = numeric(),
    comment = character(),
    title = character(),
    post_text = character(),
    link = character(),
    domain = character(),
    url = character(),
    rm = logical()
  )

  if (is.null(raw_data)) return(data_extract)

  meta_node <- raw_data[[1]]$data$children[[1]]$data
  main_node <- raw_data[[2]]$data$children

  if (min(length(meta_node), length(main_node)) > 0) {
    structures_list <- unlist(lapply(1:length(main_node), function(x) {
      reddit_struct_list(main_node[[x]], depth = ifelse(depth != 0, depth, x))
    }))

    data <- tibble::tibble(
      id               = NA,
      structure        = structures_list,
      post_date        = as.character(lubridate::as_datetime(
        as.numeric(meta_node$created_utc), tz = "UTC"
      )),
      post_date_unix   = as.numeric(meta_node$created_utc),
      comm_id          = unlist(lapply(main_node, function(x) {
        reddit_values_list(x, "id")
      })),
      comm_date        = as.character(lubridate::as_datetime(as.numeric(
        unlist(lapply(main_node, function(x) {
          reddit_values_list(x, "created_utc")
        }))
      ), tz = "UTC")),
      comm_date_unix   = as.numeric(unlist(lapply(main_node, function(x) {
        reddit_values_list(x, "created_utc")
      }))),
      num_comments     = meta_node$num_comments,
      subreddit        = ifelse(
        is.null(meta_node$subreddit),
        "UNKNOWN",
        meta_node$subreddit
      ),
      upvote_prop      = meta_node$upvote_ratio,
      post_score       = meta_node$score,
      author           = meta_node$author,
      user             = unlist(lapply(main_node, function(x) {
        reddit_values_list(x, "author")
      })),
      comment_score    = unlist(lapply(main_node, function(x) {
        reddit_values_list(x, "score")
      })),
      controversiality = unlist(lapply(main_node, function(x) {
        reddit_values_list(x, "controversiality")
      })),
      comment          = unlist(lapply(main_node, function(x) {
        reddit_values_list(x, "body")
      })),
      title            = meta_node$title,
      post_text        = meta_node$selftext,
      link             = meta_node$url,
      domain           = meta_node$domain,
      url              = req_url,
      rm               = FALSE
    )

    data$id <- 1:nrow(data)

    if (dim(data)[1] > 0 && dim(data)[2] > 0) {
      data_extract <- rbind(data, data_extract)
    } else {
      msg(paste0("No data: ", req_url, "\n"))
    }
  }

  data_extract
}
