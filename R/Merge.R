#' @title Merge collected data files
#'
#' @param path Directory path of Collect data to merge. Default is the working directory.
#' @param pattern Regular expression (regex) for matching file names to merge.
#' @param unique Logical. Remove duplicates based on observation id. Default is \code{TRUE}.
#' @param rev Logical. Reverses order of observations before removing duplicates. If collect data is provided
#'   chronologically then this should ensure the most recent copy of a duplicate is kept. Default is \code{TRUE}.
#' @param writeToFile Logical. Save data to a file in the current working directory. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#'
#' @return A merged Collect object.
#'
#' @aliases MergeFiles
#' @name MergeFiles
#' @export
MergeFiles <-
  function(path = ".",
           pattern = "(?-i).+?\\.rds$",
           unique = TRUE,
           rev = TRUE,
           writeToFile = FALSE,
           verbose = TRUE) {
    
    merge_log <- init_merge_log(
      path = path, pattern = pattern, unique = unique, rev = rev, writeToFile = writeToFile, verbose = verbose)
    
    msg("Merging collect files...\n")

    if (!dir.exists(path)) stop("Path does not exist.")

    files <- list.files(path = path, pattern = pattern, full.names = TRUE)
    if (length(files) < 1) stop("No matching files found.")

    merge_log <- append(merge_log, files)
    
    msg(paste0("Matching files:\n", paste0(paste0("- ", files), collapse = "\n"), "\n"))

    data <- purrr::reduce(lapply(files, readRDS), .f = Merge)

    merge_log <- c(merge_log, paste0(format(Sys.time(), "%a %b %d %X %Y")))
    
    if (writeToFile) write_output_file(data, "rds", "DataMergeFile", verbose = verbose, log = merge_log)
    msg("Done.\n")

    data
  }

#' @title Merge collected data
#'
#' @param ... Collect data to merge.
#' @param unique Logical. Remove duplicates based on observation id. Default is \code{TRUE}.
#' @param rev Logical. Reverses order of observations before removing duplicates. If collect data is provided
#'   chronologically then this should ensure the most recent copy of a duplicate is kept. Default is \code{TRUE}.
#' @param writeToFile Logical. Save data to a file in the current working directory. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#'
#' @return A merged Collect object.
#'
#' @aliases Merge
#' @name Merge
#' @export
Merge <- function(..., unique = TRUE, rev = TRUE, writeToFile = FALSE, verbose = TRUE) {
  
  msg("Merging collect data...\n")

  merge_dots <- list(...)
  merge_cls_list <- lapply(merge_dots, class)

  if (!identical(unname(merge_cls_list[-length(merge_cls_list)]), unname(merge_cls_list[-1]))) {
    stop("Collect data must be of the same class.", call. = FALSE)
  }

  merge_cls_data <- unlist(merge_cls_list[1])
  class(merge_dots) <- merge_cls_data

  UseMethod("Merge", merge_dots)
}

#' @noRd
#' @export
Merge.default <- function(..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown data type passed to merge.", call. = FALSE)
}

#' @noRd
#' @method Merge mastodon
#' @export
Merge.mastodon <- function(..., unique = TRUE, rev = TRUE, writeToFile = FALSE, verbose = TRUE) {
  merge_log <- init_merge_log(
    path = NULL, pattern = NULL, params = paste0(as.list(match.call()), collapse = "\n"),
    unique = unique, rev = rev, writeToFile = writeToFile, verbose = verbose)
  
  merge_dots <- list(...)
  merge_cls_list <- lapply(merge_dots, class)
  merge_cls_data <- unlist(merge_cls_list[1])
  
  posts_data <- purrr::map_dfr(merge_dots, \(x) x$posts)
  users_data <- purrr::map_dfr(merge_dots, \(x) x$users)
  
  if (unique) {
    if (rev) {
      posts_data <- rev_row_order(posts_data)
      users_data <- rev_row_order(users_data)
    }
    
    posts_data <- posts_data |> dplyr::distinct(.data$id, .keep_all = TRUE)
    users_data <- users_data |> dplyr::distinct(.data$id, .keep_all = TRUE)
    
    if (rev) {
      posts_data <- rev_row_order(posts_data)
      users_data <- rev_row_order(users_data)
    }
  }
  
  data <- list(posts = posts_data, users = users_data)
  class(data) <- merge_cls_data
  
  if (writeToFile) write_output_file(data, "rds", "MastodonDataMerge", verbose = verbose, log = merge_log)
  msg("Done.\n")
  
  data
}

#' @noRd
#' @method Merge youtube
#' @export
Merge.youtube <- function(..., unique = TRUE, rev = TRUE, writeToFile = FALSE, verbose = TRUE) {
  merge_log <- init_merge_log(
    path = NULL, pattern = NULL, params = paste0(as.list(match.call()), collapse = "\n"),
    unique = unique, rev = rev, writeToFile = writeToFile, verbose = verbose)
  
  merge_dots <- list(...)
  merge_cls_list <- lapply(merge_dots, class)
  merge_cls_data <- unlist(merge_cls_list[1])
  
  data <- dplyr::bind_rows(...)
  class(data) <- merge_cls_data

  if (unique) {
    if (rev) data <- rev_row_order(data)
    data <- data |> dplyr::distinct(.data$VideoID, .data$CommentID, .keep_all = TRUE)
    if (rev) data <- rev_row_order(data)
  }

  if (writeToFile) write_output_file(data, "rds", "YoutubeDataMerge", verbose = verbose, log = merge_log)
  msg("Done.\n")

  data
}

#' @noRd
#' @method Merge reddit
#' @export
Merge.reddit <- function(..., unique = TRUE, rev = TRUE, writeToFile = FALSE, verbose = TRUE) {
  merge_log <- init_merge_log(
    path = NULL, pattern = NULL, params = paste0(as.list(match.call()), collapse = "\n"),
    unique = unique, rev = rev, writeToFile = writeToFile, verbose = verbose)
  
  merge_dots <- list(...)
  merge_cls_list <- lapply(merge_dots, class)
  merge_cls_data <- unlist(merge_cls_list[1])

  data <- dplyr::bind_rows(...)
  class(data) <- merge_cls_data

  if (unique) {
    if (rev) data <- rev_row_order(data)
    data <- data |> dplyr::distinct(.data$subreddit, .data$thread_id, .data$comm_id, .keep_all = TRUE)
    if (rev) data <- rev_row_order(data)
  }

  if (writeToFile) write_output_file(data, "rds", "RedditDataMerge", verbose = verbose, log = merge_log)
  msg("Done.\n")

  data
}

init_merge_log <- function(
    path = NULL, pattern = NULL, params = NULL, unique = TRUE, rev = TRUE, writeToFile = FALSE, verbose = TRUE) {
  
  ts_ <- Sys.time()
  
  merge_log <- c(
    paste0("unique = ", unique),
    paste0("rev = ", rev),
    paste0("writeToFile = ", writeToFile),
    paste0("verbose = ", verbose), ""
  )
  
  if (is.null(path) | is.null(pattern)) {
    merge_log <- c(
      paste0("merge.files"),
      paste0(format(ts_, "%a %b %d %X %Y")),
      paste0(format(ts_, tz = "UTC", usetz = TRUE)), "",
      paste0("path = ", path),
      paste0("pattern = ", pattern),
      merge_log
    )
  } else {
    merge_log <- c(
      paste0("merge.data"),
      paste0("time: ", format(ts_, "%a %b %d %X %Y")),
      paste0("utc:  ", format(ts_, tz = "UTC", usetz = TRUE)), "",
      merge_log,
      params
    )
  }
  
  merge_log
}