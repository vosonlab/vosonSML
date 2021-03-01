#' @title Extract the ids from a list of youtube video urls
#'
#' @description This function reads youtube video urls from a list and or a text file and converts them to a vector of
#' video ids. For example, URL \code{https://www.youtube.com/watch?v=73I5dRucCds} returns the id \code{73I5dRucCds}.
#' This function can be used to create a vector for the youtube \code{\link{Collect.youtube}} functions \code{videoIDs}
#' parameter.
#'
#' @note Accepts youtube url formats \code{https://youtu.be/xxxxxxxx} and
#' \code{https://www.youtube.com/watch?v=xxxxxxxx}.
#'
#' @param urls Character vector. List of youtube urls.
#' @param file Character string. Text file containing youtube urls, one per line.
#'
#' @return A vector of youtube video ids as character strings that were extracted from input video urls.
#'
#' @aliases GetYoutubeVideoIDs
#' @name vosonSML::GetYoutubeVideoIDs
#' @export
GetYoutubeVideoIDs <- get_video_ids <- function(urls = NULL, file = NULL) {
  video_ids <- c()

  if (is.null(urls) & is.null(file)) {
    cat("Please provide a vector and or file of youtube video urls.\n")
    return(video_ids)
  }

  if (!is.null(urls)) {
    video_ids <-
      append(video_ids, sapply(urls, get_video_id, USE.NAMES = FALSE))
  }

  if (!is.null(file)) {
    video_ids_file <- tryCatch({
      read.table(file, sep = "\n", strip.white = TRUE)
    }, error = function(e) {
      cat(paste0(e))
      return(NULL)
    })

    if (!is.null(video_ids_file)) {
      video_ids_file <- as.vector(video_ids_file$V1)
      video_ids <-
        append(video_ids, sapply(urls, get_video_id, USE.NAMES = FALSE))
    }
  }

  if (length(video_ids) < 1) {
    cat("No youtube video ids found.\n")
  } else {
    cat(paste0("Extracted ", length(video_ids), " video ids.\n"))
  }

  video_ids
}

# extract the id from a youtube video url
get_video_id <- function(url) {
  id_pattern <- "^[0-9A-Za-z_\\-]+$"

  # already an id
  if (grepl(id_pattern, url, ignore.case = TRUE, perl = TRUE)) {
    return(url)
  }

  url <- httr::parse_url(url)
  video_id <- NULL

  if (is.null(url$hostname)) {
    return(NULL)
  }

  # url format https://youtu.be/xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "youtu.be") {
    if (length(url$path) > 0) {
      video_id <- url$path[1]
    }
  }

  # url format https://www.youtube.com/watch?v=xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "www.youtube.com") {
    if (!is.null(url$query$v)) {
      video_id <- url$query$v
    }
  }

  if (!grepl(id_pattern,
             video_id,
             ignore.case = TRUE,
             perl = TRUE)) {
    return(NULL)
  }

  video_id
}
