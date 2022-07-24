#' @title Extract the ids from a list of YouTube video urls
#'
#' @description This function reads YouTube video urls from a list and or a text file and converts them to a vector of
#'   video ids. For example, URL \code{https://www.youtube.com/watch?v=73I5dRucCds} returns the id \code{73I5dRucCds}.
#'   This function can be used to create a vector for the YouTube \code{\link{Collect.youtube}} functions
#'   \code{videoIDs} parameter.
#'
#' @note Accepts YouTube url formats \code{https://youtu.be/xxxxxxxx} and
#'   \code{https://www.youtube.com/watch?v=xxxxxxxx}.
#'
#' @param urls Character vector. List of YouTube urls or ids.
#' @param verbose Logical. Output additional information. Default is \code{FALSE}.
#'
#' @return A vector of YouTube video ids as character strings that were extracted from input video urls.
#'
#' @aliases GetYoutubeVideoIDs
#' @name GetYoutubeVideoIDs
#' @export
GetYoutubeVideoIDs <-
  get_video_ids <- function(urls = c(), verbose = FALSE) {
    msg <- f_verbose(verbose)

    video_ids <- c()

    if (is.null(urls) || !is.vector(urls)) {
      stop("Please provide a vector of YouTube video urls.")
    }

    ids <- get_yt_video_ids(urls)
    video_ids <- ids[!is.na(ids)]

    if (length(video_ids) < 1) {
      msg("Failed to extract any ids.\n")
      return(video_ids)
    } else {
      if (any(is.na(ids))) {
        msg("Failed to extract ids.\n")
        failed <- paste0("- ", urls[is.na(ids)])
        msg(paste0(failed, collapse = "\n"))
        msg("\n")
      }
    }

    msg(paste0("Extracted ", length(video_ids), " video ids.\n"))

    video_ids
  }

# extract youtube video ids using regex
get_yt_video_ids <- function(x) {
  id_regex <- "[0-9A-Za-z_\\-]+"
  id_regex_a <- paste0("^", id_regex, "$")
  url_regex_b <- paste0("^(?:https://)?youtu\\.be/(", id_regex, ")?/{0,1}$")
  url_regex_c <- paste0("^(?:https://)?(?:www\\.)?youtube\\.com/watch\\?v=(", id_regex, ")?/{0,1}$")

  y <- stringr::str_match(
    as.character(x),
    paste0("(", id_regex_a, ")|", url_regex_b, "|", url_regex_c))

  res <- dplyr::coalesce(y[, 2], y[, 3], y[, 4])
}
