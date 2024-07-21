# wrappers

#' @rdname Collect.thread.reddit
#' @export
collect_reddit_threads <-
  function(threadUrls,
           sort = "best",
           waitTime = c(6, 8),
           ua = vsml_ua(),
           writeToFile = FALSE,
           verbose = TRUE,
           ...) {
    Collect(
      credential = Authenticate("reddit"),
      endpoint = "thread",
      threadUrls = threadUrls,
      sort = sort,
      waitTime = waitTime,
      ua = ua,
      ...,
      writeToFile = writeToFile,
      verbose = verbose
    )
  }

#' @rdname Collect.listing.reddit
#' @export
collect_reddit_listings <-
  function(subreddits,
           sort = "new",
           period = NULL,
           max = 25,
           waitTime = c(6, 8),
           ua = vsml_ua(),
           writeToFile = FALSE,
           verbose = TRUE,
           ...) {
    Collect(
      credential = Authenticate("reddit"),
      endpoint = "listing",
      subreddits = subreddits,
      sort = sort,
      period = period,
      max = max,
      waitTime = waitTime,
      ua = ua,
      ...,
      writeToFile = writeToFile,
      verbose = verbose
    )
  }

#' @rdname Collect.web
#' @export
collect_web_hyperlinks <- function(pages = NULL, writeToFile = FALSE, verbose = TRUE, ...) {
  Collect(
    credential = Authenticate("web"),
    pages = pages,
    ...,
    writeToFile = writeToFile,
    verbose = verbose
  )
}

# aliases

#' @rdname ImportRtoot
#' @export
import_rtoot <- ImportRtoot

#' @rdname Merge
#' @export
merge_data <- Merge

#' @rdname MergeFiles
#' @export
merge_files <- MergeFiles

#' @rdname AddText
#' @export
add_text <- AddText

#' @rdname AddVideoData
#' @export
add_videos <- AddVideoData
