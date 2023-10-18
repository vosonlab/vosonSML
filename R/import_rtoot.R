#' @title Import rtoot collected data
#'
#' @description Imports \pkg{rtoot} collected data from \code{rda} or \code{rds} saved object file or from an rtoot
#'   dataframe. Ensures \code{datasource} and specified \code{socialmedia} type are set so data is usable by
#'   \code{\link{Create}} functions. Not required if collected data was collected by \code{vosonSML} and saved as an
#'   \code{rds} file, use \code{\link{readRDS}} instead.
#'
#' @note Only supports \pkg{rtoot} data collected using the \code{\link[rtoot]{get_timeline_hashtag}},
#'   \code{\link[rtoot]{get_timeline_public}}, \code{\link[rtoot]{get_status}}, \code{\link[rtoot]{get_context}}
#'   functions.
#'
#' @param data Character string or dataframe. File path to or tibble of collected data from \pkg{rtoot}.
#'
#' @return A dataframe suitable for input into mastodon network \code{\link{Create}} functions.
#'
#' @examples
#' \dontrun{
#' # import rtoot collected data from dataframe
#' collect_mast <- ImportRtoot(rtoot_data)
#'
#' # import rtoot collected data from file
#' collect_mast <- ImportRtoot("./rtoot_search_n100.rds")
#' }
#'
#' @aliases ImportRtoot
#' @name ImportRtoot
#' @export
ImportRtoot <- function(data) {
  if (missing(data)) {
    stop("Please provide file path or dataframe to import.", call. = FALSE)
  }

  if (check_df_n(data) > 0) return(import_rtoot_(data))

  data <- check_chr(data, param = "data")

  if (!file.exists(data)) {
    stop("Import file not found.", call. = FALSE)
  }

  type <- gsub(
    ".*\\.([A-Za-z]{3})$",
    "\\1",
    data,
    ignore.case = TRUE,
    perl = TRUE
  )

  df <- tryCatch({
    switch(
      tolower(type),
      "rda" = load(file = data),
      "rds" = readRDS(file = data)
    )
  }, error = function(e) {
    stop(paste0(
      "Could not read: ",
      data,
      ".\n",
      gsub("^Error:\\s", "", paste0(e))
    ), call. = FALSE)
  })

  import_rtoot_(df)
}

# import rtoot data
import_rtoot_ <- function(...) {
  # .progress = "extracting thread posts"
  data <- purrr::map(list(...), ~ unlist_context(.x), .progress = FALSE)
  
  # .progress = "processing posts"
  df_posts <- purrr::map(data, ~ get_posts(.x), .progress = FALSE) |> purrr::list_rbind() 
  df_users <- purrr::map(data, ~ get_users(.x), .progress = FALSE) |> purrr::list_rbind()
  
  # if (unique) {
    df_posts <- df_posts |> dplyr::distinct(.data$id, .keep_all = TRUE)
    df_users <- df_users |> dplyr::distinct(.data$id, .keep_all = TRUE)
  # }
  
  data <- list(posts = df_posts, users = df_users)
  class(data) <- append(c("datasource", "mastodon"), class(data))

  data
}

# return all context posts in a single tibble
unlist_context <- function(x) {
  if (is.list(x) && ("ancestors" %in% names(x)) && ("descendants" %in% names(x))) {
    y <- dplyr::bind_rows(x$ancestors, x$descendants)
    return(y)
  }
  
  x
}

# clean posts data
get_posts <- function(data) {
  if (!"content" %in% colnames(data)) return(data)
  
  data |>
    dplyr::mutate(content.text = ensure_tags(.data$content)) |>
    html_text("content.text")
}

# extract and clean user data
get_users <- function(data) {
  if ("account" %in% colnames(data)) {
    data <- data |>
      dplyr::select(.data$account) |>
      tidyr::unnest_wider(.data$account)  
  }
  
  if (!"note" %in% colnames(data)) return(data)
  
  data <- data |>
    dplyr::mutate(note.text = ensure_tags(.data$note)) |>
    html_text("note.text")
}

# html to text conversion
html_text <- function(data, cols) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate_at(cols, ~ rvest::html_text2(rvest::read_html(.x)), na.rm = TRUE) |>
    dplyr::ungroup()
}

# ensure html text is wrapped in html tags
ensure_tags <- function(x, ...) {
  dplyr::if_else(stringr::str_detect(x, "^<p>.*", negate = TRUE), paste0("<p>", x, "</p>"), x, ...)
}
