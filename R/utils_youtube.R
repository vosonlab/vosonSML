# check and extract ids from youtube video urls using regex
get_yt_video_ids <- function(x) {
  id_regex <- "[0-9A-Za-z_\\-]+"
  id_regex_a <- paste0("^", id_regex, "$")
  url_regex_b <- paste0("^(?:https://)?youtu\\.be/(", id_regex, ")?/{0,1}$")
  url_regex_c <- paste0("^(?:https://)?(?:www\\.)?youtube\\.com/watch\\?v=(", id_regex, ")?/{0,1}$")

  x <- stringr::str_remove(x, "t=[0-9]+?s$")

  y <- stringr::str_match(
    as.character(x),
    paste0("(", id_regex_a, ")|", url_regex_b, "|", url_regex_c))

  res <- dplyr::coalesce(y[, 2], y[, 3], y[, 4])

  res
}
