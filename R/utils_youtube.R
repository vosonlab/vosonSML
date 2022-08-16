# check and extract ids from youtube video urls using regex
get_yt_video_ids <- function(x) {
  id_regex <- "[0-9A-Za-z_\\-]+"
  id_regex_1 <- paste0("^", id_regex, "$")
  url_regex_1 <- paste0("^(?:https://)?youtu\\.be/(", id_regex, ")?/{0,1}$")
  url_regex_2 <- paste0("^(?:https://)?(?:www\\.)?youtube\\.com/watch\\?v=(", id_regex, ")?/{0,1}$")
  url_regex_3 <- paste0("^(?:https://)?(?:www\\.)?youtube\\.com/shorts/(", id_regex, ")?/{0,1}$")

  x <- stringr::str_remove(x, "&t=[0-9]+?s$")

  y <- stringr::str_match(
    as.character(x),
    paste0("(", id_regex_1, ")|", url_regex_1, "|", url_regex_2, "|", url_regex_3))

  res <- dplyr::coalesce(y[, 2], y[, 3], y[, 4], y[, 5])

  res
}
