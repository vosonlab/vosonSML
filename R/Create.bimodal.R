#' @title Create bimodal networks from social media data
#'
#' @noRd
#' @method Create bimodal
#' @export
Create.bimodal <- function(datasource, type, ...) {
  UseMethod("Create.bimodal", datasource)
}

#' @noRd
#' @export
Create.bimodal.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create bimodal network.", call. = FALSE) 
}
