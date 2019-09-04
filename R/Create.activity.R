#' @title Create activity networks from social media data
#'
#' @noRd
#' @method Create activity
#' @export
Create.activity <- function(datasource, type, ...) {
  UseMethod("Create.activity", datasource)
}

#' @noRd
#' @export
Create.activity.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create activity network.", call. = FALSE) 
}
