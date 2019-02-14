#' @title Creates a semantic network from social media data
#'
#' @noRd 
#' @method Create semantic
#' @export
Create.semantic <- function(datasource, type, ...) {
  UseMethod("Create.semantic", datasource)
}

#' @noRd
#' @export
Create.semantic.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create semantic network.", call. = FALSE) 
}
