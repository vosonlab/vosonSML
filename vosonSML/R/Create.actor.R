#' @title Create actor network from social media data
#'
#' @noRd
#' @method Create actor
#' @export
Create.actor <- function(datasource, type, ...) {
  UseMethod("Create.actor", datasource)
}

#' @export
Create.actor.default <- function(datasource, type, ...) {
  stop("Unknown datasource passed to create.", call. = FALSE) 
}
