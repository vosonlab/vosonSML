#' @title Create networks from social media data
#'
#' @description This function creates networks from social media data as produced from \code{\link{Collect}}.
#'   \code{Create} is the final step of the \code{\link{Authenticate}}, \code{\link{Collect}} and \code{Create}
#'   workflow.
#'
#' @param datasource Collected social media data of class \code{"datasource"} and \code{socialmedia}.
#' @param type Character string. Type of network to be created, can be \code{"activity"}, \code{"actor"},
#'   \code{"twomode"} or \code{"semantic"}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social media
#'   network creation.
#' @param writeToFile Logical. Write data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#'
#' @export
Create <- function(datasource, type, ..., writeToFile = FALSE, verbose = TRUE) {
  # searches the class list of datasource for matching method
  UseMethod("Create", type)
}

#' @export
Create.default <- function(datasource, type, ..., writeToFile = FALSE, verbose = TRUE) {
  # check datasource
  if (is.list(datasource) & !is.data.frame(datasource)) {
    if (check_df_n(datasource$tweets) < 1 &
        check_df_n(datasource$posts) < 1) {
      stop("Datasource passed to Create is invalid or empty.", call. = FALSE)
    }
  } else {
    if (check_df_n(datasource) < 1) {
      stop("Datasource passed to Create is invalid or empty.", call. = FALSE)
    }
  }

  # check if network type is a character string
  if (!is.character(type)) {
    stop("Create network type should be a character string.", call. = FALSE)
  }

  # check if function exists for network type
  # todo: perhaps search create methods so this can be extensible
  func_name <- paste0("Create", ".", type)
  if (!exists(func_name, where = asNamespace("vosonSML"), mode = "function")) {
    stop("Unknown network type passed to create.", call. = FALSE)
  }

  # add social media type to value class list
  class(type) <- append(class(type), type)

  # call create again
  Create(datasource, type, ..., writeToFile = writeToFile, verbose = verbose)
}

#' @title Create activity networks from social media data
#'
#' @noRd
#' @method Create activity
#' @export
Create.activity <- function(datasource, type, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Create.activity", datasource)
}

#' @noRd
#' @export
Create.activity.default <- function(datasource, type, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown datasource passed to create activity network.", call. = FALSE)
}

#' @title Create actor network from social media data
#'
#' @noRd
#' @method Create actor
#' @export
Create.actor <- function(datasource, type, ..., writeToFile = FALSE, verbose = TRUE) {
  UseMethod("Create.actor", datasource)
}

#' @noRd
#' @export
Create.actor.default <- function(datasource, type, ..., writeToFile = FALSE, verbose = TRUE) {
  stop("Unknown datasource passed to create.", call. = FALSE)
}
