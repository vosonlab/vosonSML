#' @title Collect data from social media for generating networks
#'
#' @description This function collects data from social media and structures it into a dataframe that can be used for
#'   creating networks for further analysis. \code{Collect} is the second step of the \code{\link{Authenticate}},
#'   \code{Collect}, and \code{\link{Create}} workflow.
#'
#'   Refer to \code{\link{Collect.search.twitter}}, \code{\link{Collect.timeline.twitter}},
#'   \code{\link{Collect.youtube}}, \code{\link{Collect.thread.reddit}}, \code{\link{Collect.listing.reddit}} and
#'   \code{\link{Collect.web}} for parameters and usage.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate}.
#' @param ... Optional parameters to pass to functions providied by supporting R packages that are used for social media
#'   API collection.
#'
#' @export
Collect <- function(credential, ...) {
  msg <- f_verbose(check_dots("verbose", ...))
  
  ts_ <- Sys.time()
  collect_log <- c(
    paste0("collect.", credential$socialmedia),
    paste0(format(ts_, "%a %b %d %X %Y")),
    paste0(format(ts_, tz = "UTC", usetz = TRUE)), "",
    paste0(names(list(...)), " = ", as.character(list(...)), collapse = "\n")
  )
  
  # set the environment encoding to UTF-8 for data collection
  saved_enc <- getOption("encoding")
  saved_ua <- getOption("HTTPUserAgent")
  saved_tz <- Sys.timezone()
  on.exit({
    options(encoding = saved_enc)
    options(HTTPUserAgent = saved_ua)
    Sys.setenv(TZ = saved_tz)
  }, add = TRUE)
  options(encoding = "UTF-8")
  options(HTTPUserAgent = paste0("vosonSML v.", get_version(), " (R Package)"))
  Sys.setenv(TZ = "Etc/UTC")

  endpoint <- check_dots("endpoint", ...)

  if (!is.null(endpoint)) {
    class(endpoint) <- append(class(endpoint), endpoint)
    UseMethod("Collect", endpoint)
  } else {
    UseMethod("Collect", credential)
  }
}

#' @export
Collect.default <- function(credential, ...) {
  stop("Unknown credential social media or endpoint passed to collect.", call. = FALSE)
}

#' @noRd
#' @method Collect twitter
#' @export
Collect.twitter <- function(credential, endpoint, ...) {
  UseMethod("Collect.search", credential)
}

#' @title Collect search data
#'
#' @noRd
#' @method Collect search
#' @export
Collect.search <- function(credential, endpoint, ...) {
  UseMethod("Collect.search", credential)
}

#' @noRd
#' @export
Collect.search.default <- function(credential, endpoint, ...) {
  stop("Unknown social media credential passed to collect search data", call. = FALSE)
}

#' @title Collect timeline data
#'
#' @noRd
#' @method Collect timeline
#' @export
Collect.timeline <- function(credential, endpoint, ...) {
  UseMethod("Collect.timeline", credential)
}

#' @noRd
#' @export
Collect.timeline.default <- function(credential, endpoint, ...) {
  stop("Unknown social media credential passed to collect timeline data", call. = FALSE)
}

# ----------

#' @noRd
#' @method Collect reddit
#' @export
Collect.reddit <- function(credential, endpoint, ...) {
  UseMethod("Collect.thread", credential)
}

#' @title Collect thread data
#'
#' @noRd
#' @method Collect thread
#' @export
Collect.thread <- function(credential, endpoint, ...) {
  UseMethod("Collect.thread", credential)
}

#' @noRd
#' @export
Collect.thread.default <- function(credential, endpoint, ...) {
  stop("Unknown social media credential passed to collect thread data", call. = FALSE)
}

#' @title Collect list data
#'
#' @noRd
#' @method Collect listing
#' @export
Collect.listing <- function(credential, endpoint, ...) {
  UseMethod("Collect.listing", credential)
}

#' @noRd
#' @export
Collect.listing.default <- function(credential, endpoint, ...) {
  stop("Unknown social media credential passed to collect listing data", call. = FALSE)
}
