#' @title Twitter API v2 authentication
#'
#' @description Twitter API v2 authentication uses OAuth2 and requires app consumer keys or bearer token.
#'
#' @param socialmedia Character string. Identifier for social media API to authenticate, set to \code{"twitter2"}.
#' @param consumerKey Character string. API consumer key to authenticate.
#' @param consumerSecret Character string. API consumer secret to authenticate.
#' @param bearerToken Character string. API access token to authenticate.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{credential} object containing a bearer token \code{$bearer} and social media type descriptor
#'   \code{$socialmedia} set to \code{"twitter2"}. Object has the class names \code{"credential"} and \code{"twitter2"}.
#'
#' @examples
#' \dontrun{
#' # twitter API v2 consumer keys
#' twitter2Auth <- Authenticate("twitter2",
#'   consumerKey = "xxxxxxxx",
#'   consumerSecret = "xxxxxxxx")
#'
#' # twitter API v2 bearer token
#' twitter2Auth <- Authenticate("twitter2",
#'   bearerToken = "xxxxxxxx")
#' )
#' }
#'
#' @export
Authenticate.twitter2 <-
  function(socialmedia,
           consumerKey,
           consumerSecret,
           bearerToken,
           ...) {
    credential <-
      list(socialmedia = "twitter2",
           auth = NULL,
           bearer = NULL)
    class(credential) <-
      append(class(credential), c("credential", "twitter2"))

    if (missing(bearerToken)) {
      if (missing(consumerKey) || missing(consumerSecret)) {
        stop("Missing twitter2 consumer API keys or bearer token.", call. = FALSE)
      }

      rlang::check_installed("voson.tcn", "for Authenticate.twitter2")
      stop_req_pkgs(c("voson.tcn"), "Authenticate.twitter2")

      token <- voson.tcn::tcn_token(consumer_key = consumerKey,
                                    consumer_secret = consumerSecret)

      credential$bearer <- token$bearer

      return(credential)
    }

    if (!is.null(bearerToken)) {
      credential$bearer <- bearerToken
    } else {
      stop("Missing twitter2 consumer API bearer token.", call. = FALSE)
    }

    credential
  }
