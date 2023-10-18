# twitter oauth R6 token class and methods from the ropensci rtweet v0.7.0 package on github
# https://github.com/ropensci/rtweet/blob/v0.7.0/R/tokens.R
# functions authored by @mkearney rtweet
twitter_init_oauth1.0 <- function (endpoint,
                                   app,
                                   permission = NULL,
                                   is_interactive = interactive(),
                                   private_key = NULL) {

  oauth_sig <- function(url,
                        method,
                        token = NULL,
                        token_secret = NULL,
                        private_key = NULL,
                        ...) {
    httr::oauth_header(
      httr::oauth_signature(
        url,
        method,
        app,
        token,
        token_secret,
        private_key,
        other_params = c(list(...),
                         oauth_callback = "http://127.0.0.1:1410")
      )
    )
  }

  response <-
    httr::POST(endpoint$request,
               oauth_sig(endpoint$request,
                         "POST", private_key = private_key))
  httr::stop_for_status(response)

  params <- httr::content(response, type = "application/x-www-form-urlencoded")
  token <- params$oauth_token
  secret <- params$oauth_token_secret

  authorize_url <- httr::modify_url(endpoint$authorize,
                                    query = list(oauth_token = token, permission = "read"))
  verifier <- httr::oauth_listener(authorize_url, is_interactive)
  verifier <- verifier$oauth_verifier %||% verifier[[1]]

  response <- httr::POST(
    endpoint$access,
    oauth_sig(
      endpoint$access,
      "POST",
      token,
      secret,
      oauth_verifier = verifier,
      private_key = private_key
    ),
    body = ""
  )

  httr::stop_for_status(response)
  httr::content(response, type = "application/x-www-form-urlencoded")
}

`%||%` <- function(a, b) {
  if (length(a) > 0)
    a
  else
    b
}

keep_last <- function(...) {
  x <- c(...)
  x[!duplicated(names(x), fromLast = TRUE)]
}

is_empty <- function(x)
  length(x) == 0

compact <- function(x) {
  empty <- vapply(x, is_empty, logical(1))
  x[!empty]
}

auth_request <-
  function(method = NULL,
            url = NULL,
            headers = NULL,
            fields = NULL,
            options = NULL,
            auth_token = NULL,
            output = NULL) {
    if (!is.null(method))
      stopifnot(is.character(method), length(method) == 1)
    if (!is.null(url))
      stopifnot(is.character(url), length(url) == 1)
    if (!is.null(headers))
      stopifnot(is.character(headers))
    if (!is.null(fields))
      stopifnot(is.list(fields))
    if (!is.null(output))
      stopifnot(inherits(output, "write_function"))
    structure(
      list(
        method = method,
        url = url,
        headers = keep_last(headers),
        fields = fields,
        options = compact(keep_last(options)),
        auth_token = auth_token,
        output = output
      ),
      class = "request"
    )
  }

twitter_Token1.0 <-
  R6::R6Class(
    "Token1.0",
    inherit = httr::Token,
    list(
      init_credentials = function(force = FALSE) {
        self$credentials <- twitter_init_oauth1.0(
          self$endpoint,
          self$app,
          permission = self$params$permission,
          private_key = self$private_key
        )
      },
      can_refresh = function() {
        FALSE
      },
      refresh = function() {
        stop("Not implemented")
      },
      sign = function(method, url) {
        oauth <- httr::oauth_signature(
          url,
          method,
          self$app,
          self$credentials$oauth_token,
          self$credentials$oauth_token_secret,
          self$private_key
        )
        if (isTRUE(self$params$as_header)) {
          c(auth_request(url = url), httr::oauth_header(oauth))
        } else {
          url <- httr::parse_url(url)
          url$query <- c(url$query, oauth)
          auth_request(url = httr::build_url(url))
        }
      }
    )
  )
