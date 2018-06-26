## The AuthenticateWithTwitterAPI is not functional because it relies on a "side effect". It is a twitteR design problem.
## AuthenticateWithFacebookAPI can be fixed to make it functional.

## TODO: Maybe need to unify the variable names, currently there are:
### facebook: appID, appSecret, extended_permissions, useCachedToken
### twitter: api_key, api_secret, access_token, access_token_secret, createToken <- inconsistent?
### youtube: apiKeyYoutube <- inconsistent?
### instagram: appID, appSecret, useCachedToken

## Maybe make it consistent with only camel, as the rest of the package uses camel, not underscore. But hadleyverse packages usually use underscores:
## Therefore, unified variable names:
## appID, appSecret, apiKey, apiSecret, accessToken, accessTokenSecret, useCachedToken, extendedPermissions, createToken



#' Create credential to access social media APIs
#'
#' \code{Authenticate} creates a \code{credential} object that enables R to
#' make authenticated calls to social media APIs.  A \code{credential} object
#' is a S3 object with the authentication-related information such as access
#' tokens and the information on the social media that grant authentication.
#' \code{Authenticate} is the first step of the \code{Authenticate},
#' \code{Collect}, \code{Create} workflow.
#'
#'
#' @param socialmedia character string, social media API to authenticate,
#' currently supports "facebook", "youtube", "twitter" and "instagram"
#' @param ... additional parameters for authentication
#'
#' \code{facebook}: appID, appSecret
#'
#' \code{youtube}: apiKey
#'
#' \code{twitter}: apiKey, apiSecret, accessToken, accessTokenSecret
#'
#' \code{instagram}: appID, appSecret
#' @return credential object with authentication information
#' @note Currently, \code{Authenticate} with socialmedia = "twitter" generates
#' oauth information to be used in the current active session only (i.e.
#' "side-effect") and no authentication-related information will be stored in
#' the returned \code{credential} object.
#' @author Chung-hong Chan <chainsawtiney@@gmail.com>
#' @seealso \code{\link{AuthenticateWithFacebookAPI}},
#' \code{\link{AuthenticateWithInstagramAPI}},
#' \code{\link{AuthenticateWithYoutubeAPI}},
#' \code{\link{AuthenticateWithTwitterAPI}}, \code{\link{SaveCredential}},
#' \code{\link{LoadCredential}}
#' @examples
#'
#' \dontrun{
#' require(magrittr)
#' ## Instagram ego network example
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#' myUsernames <- c("senjohnmccain","obama")
#'
#' Authenticate("instagram",
#' appID = myAappId,
#' appSecret = myAppSecret) %>% Collect(ego = TRUE,
#' username = myUsernames) %>% Create
#'
#' ## YouTube actor network example
#' my_apiKeyYoutube <- "314159265358979qwerty"
#' videoIDs <- c("W2GZFeYGU3s","mL27TAJGlWc")
#'
#' Authenticate("youtube",
#' apiKey = my_apiKeyYoutube) %>% Collect(videoIDs = videoIDs) %>% Create('actor')
#' }
#' @export
Authenticate <- function(socialmedia, ...) {
    authenticator <- switch(tolower(socialmedia),
                            facebook = facebookAuthenticator,
                            youtube = youtubeAuthenticator,
                            twitter = twitterAuthenticator,
                            instagram = instagramAuthenticator,
                            stop("Unknown socialmedia")
                            )
    auth <- authenticator(...)
    credential <- list(socialmedia = tolower(socialmedia), auth = auth)
    class(credential) <- append(class(credential), "credential")
    return(credential)
}

### For the side effect of saving the credential into a file.
### Useful to cache the Credential to a file and then re-use it in the future session.
### i.e. Authenticate %>% SaveCredential %>% Collect
### and then, LoadCredential %>% Collect

#' Save and load credential information
#'
#' Functions to save and load credential information. Currently, credential
#' information will be stored as a RDS file. \code{SaveCredential} will return
#' the input \code{credential}, useful for working as a filter between the
#' \code{Authenticate} and \code{Collect}.
#'
#'
#' @aliases LoadCredential SaveCredential
#' @param credential \code{credential} object
#' @param filename character, filename to be saved to or restored from
#' @return \code{credential} object
#' @note \code{credential} created from \code{Authenticate} with socialmedia =
#' 'twitter' will not be saved by SaveCredential
#' @examples
#'
#' \dontrun{
#' require(magrittr)
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#' myUsernames <- c("senjohnmccain","obama")
#'
#' Authenticate("instagram",
#' appID = myAppId,
#' appSecret = myAppSecret) %>% SaveCredential("instagramCred.RDS") %>% Collect(ego = TRUE,
#' username = myUsernames) %>% Create
#'
#' ## Load the previously saved credential information
#' LoadCredential("instagramCred.RDS") %>% Collect(tag="obama",
#' distance=5000, n=100) %>% Create("bimodal")
#' }
#' @export
SaveCredential <- function(credential, filename = "credential.RDS") {
    if (credential$socialmedia == "twitter") {
        warning("Credential created for Twitter will not be saved.")
    } else {
        saveRDS(credential, filename)
    }
    return(credential)
}

#' @rdname SaveCredential
#' @export
LoadCredential <- function(filename = "credential.RDS") {
    credential <- readRDS(filename)
    return(credential)
}

### *Authenticator functions should not be exported. It is just a bunch of helper functions to bridge the AuthenticateWith* functions with Authenticate(), but with datasource as the first argument and always return an auth object

### As a convention, function starts with lower case shouldn't be exported.

youtubeAuthenticator <- function(apiKey) {
    return(AuthenticateWithYoutubeAPI(apiKey))
}

### Currently, this Authenticator will return nothing, only for its side effect
### SAD!!!!!!!!!!!!!!!!!!
### i.e. cannot use SaveCredential and LoadCredential!

twitterAuthenticator <- function(apiKey, apiSecret, accessToken, accessTokenSecret, createToken) {
    AuthenticateWithTwitterAPI(api_key = apiKey, api_secret = apiSecret, access_token = accessToken, access_token_secret = accessTokenSecret, createToken = createToken) # ah, only for its side effect, really bad design decision, twitteR!
    return(NULL)
}

facebookAuthenticator <- function(appID, appSecret, extendedPermissions = FALSE) {
    return(AuthenticateWithFacebookAPI(appID, appSecret, extended_permissions = extendedPermissions, useCachedToken = FALSE))
}

instagramAuthenticator <- function(appID, appSecret) {
    return(AuthenticateWithInstagramAPI(appID, appSecret))
}
