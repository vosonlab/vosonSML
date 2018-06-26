#' Populate Twitter networks with user information
#'
#' This function is used to 'populate' Twitter networks (generated
#' with the \code{\link{Create}} function) with information about
#' the users in the network. This involves calls to the Twitter API
#' to collect this information, which is then applied to the network
#' as vertex attributes.
#'
#' @param networkObject an igraph graph object created with \code{\link{Create}}
#' @return An igraph graph object
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{\link{Collect}}, \code{\link{Create}}
#' @examples
#'
#' \dontrun{
#' require(magrittr)
#' ## Get Twitter user information and apply to network
#' myTwitterNetwork_userInfo <- PopulateUserInfo(myTwitterNetwork)
#'
#' }
#' @export
PopulateUserInfo <- function(networkObject) {
  name=NULL # appease the gods of R CMD CHECK
  # This function is supposed to perform the lookups in batches
  # and mind the rate limit:
  getUserObjects <- function(users) {
    groups <- split(users, ceiling(seq_along(users)/50))
    userObjects <- ldply(groups, function(group) { # ldply is a very cool function, found in plyr package.
      objects <- lookupUsers(group, includeNA=TRUE)
      out <- twListToDF(objects) # twListToDF is also a handy function, found in twitteR package. Converts weird class object to data frame.
        # print("Waiting for 15 minutes (to 'refresh' the rate limit)...") # Don't need to use this yet. Implement later for number of users > 7500 (have to do chunked batches... chunks of chunks... urrghh)
        # Sys.sleep(900)
      return(out)
    })
    return(userObjects)
  }

  # get the list of users
  listOfUsers <- unique(V(networkObject)$name)
  listOfUsers_actual <- listOfUsers[-grep("^#",listOfUsers)]

  ## Test if it is a Twitter actor network
  if (length(listOfUsers_actual)<1) {

      # predefine a data table to store the results (later delete unneeded rows)
      actors <- data.table(
        name=as.character(c(rep("NA_f00",length(listOfUsers)))),
        userDescription=as.character(c(rep("NA_f00",length(listOfUsers)))),
        statusesCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
        followersCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
        favoritesCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
        friendsCount=as.character(c(rep("NA_f00",length(listOfUsers)))),
        url=as.character(c(rep("NA_f00",length(listOfUsers)))),
        realName=as.character(c(rep("NA_f00",length(listOfUsers)))),
        dateAccountCreated=as.character(c(rep("NA_f00",length(listOfUsers)))),
        userLocation=as.character(c(rep("NA_f00",length(listOfUsers)))),
        userLanguage=as.character(c(rep("NA_f00",length(listOfUsers)))),
        numberOfListsUserIsFeaturedOn=as.character(c(rep("NA_f00",length(listOfUsers)))),
        profileImageUrl=as.character(c(rep("NA_f00",length(listOfUsers))))
        )

        setkey(actors,name) # set the key value of the data table

        nextEmptyRow <- 1 # so we can update rows in `dataCombined` in a relatively efficient way

          # This function is supposed to perform the lookups in batches
          # and mind the rate limit:
          getUserObjects <- function(users) {
            groups <- split(users, ceiling(seq_along(users)/50))
            userObjects <- ldply(groups, function(group) { # ldply is a very cool function, found in plyr package.
              objects <- lookupUsers(group, includeNA=TRUE)
              out <- twListToDF(objects) # twListToDF is also a handy function, found in twitteR package. Converts weird class object to data frame.
                # print("Waiting for 15 minutes (to 'refresh' the rate limit)...") # Don't need to use this yet. Implement later for number of users > 7500 (have to do chunked batches... chunks of chunks... urrghh)
                # Sys.sleep(900)
              return(out)
            })
            return(userObjects)
          }

        # Collect user data (will return NA for users who don't exist)

          # query the user data
          cat("\n Fetching the user data...\n") # DEBUG
          usersInformationAttributes <- getUserObjects(listOfUsers)
          actorsInfoDF <- usersInformationAttributes

          actors$name <- actorsInfoDF$screenName
          actors$userDescription <- actorsInfoDF$description
          actors$statusesCount <- actorsInfoDF$statusesCount
          actors$followersCount <- actorsInfoDF$followersCount
          actors$favoritesCount <- actorsInfoDF$favoritesCount
          actors$friendsCount <- actorsInfoDF$friendsCount
          actors$url <- actorsInfoDF$url
          actors$realName <- actorsInfoDF$name
          actors$dateAccountCreated <- actorsInfoDF$created
          actors$userLocation <- actorsInfoDF$location
          actors$userLanguage <- actorsInfoDF$lang
          actors$numberOfListsUserIsFeaturedOn <- actorsInfoDF$listedCount
          actors$profileImageUrl <- actorsInfoDF$profileImageUrl

      # the final thing to do is apply the values in `actors` to the networkObject

      V(networkObject)$screenName <- actors$name
      V(networkObject)$userDescription <- actors$userDescription
      V(networkObject)$statusesCount <- actors$statusesCount
      V(networkObject)$followersCount <- actors$followersCount
      V(networkObject)$favoritesCount <- actors$favoritesCount
      V(networkObject)$friendsCount <- actors$friendsCount
      V(networkObject)$url <- actors$url
      V(networkObject)$realName <- actors$realName
      V(networkObject)$dateAccountCreated <- actors$dateAccountCreated
      V(networkObject)$userLocation <- actors$userLocation
      V(networkObject)$userLanguage <- actors$userLanguage
      V(networkObject)$numberOfListsUserIsFeaturedOn <- actors$numberOfListsUserIsFeaturedOn
      V(networkObject)$profileImageUrl <- actors$profileImageUrl

      return(networkObject)

  }

  ## Test if it is a Twitter bimodal network
  if (length(listOfUsers_actual)>=1) {

    # predefine a data table to store the results (later delete unneeded rows)
    actors <- data.table(
      name=as.character(c(rep(NA,length(listOfUsers)))),
      userDescription=as.character(c(rep(NA,length(listOfUsers)))),
      statusesCount=as.character(c(rep(NA,length(listOfUsers)))),
      followersCount=as.character(c(rep(NA,length(listOfUsers)))),
      favoritesCount=as.character(c(rep(NA,length(listOfUsers)))),
      friendsCount=as.character(c(rep(NA,length(listOfUsers)))),
      url=as.character(c(rep(NA,length(listOfUsers)))),
      realName=as.character(c(rep(NA,length(listOfUsers)))),
      dateAccountCreated=as.character(c(rep(NA,length(listOfUsers)))),
      userLocation=as.character(c(rep(NA,length(listOfUsers)))),
      userLanguage=as.character(c(rep(NA,length(listOfUsers)))),
      numberOfListsUserIsFeaturedOn=as.character(c(rep(NA,length(listOfUsers)))),
      profileImageUrl=as.character(c(rep(NA,length(listOfUsers))))
    )

      setkey(actors,name) # set the key value of the data table

      # query the user data
      cat("\n Fetching the user data...\n") # DEBUG
      usersInformationAttributes <- getUserObjects(listOfUsers_actual) # exclude hashtag vertices
      actorsInfoDF <- usersInformationAttributes
      actorsInfoDF_hashtag_NA <-

      # but this does not give us data for 'hashtag' type vertices (none exists of course)
      # so, need to fill in this information manually

      actors$name[1:length(listOfUsers_actual)] <- actorsInfoDF$screenName
      actors$userDescription[1:length(listOfUsers_actual)] <- actorsInfoDF$description
      actors$statusesCount[1:length(listOfUsers_actual)] <- actorsInfoDF$statusesCount
      actors$followersCount[1:length(listOfUsers_actual)] <- actorsInfoDF$followersCount
      actors$favoritesCount[1:length(listOfUsers_actual)] <- actorsInfoDF$favoritesCount
      actors$friendsCount[1:length(listOfUsers_actual)] <- actorsInfoDF$friendsCount
      actors$url[1:length(listOfUsers_actual)] <- actorsInfoDF$url
      actors$realName[1:length(listOfUsers_actual)] <- actorsInfoDF$name
      actors$dateAccountCreated[1:length(listOfUsers_actual)] <- actorsInfoDF$created
      actors$userLocation[1:length(listOfUsers_actual)] <- actorsInfoDF$location
      actors$userLanguage[1:length(listOfUsers_actual)] <- actorsInfoDF$lang
      actors$numberOfListsUserIsFeaturedOn[1:length(listOfUsers_actual)] <- actorsInfoDF$listedCount
      actors$profileImageUrl[1:length(listOfUsers_actual)] <- actorsInfoDF$profileImageUrl

      # the final thing to do is apply the values in `actors` to the networkObject

      V(networkObject)$screenName <- actors$name
      V(networkObject)$userDescription <- actors$userDescription
      V(networkObject)$statusesCount <- actors$statusesCount
      V(networkObject)$followersCount <- actors$followersCount
      V(networkObject)$favoritesCount <- actors$favoritesCount
      V(networkObject)$friendsCount <- actors$friendsCount
      V(networkObject)$url <- actors$url
      V(networkObject)$realName <- actors$realName
      V(networkObject)$dateAccountCreated <- actors$dateAccountCreated
      V(networkObject)$userLocation <- actors$userLocation
      V(networkObject)$userLanguage <- actors$userLanguage
      V(networkObject)$numberOfListsUserIsFeaturedOn <- actors$numberOfListsUserIsFeaturedOn
      V(networkObject)$profileImageUrl <- actors$profileImageUrl

      return(networkObject)

  }

}
