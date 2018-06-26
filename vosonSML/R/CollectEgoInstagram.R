#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Collect} function
#'
#' Collect Instagram data for generating ego network
#'
#' This function collects data for creating a (weighted and directed) 'ego'
#' network from a given set of seed users (the ego nodes). Note! The network
#' size can become extremely large very quickly, depending on the arguments the
#' user provides to this function. For example, specifying degreeEgoNet=2 and
#' getFollows=TRUE can generate very large networks from just a small number of
#' ego users (even just 3 or 4 ego nodes).
#'
#'
#' @param username character vector, specifying a set of usernames who will be
#' the 'ego' nodes for the network generation.
#' @param userid character vector, specifying a set of usernames who will be
#' the 'ego' nodes for the network generation.
#' @param verbose logical. If TRUE then this function will output runtime
#' information to the console as it computes. Useful diagnostic tool for long
#' computations. Default is FALSE.
#' @param degreeEgoNet Numeric, the 'order' or 'degree' of the ego-network to
#' create (1 is default). 1 = ego + alters. 2 = ego + alters + alters of
#' alters.
#' @param waitForRateLimit logical. If TRUE then it will try to observe the API
#' rate limit by ensuring that no more than 5000 API calls are made per hour
#' (the current rate limit). If more than 5000 calls are made within a 60
#' minute window, then all operates will suspend for 60 minutes, and resume
#' afterwards. Note: API calls are only tracked within the scope of this
#' function.
#' @param getFollows Logical, if TRUE (default), also collect who each 'ego'
#' node itself follows (i.e. not just the followers of ego), and integrate
#' these data into the ego network.
#' @param credential credential object. Credential information to be use with
#' API calls. Default to NULL, meaning using the API credential information
#' store in the current workspace.
#' @return A data frame object of class \code{dataSource.instagram.ego} that
#' can be used with \code{Create}.
#' @author Timothy Graham <timothy.graham@@anu.edu.au>, Robert
#' Ackland<robert.ackland@@anu.edu.au> & Chung-hong Chan
#' <chainsawtiney@@gmail.com>
#' @examples
#'
#' \dontrun{
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#'
#' # Authenticate with the Instagram API using `AuthenticateWithInstagramAPI`
#' instagram_oauth_token <- AuthenticateWithInstagramAPI(appID=myAppI,
#' appSecret=myAppSecret, useCachedToken=TRUE)
#'
#' myUsernames <- c("senjohnmccain","obama")
#' instagramEgodata <- CollectEgoInstgram(username=myUsernames,
#' verbose=TRUE,degreeEgoNet=1, waitForRateLimit=TRUE,
#' getFollows=FALSE)
#' Create(instagramEgodata)
#' }
#' @export
CollectEgoInstagram <-
function(username, userid, verbose, degreeEgoNet, waitForRateLimit, getFollows, credential = NULL)
{

  egoName = profile_picture = full_name = id = . = NULL # appease the gods of R CMD CHECK --as-cran
  if(!(exists("instagram_oauth_token")) & is.null(credential)) {
      instagram_oauth_token <- NULL
  }
  ## not using side effect
  if (!is.null(credential)) {
      instagram_oauth_token <- credential$auth
  }

  # if (missing(token)){
  #  cat("Please specify a valid API token (missing `token` argument in function call).\n")
  #  return()
  # }
  # token <- instagram_oauth_token


  if (missing(verbose)) {
    verbose <- TRUE
  }

  ## if (missing(writeToFile)) {
  ##   writeToFile <- FALSE # default = not write to file
  ## }

  if (missing(username) & missing(userid)) {
    cat("\nPlease specify either a character vector of username or numeric vector of userid. Exiting now... \n")
    return()
  }

  if (!missing(username)) { # if user has specified username, then we make the userid NULL (they can't have both)
    cat("\n I found a username argument.") # DEBUG
    userid <- NULL
  }

  if (missing(username) & (!missing(userid) & !is.null(userid))) { # if user has NOT specified usernames, but HAS specified userids
    cat("\n I did not find a username argument, but I found a userid argument.") # DEBUG
    userid <- as.vector(userid) # coerce to vector as a matter of course
    username <- userid # what we will do is simply use the one variable `username`, but run different code if character (username argument) or numeric (userid argument)
  }

  if (missing(degreeEgoNet)) {
    degreeEgoNet <- 1 # default to 1-degree ego network
  }

  if (missing(getFollows)) {
    getFollows <- FALSE # default to not collecting follows data (can become very huge network very quickly...)
  }

  if (missing(waitForRateLimit)) {
    waitForRateLimit <- FALSE # the default is NOT to wait for the rate limit. This will probably result in an error if the rate limit is maxed out.
  }

  ########################
  # Start data collection
  rateLimitHourTimer <- proc.time() # start the 60 minute timer (for minding the rate limit if waitForRateLimit==TRUE)
  totalCallsToAPI <- 0

  # if the usernames are numeric ids, then for simplicity we just query the usernames and use those instead
  usernamesCharacter <- vector(mode="character", length=length(username))
  if (is.numeric(username)) {
    cat(paste0("\n Matching user ids to usernames..."))
    for (i in 1:length(username)) {

      if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
        cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
      }

      # stop it at 4990, just shy of 5000 (margin of error)
      if (waitForRateLimit & totalCallsToAPI==4990 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
        cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
        Sys.sleep(3600)
        #cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
        # Sys.sleep(120) # DEBUG
        cat("Waking up now! Back to work...\n")
        totalCallsToAPI <- 0 # reset number of calls
        rateLimitHourTimer <- proc.time() # reset hourly timer
      }

      possibleError <- tryCatch({
        tempDF <- getUser(userid = username[i], token = instagram_oauth_token)
        totalCallsToAPI <- totalCallsToAPI + 1
        usernamesCharacter[i] <- tempDF$username
      },
      error=function(e) e
      )
      if(inherits(possibleError, "error")) {
        cat(paste0("\n I caught an error (user id possibly doesn't exist?)"))
        next
      }

    }
    username <- usernamesCharacter # now swap the numeric ids for the character string usernames
  }

  if (length(username)==1) {
    username <- as.vector(username, mode="character") # if character then ensure it is a vector with length 1 (coerce it)
  }

  ## DEBUG
  cat(paste0("\n list of usernames: ", username,"\n")) # DEBUG

  dataCombined <- data.table(
      egoName = rep("foo",length(username)),
      username = rep("foo",length(username)),
      bio = rep("foo",length(username)),
      website = rep("foo",length(username)),
      profile_picture = rep("foo",length(username)),
      full_name = rep("foo",length(username)),
      id = 10000000000000000000
  )

  usernameNoConflict <- username # avoid the variable name conflict when setting values of rows in data.table in next lines of code
  for (i in 1:length(username)) {
    dataCombined[i, username := usernameNoConflict[i]]
    dataCombined[i, egoName := usernameNoConflict[i]]
  }

  ## STEP 1 - this is for degreeEgoNet==1
  ## Get the followers of the ego node(s)

  for (i in 1:length(username)) {

    if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
      cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
    }

    # stop it at 4990, just shy of 5000 (margin of error)
    if (waitForRateLimit & totalCallsToAPI==4990 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
      cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
      Sys.sleep(3600)
      #cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
      # Sys.sleep(120) # DEBUG
      cat("Waking up now! Back to work...\n")
      totalCallsToAPI <- 0 # reset number of calls
      rateLimitHourTimer <- proc.time() # reset hourly timer
    }

    possibleError <- tryCatch({
      cat(paste0("\nI am getting followers for ",username[i],".\n")) # DEBUG

        # obama_info <- getUser("obama", instagram_oauth_token) # DEBUG
        # cat(paste0("\nNumber of rows in obama info: ",nrow(obama_info),"\n")) # DEBUG

      tempData <- getFollowers(username[i], instagram_oauth_token)
        # cat(paste0("\nNumber of rows in tempData: ",nrow(tempData),"\n")) # DEBUG
        # cat(print(tempData)) # DEBUG
      totalCallsToAPI <- totalCallsToAPI + 1
    },
    error=function(e) e
    )
    if(inherits(possibleError, "error")) {
      cat(paste0("\n I caught an error (possibly no followers?)"))
      next
    }
    tempData$egoName <- rep(username[i],nrow(tempData))
    tempData <- data.table(tempData)
    dataCombined <- rbind(dataCombined, tempData)
  }

  # now delete dummy row of data
  # dataCombined <- dataCombined[profile_picture != "foo"] # we just keep the rows that are unchanged from the original dummy data values

  # we also want to specify in the "ringset" column that these are in the first ringset
  dataCombined$ringset <- "First_Ringset"

  ## STEP 2 - this is for degreeEgoNet==2
  ## Get the followers of the alters of ego (friends of friends of ego node(s))

  if (degreeEgoNet > 1) {

      # create a temporary data.table to store the data in
      dataCombinedTEMP <- data.table(
        egoName = "foo",
        username = "foo",
        bio = "foo",
        website = "foo",
        profile_picture = "foo",
        full_name = "foo",
        id = 10000000000000000000
      )

      for (i in 1:nrow(dataCombined)) {

        if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
          cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
        }

        # stop it at 4990, just shy of 5000 (margin of error)
        if (waitForRateLimit & totalCallsToAPI==4990 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
          cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
          Sys.sleep(3600)
          #cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
          # Sys.sleep(120) # DEBUG
          cat("Waking up now! Back to work...\n")
          totalCallsToAPI <- 0 # reset number of calls
          rateLimitHourTimer <- proc.time() # reset hourly timer
        }

        possibleError <- tryCatch({
          tempData <- getFollowers(dataCombined$username[i], instagram_oauth_token)
          totalCallsToAPI <- totalCallsToAPI + 1
        },
        error=function(e) e
        )
        if(inherits(possibleError, "error")) {
          cat(paste0("\n I caught an error (possibly no followers?)"))
          next
        }
        tempData$egoName <- rep(dataCombined$username[i],nrow(tempData))
        tempData <- data.table(tempData)
        dataCombinedTEMP <- rbind(dataCombinedTEMP, tempData)
      }

      # now delete dummy row of data
      dataCombinedTEMP <- dataCombinedTEMP[profile_picture != "foo"] # we just keep the rows that are unchanged from the original dummy data values

      # we want to specify in the "ringset" column that these nodes are in the second ringset (followers of followers of ego)
      dataCombinedTEMP$ringset <- "Second_Ringset"

  }
  # now we want to create combine the dataframes
  if (degreeEgoNet > 1) {
    dataCombinedFinal <- rbind(dataCombined,dataCombinedTEMP)
  }
  if (degreeEgoNet==1) {
    dataCombinedFinal <- dataCombined # we just use the original dataframe and rename it, to work nicely with code below
  }

  ## STEP 3
  ## now we also get the 'follows' of the ego nodes (and their alters if degree == 2)

  if (getFollows) {

        dataCombinedFollows <- data.table(
          egoName = rep("foo",length(username)),
          username = rep("foo",length(username)),
          bio = rep("foo",length(username)),
          website = rep("foo",length(username)),
          profile_picture = rep("foo",length(username)),
          full_name = rep("foo",length(username)),
          id = 10000000000000000000
        )

        #for (i in 1:length(username)) {
        #  dataCombinedFollows[i, username := usernameNoConflict[i]]
        #  dataCombinedFollows[i, egoName := usernameNoConflict[i]]
        #}

        ## STEP 3.1 - for degreeEgoNet==1
        ## Get the follows of the ego node(s)

        for (i in 1:length(username)) {

          if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
            cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
          }

          # stop it at 4990, just shy of 5000 (margin of error)
          if (waitForRateLimit & totalCallsToAPI==4990 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
            cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
            Sys.sleep(3600)
            #cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
            # Sys.sleep(120) # DEBUG
            cat("Waking up now! Back to work...\n")
            totalCallsToAPI <- 0 # reset number of calls
            rateLimitHourTimer <- proc.time() # reset hourly timer
          }

          possibleError <- tryCatch({
            tempData <- getFollows(username[i], instagram_oauth_token)
            totalCallsToAPI <- totalCallsToAPI + 1
          },
          error=function(e) e
          )
          if(inherits(possibleError, "error")) {
            cat(paste0("\n I caught an error (possibly follows nobody?)"))
            next
          }
          tempData$egoName <- rep(username[i],nrow(tempData))
          tempData <- data.table(tempData)
          dataCombinedFollows <- rbind(dataCombinedFollows, tempData)
        }

        # now delete dummy row of data
        dataCombinedFollows <- dataCombinedFollows[profile_picture != "foo"] # we just keep the rows that are unchanged from the original dummy data values

        # we also want to specify in the "ringset" column that these are in the first ringset
        dataCombinedFollows$ringset <- "First_Ringset"

        ## STEP 3.2 - for degreeEgoNet > 1
        ## Get the follows of the alters of ego (friends of friends of ego node(s))

        if (degreeEgoNet > 1) { # we only run this if the desired degree of the egonet is greater than 1 (e.g. 1.5 or 2 degree egonet)

            # create a temporary data.table to store the data in
            dataCombinedFollowsTEMP <- data.table(
              egoName = "foo",
              username = "foo",
              bio = "foo",
              website = "foo",
              profile_picture = "foo",
              full_name = "foo",
              id = 10000000000000000000
            )

            for (i in 1:nrow(dataCombinedFollows)) {

              if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
                cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
              }

              if (waitForRateLimit & totalCallsToAPI==4999 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
                cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
                Sys.sleep(3600)
                #cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
                # Sys.sleep(120) # DEBUG
                cat("Waking up now! Back to work...\n")
                totalCallsToAPI <- 0 # reset number of calls
                rateLimitHourTimer <- proc.time() # reset hourly timer
              }

              possibleError <- tryCatch({
                tempData <- getFollows(dataCombinedFollows$username[i], instagram_oauth_token)
                totalCallsToAPI <- totalCallsToAPI + 1
              },
              error=function(e) e
              )
              if(inherits(possibleError, "error")) {
                cat(paste0("\n I caught an error (possibly follows nobody?)"))
                next
              }
              tempData$egoName <- rep(dataCombinedFollows$username[i],nrow(tempData))
              tempData <- data.table(tempData)
              dataCombinedFollowsTEMP <- rbind(dataCombinedFollowsTEMP, tempData)
            }

            # now delete dummy row of data
            dataCombinedFollowsTEMP <- dataCombinedFollowsTEMP[profile_picture != "foo"] # we just keep the rows that are unchanged from the original dummy data values

            # we want to specify in the "ringset" column that these nodes are in the second ringset (followers of followers of ego)
            dataCombinedFollowsTEMP$ringset <- "Second_Ringset"

        }
            # now we want to create combine the dataframes

        if (degreeEgoNet > 1) {
          dataCombinedFollowsFinal <- rbind(dataCombinedFollows,dataCombinedFollowsTEMP)
        }
        if (degreeEgoNet==1) {
          dataCombinedFollowsFinal <- dataCombinedFollows # we just use the original data.table from step 3.1 and rename it, to work nicely with code below
        }

        # the egoName and username columns are back to front compared to the 'followers' data
        # (i.e. this time the ego nodes are actually sending directed ties *to* the nodes in username column)
        # so we need to switch this around:

        dataCombinedFollowsFinal_egoName <- dataCombinedFollowsFinal[,egoName]
        dataCombinedFollowsFinal_username <- dataCombinedFollowsFinal[,username]
        dataCombinedFollowsFinal[,egoName:=dataCombinedFollowsFinal_username]
        dataCombinedFollowsFinal[,username:=dataCombinedFollowsFinal_egoName]

        #setnames(dataCombinedFollowsFinal,"egoName","username1")
        #setnames(dataCombinedFollowsFinal,"username","egoName1")
        #setnames(dataCombinedFollowsFinal,"egoName1","egoName")
        #setnames(dataCombinedFollowsFinal,"username1","username")

        #setcolorder(dataCombinedFollowsFinal, c("egoName","username","bio","website","profile_picture","full_name","id","ringset"))

        rm(dataCombinedFollowsFinal_username)
        rm(dataCombinedFollowsFinal_egoName)

  }

  # we combine the 'followers' and 'follows' data.tables (if needed)
  if (getFollows) {
    dataCombinedFinal <- rbind(dataCombinedFinal, dataCombinedFollowsFinal)
  }
  class(dataCombinedFinal) <- append(class(dataCombinedFinal),c("dataSource","instagram","ego"))
  return(dataCombinedFinal)
}
