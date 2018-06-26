#' @export
CreateActorNetwork.twitter <-
function(x,writeToFile)
{

  from=retweet_from=to=edgeType=timeStamp=tweet_id=users_mentioned=reply_to=NULL # to please the gods of R CMD CHECK

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  df <- x # match the variable names (this must be used to avoid warnings in package compilation?)

  # if `df` is a list of dataframes, then need to convert these into one dataframe
  # CURRENTLY NOT IMPLEMENTED - there is no method for lists yet.
  # suppressWarnings(
  #   if (class(df)=="list") {
  #   df <- do.call("rbind", df)
  #   }
  # )

  # The `hashtags_used` column in `df` causes problems for creating actor network, so delete it:
  df <- df[,-21]

  # clear any odd characters
  # df <- removeOddChars(df)

  # convert df to data.table
  df <- data.table(df)

    # Now create the dfActorNetwork1, a dataframe of relations between users
    cat("Generating the network...\n")  ### DEBUG
    flush.console()

      # for speed we will pre-allocate `dataCombined` to a very large size (more rows than needed)
      # and after everything is finished we will delete the unused rows

      dataCombined <- data.table(
        from = as.character(c(rep("NA_f00",20000000))),
        to = as.character(c(rep("NA_f00",20000000))),
        edgeType = as.character(c(rep("NA_f00",20000000))),
        timeStamp = as.character(c(rep("NA_f00",20000000))),
        tweet_id = as.character(c(rep("NA_f00",20000000)))
        )

      setkey(dataCombined,from) # set the key value of the data table

      nextEmptyRow <- 1 # so we can update rows in `dataCombined` in a relatively efficient way

      # We firstly do the retweet data
      for (i in 1:nrow(df)) {

        if (is.na(df[i,retweet_from][[1]])) {next} # we check if there are retweets, if not skip to next row

          # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

          dataCombined[nextEmptyRow, from:= as.character(df$from_user[i][[1]])]
          dataCombined[nextEmptyRow, to := as.character(df$retweet_from[i][[1]])]
          dataCombined[nextEmptyRow, edgeType := as.character("Retweet")]
          dataCombined[nextEmptyRow, timeStamp := as.character(df$created_at[i][[1]])]
          dataCombined[nextEmptyRow, tweet_id := as.character(df$id[i][[1]])]

          nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

      }

      # Next we do the mentions
      for (i in 1:nrow(df)) {

        if (length(df[i,users_mentioned][[1]]) < 1) {next} # we check if there are likes, if not skip to next row

        for (j in 1:length(df$users_mentioned[i][[1]])){ # for each row of the likes data for post i

          # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

          dataCombined[nextEmptyRow, from := as.character(df$from_user[i][[1]])]
          dataCombined[nextEmptyRow, to := as.character(df$users_mentioned[i][[1]][j])]
          dataCombined[nextEmptyRow, edgeType := as.character("Mention")]
          dataCombined[nextEmptyRow, timeStamp := as.character(df$created_at[i][[1]])]
          dataCombined[nextEmptyRow, tweet_id := as.character(df$id[i][[1]])]

          nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

        }

      }

      # Finally, we do the replies data
      for (i in 1:nrow(df)) {

        if (is.na(df[i,reply_to][[1]])) {next} # we check if there are retweets, if not skip to next row

          # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

          dataCombined[nextEmptyRow, from:= as.character(df$from_user[i][[1]])]
          dataCombined[nextEmptyRow, to := as.character(df$reply_to[i][[1]])]
          dataCombined[nextEmptyRow, edgeType := as.character("Reply")]
          dataCombined[nextEmptyRow, timeStamp := as.character(df$created_at[i][[1]])]
          dataCombined[nextEmptyRow, tweet_id := as.character(df$id[i][[1]])]

          nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

      }

      # we now delete all the rows at the end of `dataCombined` that are unused
      dataCombined <- dataCombined[edgeType != "NA_f00"] # we just keep the rows that are unchanged from the original dummy data values

    ## --------------------------------

    # make a vector of all the unique actors in the network1
    # actorsNames <- unique(c(as.character(dataCombined$from),as.character(dataCombined$to)))
    actorsNames <- unique(factor(c(as.character(unique(dataCombined$from)),as.character(unique(dataCombined$to)))))

#
# # cat(actorsNames) # DEBUG
#
#     # Retrieve all the user details (e.g. follower count, number of tweets, etc) and include as node attributes.
#       # NOTE: Given API rate limits, the below implementation supports up to 7500 users overall in dataset (150 requests * 50 users per request).
#       # NOTE: Future work needs to address the Twitter API rate limit for looking up user information (150 requests per 15 minutes).
#       # NOTE: Requesting 50 users at a time seems to avoid rate limit errors (it's a safe bet...).
#
#         # This function is supposed to perform the lookups in batches
#         # and mind the rate limit:
#         getUserObjects <- function(users) {
#           groups <- split(users, ceiling(seq_along(users)/50))
#           userObjects <- ldply(groups, function(group) { # ldply is a very cool function, found in plyr package.
#             objects <- lookupUsers(group)
#             out <- twListToDF(objects) # twListToDF is also a handy function, found in twitteR package. Converts weird class object to data frame.
#               # print("Waiting for 15 minutes (to 'refresh' the rate limit)...") # Don't need to use this yet. Implement later for number of users > 7500 (have to do chunked batches... chunks of chunks... urrghh)
#               # Sys.sleep(900)
#             return(out)
#           })
#           return(userObjects)
#         }
#
#     # Putting it into action:
#     usersInformationAttributes <- getUserObjects(actorsNames)
#     actorsInfoDF <- usersInformationAttributes
#
#     # Need to clean the user text collected here (get rid of odd characters):
#     # actorsInfoDF <- RemoveOddCharsUserInfo(actorsInfoDF) # uses the new function in v2_munge_tweets.R
#
#     # We sometimes have a PROBLEM of missing actors (no info could be retrieved for them - might be misspellings/errors/pun or joke, etc)
#     # So, identify which users are missing from original set to retrieved set,
#     # then ensure these users/connections are removed before proceeding onwards:
#
#     missingActors <- setdiff(actorsNames,usersInformationAttributes$screenName)
#       # NOTE: This is a horrible approach, need to optimise.
#     missingTemp <- NULL # store the indexes of "offending" edge connections (i.e. bad/missing actors)
#       # NOTE: Obviously the 'offending' users can only be found in the 2nd column
#       # NOTE: Ipso facto, if they are not real/actual users, then they can't be the source of a directed edge
#
#     for (i in 1:length(missingActors)) {
#       missingTemp <- c(missingTemp, which(missingActors[i] == dataCombined$to))
#     }
#
#     # REMOVE the offendors:
#       if(length(missingTemp) > 0) {
#       dataCombined <- dataCombined[-missingTemp,]
#       }
#
#     # REMOVE any duplicated usernames in the retrieved user information (NOT SURE HOW/WHY THIS WOULD OCCUR **NEED TO CHECK**):
#     # duplicatedUsers <- which(duplicated(actorsInfoDF$screenName))
#
#     # if(length(duplicatedUsers) > 0) {
#     #   actorsInfoDF <- actorsInfoDF[-duplicatedUsers,]
#     # }
#
#     actors <- data.frame(
#       name=actorsInfoDF$screenName,
#       userDescription=actorsInfoDF$description,
#       statusesCount=actorsInfoDF$statusesCount,
#       followersCount=actorsInfoDF$followersCount,
#       favoritesCount=actorsInfoDF$favoritesCount,
#       friendsCount=actorsInfoDF$friendsCount,
#       url=actorsInfoDF$url,
#       realName=actorsInfoDF$name,
#       dateAccountCreated=actorsInfoDF$created,
#       userLocation=actorsInfoDF$location,
#       userLanguage=actorsInfoDF$lang,
#       numberOfListsUserIsFeaturedOn=actorsInfoDF$listedCount,
#       profileImageUrl=actorsInfoDF$profileImageUrl
#       )
#
#     # actors <- actors[-which(duplicated(actors$name)),]
#     # actors <- unique(actors)
#
#     # make a dataframe of the relations between actors
#       # NOTE - FUTURE WORK: include edge attributes to specify the specific type of "mentions" (see previous comments on temporal network problem (see: approx. LINES 113-116)).
#       # NOTE - For example, "RETWEET" versus "TWEET TO" (@username specified beginning of tweet) versus "MENTION" (@username specified somewhere else in tweet text)
#
#     # return(df) # DEBUG

    relations <- data.frame(
      from=dataCombined$from,
      to=dataCombined$to,
      edgeType=dataCombined$edgeType,
      timeStamp=dataCombined$timeStamp,
      tweet_id=dataCombined$tweet_id)

    ##### STEP FOUR #####
# cat("\n I got to the final step before network generation")

    # convert into a graph
    # note: suppressing warnings is used to avoid this error:
    #     In if (class(newval) == "factor") { :
    #     the condition has length > 1 and only the first element will be used

  suppressWarnings(
    g <- graph.data.frame(relations, directed=TRUE, vertices=actorsNames) # used to be vertices=actors (when it collected user data)
  )

    # Make the node labels play nice with Gephi
    V(g)$label <- V(g)$name

    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
      # Output the final network to a graphml file, to import directly into Gephi
      currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
      currTime <- gsub(":","_",currTime)
      write.graph(g,paste0(currTime,"_TwitterActorNetwork.graphml"),format="graphml")
      cat("Twitter actor network was written to current working directory, with filename:\n")
      cat(paste0(currTime,"_TwitterActorNetwork.graphml"))
    }

    cat("\nDone.\n") ### DEBUG
    flush.console()

    return(g)

}
