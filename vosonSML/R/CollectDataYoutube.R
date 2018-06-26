#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Collect} function
#'
#' Collect YouTube comments data for generating different types of networks
#'
#' This function collects YouTube comments data for one or more YouTube videos.
#' It structures the data into a data frame of class \code{dataSource.youtube},
#' ready for creating networks for further analysis.
#'
#' \code{CollectDataYoutube} collects public comments from YouTube videos,
#' using the YouTube API.
#'
#' The function then finds and maps the relationships of YouTube users who have
#' interacted with each other (i.e. user i has replied to user j or mentioned
#' user j in a comment) and structures these relationships into a data frame
#' format suitable for creating unimodal networks (\code{CreateActorNetwork}).
#'
#' For multiple videos, the user may wish to use the function
#' \code{GetYoutubeVideoIDs}, which creates a character vector of video IDs
#' from a plain text file of YouTube video URLs, which can then be used for the
#' \code{videoIDs} argument of the function \code{CollectDataYoutube}.
#'
#' @param videoIDs character vector, specifying one or more YouTube video IDs.
#' For example, if the video URL is
#' 'https://www.youtube.com/watch?v=W2GZFeYGU3s', then use
#' \code{videoIDs='W2GZFeYGU3s'}. For multiple videos, the function
#' \code{GetYoutubeVideoIDs} can be used to create a vector object suitable as
#' input for \code{videoIDs}.
#' @param apiKeyYoutube character string, specifying the Google Developer API
#' Key used for authentication.
#' @param verbose logical. If \code{TRUE} then this function will output
#' runtime information to the console as it computes. Useful diagnostic tool
#' for long computations. Default is \code{FALSE}.
#' @param writeToFile logical. If \code{TRUE} then the data is saved to file in
#' current working directory (CSV format), with filename denoting current
#' system time. Default is \code{FALSE}.
#' @param maxComments numeric integer, specifying how many 'top-level' comments
#' to collect from each video. This value *does not* take into account 'reply'
#' comments (i.e. replies to top-level comments), therefore the total number of
#' comments collected may be higher than \code{maxComments}. By default this
#' function attempts to collect all comments.
#' @return A data frame object of class \code{dataSource.youtube} that can be
#' used for creating unimodal networks (\code{CreateActorNetwork}).
#' @note Currently supported network types:
#'
#' - unimodal 'actor' network; \code{CreateActorNetwork}
#'
#' Data generated using this function is *not* suitable for dynamic networks.
#' Dynamic YouTube comments networks are not currently implemented in the
#' vosonSML package. This will be implemented in a future release.
#'
#' Note on \code{maxComments} argument: Due to quirks/specifications of the
#' Google API, it is currently not possible to specify the exact number of
#' comments to return from the API using \code{maxResults} argument (i.e.
#' including comments that are replies to top-level comments). Therefore, the
#' number of comments collected is usually somewhat greater than
#' \code{maxComments}, if a value is specified for this argument. For example,
#' if a video contains 10 top-level comments, and one of these top-level
#' comments has 5 'child' or reply comments, then the total number of comments
#' collected will be equal to 15. Currently, the user must 'guesstimate' the
#' \code{maxResults} value, to collect a number of comments in the order of
#' what they require.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithYoutubeAPI} must be run first or no data will
#' be collected.
#' @keywords youtube data mining SNA
#' @examples
#'
#' \dontrun{
#'   # Use your own Google Developer API Key here:
#'   myApiKey <- "1234567890"
#'
#'   # Authenticate with the Google API
#'   apiKeyYoutube <- AuthenticateWithYoutubeAPI(apiKeyYoutube=myApiKey)
#'
#'   # Generate a vector of YouTube video IDs to collect data from
#'   # (or use the function `GetYoutubeVideoIDs` to automatically
#'   # generate from a plain text file of video URLs)
#'   videoIDs <- c("W2GZFeYGU3s","mL27TAJGlWc")
#'
#'   # Collect the data using function `CollectDataYoutube`
#'   myYoutubeData <- CollectDataYoutube(videoIDs,apiKeyYoutube,writeToFile=FALSE)
#'
#'   # Create an 'actor' network using the function `CreateActorNetwork`
#'   g_actor_youtube <- CreateActorNetwork(myYoutubeData)
#' }
#' @export
CollectDataYoutube <-
function(videoIDs, apiKeyYoutube, verbose, writeToFile, maxComments) {

  # with(list(videoIDs, apiKeyYoutube, verbose, writeToFile), {

  # cat(paste("DEBUG - numTweets is set to:", numTweets)) # DEBUG

  # handle the arguments

  if (missing(verbose)) {
    verbose <- FALSE # default to not verbose
  }

  if (missing(maxComments)) {
    maxComments <- 10000000000000 # some arbitrary very large number
  }

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE ) {
    verbose <- TRUE
  }
  else {verbose <- FALSE}

  if (missing(apiKeyYoutube)) {
    cat("Error. Argument `apiKeyYoutube` is missing. Please specify a valid API key to collect data (i.e. your Google Developer API Key).\n")
    return(NA)
  }

  # Ensure that argument `pageName` has been specified by user.

  if (missing(videoIDs)) {
    cat("Error. Argument `videoIDs` is missing.\nPlease specify a vector of video IDs to collect data from.\n Hint: to do this you can use the `GetYoutubeVideoIDs` function in this package.")
    return(NA)
  }

  apiKey <- apiKeyYoutube # to play nice with existing code

  # Start data collection

      # Create a dataframe to iteratively store comments from all the videos
      # that the user wants to scrape (i.e. specified in videoIDs)
      # DEBUG - uses 'dummy' data in first row (which is removed later)

      dataCombined <- data.frame(
        Comment = "foo",
        User = "bar",
        ReplyCount = "99999999",
        LikeCount = "99999999",
        PublishTime = "timestamp",
        CommentId = "99999999123456789",
        ParentID = "foobar",
        ReplyToAnotherUser = "FALSE",
        VideoID = "foobarfoobar",
        stringsAsFactors=FALSE)

      # Iterate through the videos in videoIDs, adding to dataCombined.

      for (k in 1:length(videoIDs)) {
        if (verbose) {
          cat(paste0("\nNow scraping video number: ",k," (out of ",length(videoIDs)," videos in total).",sep="")) # DEBUG
          flush.console() # DEBUG
          if(maxComments != 10000000000000) { # i.e. the default value
            cat(paste0("\nScraping a maximum of ",maxComments," comments for each video.\n")) # DEBUG
          }
        }

          ############################## Begin scraping comments #############################

          rObj <- yt_scraper(videoIDs,apiKey,k)

          rObj$scrape_all(maxComments)

          ## Make a dataframe out of the results

          tempData <- lapply(rObj$data, function(x) {
            data.frame(
              Comment = x$snippet$topLevelComment$snippet$textDisplay,
              User = x$snippet$topLevelComment$snippet$authorDisplayName,
              ReplyCount = x$snippet$totalReplyCount,
              LikeCount = x$snippet$topLevelComment$snippet$likeCount,
              PublishTime = x$snippet$topLevelComment$snippet$publishedAt,
              CommentId = x$snippet$topLevelComment$id,
              ParentID = "None",
              ReplyToAnotherUser = "FALSE",
              VideoID = videoIDs[k], # actual reference to API data is: x$snippet$topLevelComment$snippet$videoIDs[k],
              stringsAsFactors=FALSE)
          })

          core_df <- do.call("rbind", tempData)

          ############################## Scrape the comment replies #############################
          if (verbose) {
            cat(".") # DEBUG # UI FEEDBACK - I AM STILL ALIVE AND WORKING
            flush.console() # DEBUG
          }

          commentIDs <- core_df$CommentId

          base_url <- "https://www.googleapis.com/youtube/v3/comments"

          # 'dummy' first row of dataframe, for DEBUG purposes (fix later.....)
          dataRepliesAll <- data.frame(
            Comment = "foo",
            User = "bar",
            ReplyCount = "99999999",
            LikeCount = "99999999",
            PublishTime = "timestamp",
            CommentId = "99999999123456789",
            ParentID = "foobar",
            ReplyToAnotherUser = "FALSE",
            VideoID = videoIDs[k], # API DOESN'T SEEM TO RETURN HERE, no matter anyway
            stringsAsFactors=FALSE)

          for (i in 1:length(commentIDs)) {

                    cat(".") # DEBUG # UI FEEDBACK - I AM STILL ALIVE AND WORKING
                    flush.console() # DEBUG

            api_opts <- list(
              part = "snippet",
              textFormat = "plainText",
              parentId=commentIDs[i],
              key = apiKey
            )

            init_results <- httr::content(httr::GET(base_url, query = api_opts)) # TODO: should die when there is error

            tempDataReplies <- lapply(init_results$items, function(x) {
              data.frame(
                Comment = x$snippet$textDisplay,
                User = x$snippet$authorDisplayName,
                ReplyCount = 0, # there is no ReplyCount returned for replies (API specs)
                LikeCount = x$snippet$likeCount,
                PublishTime = x$snippet$publishedAt,
                CommentId = x$id,
                ParentID = x$snippet$parentId,
                ReplyToAnotherUser = "FALSE",
                VideoID = videoIDs[k], # API DOESN'T SEEM TO RETURN HERE, not that it matters
                stringsAsFactors=FALSE)

                    #cat(".") # DEBUG # UI FEEDBACK - I AM STILL ALIVE AND WORKING
                    #flush.console() # DEBUG

            })

            tempDataRepliesBinded <- do.call("rbind", tempDataReplies)

            dataRepliesAll <- rbind(dataRepliesAll,tempDataRepliesBinded)
          }

          ########### Combine comments + replies,
          # get rid of "dummy" first row
          dataRepliesAll <- dataRepliesAll[-1,]

          # combine the comments and replies dataframes
          dataCombinedTemp <- rbind(core_df,dataRepliesAll)

          # APPEND TO THE OVERALL DATAFRAME (I.E. MULTIPLE VIDEO COMMENTS)
          dataCombined <- rbind(dataCombined,dataCombinedTemp)

    }

      # Now we have all the comments/replies, from all videos!
      if (verbose) {
        cat("\nNow cleaning and structuring data... please be patient...\n") # DEBUG
        flush.console() # DEBUG
      }
      # Remove 'dummy' first row
    dataCombined <- dataCombined[-1,]
    
    ## Throw Error when no comment can be collected
    
    if (nrow(dataCombined) == 0) {
        stop("No comment can be collected from the given videoIDs.")
    }
    
    # REMOVE COMMENTS WITH NO TEXT
    # Note: this often occurs because users shared the video on Google+, but didn't write a comment.

            # DEBUG - SEE WHAT HAPPENS WHEN WE JUST LEAVE EMPTY COMMENTS IN THE DATA
              #toDel <- which(dataCombined$Comment=="")
              #if (length(toDel)!=0) {
              #  dataCombined <- dataCombined[-toDel,]
              #}

      # Clean away any junk characters
      # USE THIS IF ANY CHARACTER ENCODING PROBLEMS! BAD FOR CHINESE TEXT!?
        #TrimOddChar <- function(x) {
        #  iconv(x, to = 'UTF-8')
        #}
        #dataCombined[,1] <- TrimOddChar(dataCombined[,1])
        #dataCombined[,2] <- TrimOddChar(dataCombined[,2])
        #dataCombined[,8] <- TrimOddChar(dataCombined[,8])

      ########### Map relations between users into dataframe ################

      ## For each commentsDataNames element, if any commentTexts elements pattern matches
      ## with a commentsDataNames element, then it is a reply/mention:

      # isReplyToAnotherUser <- c(rep("FALSE",length(dataCombined$Comment)))
        isReplyToAnotherUser <- dataCombined[,8]

          ### !!!!! Escape any punctuation characters in names when using GREP!!!
          ## From: http://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
          ## This uses an R implementation of Perl's `quotemeta` function

          usernamesCleaned <- dataCombined$User # vector of user names (speed + readability)
          commentsTextCleaned <- dataCombined$Comment # duplicate of comment text data (speed + readability)
          #usernamesNotCleaned <- dataCombined$User # vector of original user names (pass to function)

          # This function is from the library("Hmisc")
          usernamesCleaned <- escapeRegex(usernamesCleaned)
          #commentsTextCleaned <- escapeRegex(commentsTextCleaned)
                          #
                          # for (i in 1:length(commentsTextCleaned)){
                          #     for (j in 1:length(usernamesCleaned)) {
                          #         matchTemp <- grep(paste("(\\+|\\@)", usernamesCleaned[j],  sep=""),commentsTextCleaned[i]) # !! ensure it's a username match (i.e. preceded by a "+" symbol)
      # OLD WAY:          #
                          #       if (length(matchTemp)>0) {
                          #         isReplyToAnotherUser[i] <- unique(dataCombined$User)[j] # we want the ORIGINAL (non-cleaned) username here!
                          #       }
                          #     }
                          # }
                          #
                          # # Replace the vector in dataframe with computed values
                          # dataCombined$ReplyToAnotherUser <- isReplyToAnotherUser
                          #

                                        # NEW WAY (OPTIMISED - better, faster, stronger...)
      dataCombined$ReplyToAnotherUser <- searchCommentsForMentions(commentsTextCleaned,usernamesCleaned)

          # print(dataCombined) # DEBUG
          # return(dataCombined) # DEBUG

      ## Map the comment replies within PARENT COMMENT THREADS into dataframe

      parentsTemp <- which(dataCombined[,7]!="None" & dataCombined[,8]=="FALSE")

      if (length(parentsTemp) != 0) {
        for (i in 1:nrow(dataCombined[parentsTemp,])) {

          tempMatch <- which(dataCombined[parentsTemp[i],7]==dataCombined[,6])[1] # take the 1st match - we could try to scrape MULTIPLE REPLIES/MENTIONS, but would require re-think.
          dataCombined[parentsTemp[i],8] <- dataCombined[tempMatch,2]

        }
      }

       if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
         currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
         currTime <- gsub(":","_",currTime)
         write.csv(dataCombined,paste0(currTime,"_YoutubeData.csv"))
         cat("YouTube data was written to current working directory, with filename:\n")
         cat(paste0(currTime,"_YoutubeData.csv"))
       }

      if (verbose) {
        cat("\nDone!\n")
      }

  ################################################
  # return dataframe to environment

  class(dataCombined) <- append(class(dataCombined),c("dataSource","youtube"))

  cat("\n")

  return(dataCombined)

  ################################################

  #}
  #)
}

## Set up a class and methods/functions for scraping

yt_scraper <- setRefClass(
  "yt_scraper",
  fields = list(
    base_url = "character",
    api_opts = "list",
    nextPageToken = "character",
    data = "list",
    unique_count = "numeric",
    done = "logical",
    core_df = "data.frame"),

  methods = list(
    scrape = function() {
      opts <- api_opts
          # DEBUG
          # cat(paste0("\n","Value of nextPageToken = ", nextPageToken,"\n"))
      if (!is.null(nextPageToken) || length(nextPageToken) != 0L || nextPageToken != "") {
      # if (!is.null(nextPageToken) || nextPageToken != "")
        opts$pageToken <- nextPageToken
      }

      res <- httr::content(
        httr::GET(base_url, query = opts))

      nextPageToken <<- gsub("\\=","",res$nextPageToken)
      data <<- c(data, res$items)
      unique_count <<- length(unique(data))
    },

    scrape_all = function(maxComments) {
      while (TRUE) {
                        cat(".") # DEBUG # UI FEEDBACK - I AM STILL ALIVE AND WORKING
                        flush.console() # DEBUG
        scrape()
        old_count <- unique_count
        if (unique_count == old_count) {
          done <<- TRUE
          nextPageToken <<- ""
          if (unique_count > maxComments) {
            # cat(paste("\nAPI returned more than max comment limit of:",maxComments,"\nFetching only first",maxComments,"comments.\n"))
            data <<- unique(data)[1:maxComments]
          }

          break
        }
      }
    },

    initialize = function(videoIDs,apiKey,k) {
              #print(apiKey) # DEBUG
              #flush.console() # DEBUG
              #print(videoIDs[k]) # DEBUG
              #flush.console() # DEBUG
              #print(paste("Value of k is: ",k)) # DEBUG
              #flush.console() # DEBUG
      base_url <<- "https://www.googleapis.com/youtube/v3/commentThreads/"
      api_opts <<- list(
        part = "snippet",
        maxResults = 100,
        textFormat = "plainText",
        videoId = videoIDs[k],
        key = apiKey,
        fields = "items,nextPageToken",
        orderBy = "published")
      nextPageToken <<- ""
      data <<- list()
      unique_count <<- 0
      done <<- FALSE
      core_df <<- data.frame()
    },

    reset = function() {
      data <<- list()
      nextPageToken <<- ""
      unique_count <<- 0
      done <<- FALSE
      core_df <<- data.frame()
    },

    cache_core_data = function() {
      if (nrow(core_df) < unique_count) {
        sub_data <- lapply(data, function(x) {
          data.frame(
            Comment = x$snippet$topLevelComment$snippet$textDisplay,
            User = x$snippet$topLevelComment$snippet$authorDisplayName,
            ReplyCount = x$snippet$totalReplyCount,
            LikeCount = x$snippet$topLevelComment$snippet$likeCount,
            PublishTime = x$snippet$topLevelComment$snippet$publishedAt,
            CommentId = x$snippet$topLevelComment$id,
            stringsAsFactors=FALSE)

        })
        core_df <<- do.call("rbind", sub_data)
      } else {
        message("\n`core_df` is already up to date.\n")
      }
    }
  )
)
