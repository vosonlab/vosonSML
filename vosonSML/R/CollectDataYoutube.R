#' Collect YouTube comments data for generating different types of networks
#'
#' This function collects YouTube comments data for one or more YouTube videos. It structures the data into a data 
#' frame of class dataSource.youtube, ready for creating networks for further analysis.
#'
#' CollectDataYoutube collects public comments from YouTube videos, using the YouTube API.
#'
#' The function then finds and maps the relationships of YouTube users who have interacted with each other 
#' (i.e. user i has replied to user j or mentioned user j in a comment) and structures these relationships into a data 
#' frame format suitable for creating unimodal networks (CreateActorNetwork).
#'
#' For multiple videos, the user may wish to use the function GetYoutubeVideoIDs, which creates a character
#' vector of video IDs from a plain text file of YouTube video URLs, which can then be used for the videoIDs
#' argument of the function CollectDataYoutube.
#'
#' @param videoIDs character vector, specifying one or more YouTube video IDs. For example, if the video URL is 
#' 'https://www.youtube.com/watch?v=W2GZFeYGU3s', then use videoIDs='W2GZFeYGU3s'. For multiple videos, the 
#' function GetYoutubeVideoIDs can be used to create a vector object suitable as input for videoIDs.
#' @param apiKeyYoutube character string, specifying the Google Developer API Key used for authentication.
#' @param verbose logical. If TRUE then this function will output runtime information to the console as it 
#' computes. Useful diagnostic tool for long computations. Default is FALSE.
#' @param writeToFile logical. If TRUE then the data is saved to file in current working directory (CSV format), 
#' with filename denoting current system time. Default is FALSE.
#' @param maxComments numeric integer, specifying how many 'top-level' comments to collect from each video. This value 
#' *does not* take into account 'reply' comments (i.e. replies to top-level comments), therefore the total number of
#' comments collected may be higher than maxComments. By default this function attempts to collect all comments.
#' 
#' @return A dataframe object of class dataSource.youtube that can be used for creating unimodal networks 
#' (CreateActorNetwork).
#' 
#' @note Currently supported network types: unimodal 'actor' network; CreateActorNetwork.
#'
#' Data generated using this function is *not* suitable for dynamic networks.
#' Dynamic YouTube comments networks are not currently implemented in the vosonSML package. This will be implemented in 
#' a future release.
#'
#' Note on maxComments argument: Due to quirks/specifications of the Google API, it is currently not possible to 
#' specify the exact number of comments to return from the API using maxResults argument (i.e.including comments 
#' that are replies to top-level comments). Therefore, the number of comments collected is usually somewhat greater than
#' maxComments, if a value is specified for this argument. For example, if a video contains 10 top-level 
#' comments, and one of these top-level comments has 5 'child' or reply comments, then the total number of comments
#' collected will be equal to 15. Currently, the user must 'guesstimate' the maxResults value, to collect a 
#' number of comments in the order of what they require.
#'
#' @noRd
CollectDataYoutube <- function(videoIDs, apiKeyYoutube, verbose = FALSE, writeToFile = FALSE, 
                               maxComments = 10000000000000) {
  
  # maxComments defaults to an arbitrary very large number

  if (missing(videoIDs) || !is.vector(videoIDs) || length(videoIDs) < 1) {
    stop("Please provide a vector of one or more youtube video ids.\n", call. = FALSE)
  }
 
  if (missing(apiKeyYoutube) || nchar(apiKeyYoutube) < 1) {
    stop("Please provide a valid youtube api key.\n", call. = FALSE)
  }
  
  apiKey <- apiKeyYoutube # to play nice with existing code
    
  # Start data collection
  
  # Create a dataframe to iteratively store comments from all the videos that the user wants to scrape 
  # (i.e. specified in videoIDs) uses 'dummy' data in first row (which is removed later)
  dataCombined <- data.frame(Comment = "foo",
                             User = "bar",
                             ReplyCount = "99999999",
                             LikeCount = "99999999",
                             PublishTime = "timestamp",
                             CommentId = "99999999123456789",
                             ParentID = "foobar",
                             ReplyToAnotherUser = "FALSE",
                             VideoID = "foobarfoobar",
                             stringsAsFactors = FALSE)
  
  # Iterate through the videos in videoIDs, adding to dataCombined.
  for (k in 1:length(videoIDs)) {
    cat(paste0("Collecting video number: ", k, " of ", length(videoIDs), "\n", sep = "")) # DEBUG
    cat("---------------------------------------------------------------\n")
    
    ############################## Collect comment threads #############################
    
    rObj <- yt_scraper(videoIDs, apiKey, k, verbose)
    
    rObj$scrape_all(maxComments)

    ## Make a dataframe out of the results
    
    if (verbose) { cat(paste0("** Creating dataframe from threads of ", videoIDs[k], ".\n", sep = "")) }
    
    tempData <- lapply(rObj$data, function(x) {
      data.frame(Comment = x$snippet$topLevelComment$snippet$textDisplay,
                 User = x$snippet$topLevelComment$snippet$authorDisplayName,
                 ReplyCount = x$snippet$totalReplyCount,
                 LikeCount = x$snippet$topLevelComment$snippet$likeCount,
                 PublishTime = x$snippet$topLevelComment$snippet$publishedAt,
                 CommentId = x$snippet$topLevelComment$id,
                 ParentID = "None",
                 ReplyToAnotherUser = "FALSE",
                 VideoID = videoIDs[k], # actual reference to API data is: 
                                        # x$snippet$topLevelComment$snippet$videoIDs[k]
                 stringsAsFactors = FALSE)
    })
    
    core_df <- do.call("rbind", tempData)
      
    ############################## Collect comment replies #############################
    
    commentIDs <- core_df$CommentId
    
    # only attempt to collect replies for comments we know have replies
    commentIDs_with_replies <- core_df[which(core_df$ReplyCount > 0), ] # column 6
    commentIDs_with_replies <- commentIDs_with_replies$CommentId
    
    cat(paste0("** Collecting replies for ", length(commentIDs_with_replies), 
               " threads with replies. Please be patient.\n", sep = "")) # commentIDs
    
    base_url <- "https://www.googleapis.com/youtube/v3/comments"
    
    # 'dummy' first row of dataframe, for DEBUG purposes (fix later..)
    dataRepliesAll <- data.frame(Comment = "foo",
                                 User = "bar",
                                 ReplyCount = "99999999",
                                 LikeCount = "99999999",
                                 PublishTime = "timestamp",
                                 CommentId = "99999999123456789",
                                 ParentID = "foobar",
                                 ReplyToAnotherUser = "FALSE",
                                 VideoID = videoIDs[k], # API DOESN'T SEEM TO RETURN HERE, no matter anyway
                                 stringsAsFactors = FALSE)

    # for each thread
    total_replies <- 0
    for (i in 1:length(commentIDs_with_replies)) { # commentIDs
      api_opts <- list(part = "snippet",
                       textFormat = "plainText",
                       parentId=commentIDs_with_replies[i], # commentIDs
                       key = apiKey)
      
      init_results <- httr::content(httr::GET(base_url, query = api_opts)) # TODO: should die when there is error
      
      num_items <- length(init_results$items)
      
      if (verbose) {
        if (i == 1) {
          cat("Comment replies ")
        }
        
        cat(paste(num_items, ""))
        flush.console()
      } else {
        cat(".")
        flush.console()        
      }
        
      total_replies <- total_replies + num_items
      
      tempDataReplies <- lapply(init_results$items, function(x) {
        data.frame(Comment = x$snippet$textDisplay,
                   User = x$snippet$authorDisplayName,
                   ReplyCount = 0, # there is no ReplyCount returned for replies (API specs)
                   LikeCount = x$snippet$likeCount,
                   PublishTime = x$snippet$publishedAt,
                   CommentId = x$id,
                   ParentID = x$snippet$parentId,
                   ReplyToAnotherUser = "FALSE",
                   VideoID = videoIDs[k], # API DOESN'T SEEM TO RETURN HERE, not that it matters
                   stringsAsFactors = FALSE)
      })
      
      tempDataRepliesBinded <- do.call("rbind", tempDataReplies)
      
      dataRepliesAll <- rbind(dataRepliesAll, tempDataRepliesBinded)
    }
    
    cat(paste0("\n** Collected replies: ", total_replies, "\n", sep = ""))
    cat(paste0("** Total video comments: ", length(commentIDs) + total_replies, "\n", sep = ""))
    cat("---------------------------------------------------------------\n")
    
    ############################## Combine comment threads and replies #############################
      
    # get rid of "dummy" first row
    dataRepliesAll <- dataRepliesAll[-1, ]
    
    # combine the comments and replies dataframes
    dataCombinedTemp <- rbind(core_df, dataRepliesAll)
    
    # APPEND TO THE OVERALL DATAFRAME (I.E. MULTIPLE VIDEO COMMENTS)
    dataCombined <- rbind(dataCombined, dataCombinedTemp)
  }
    
  cat(paste0("** Total comments collected for all videos ", nrow(dataCombined)-1, ".\n", sep = ""))
  
  # Remove 'dummy' first row
  dataCombined <- dataCombined[-1, ]
  
  ## Throw Error when no comment can be collected
  if (nrow(dataCombined) == 0) {
    stop(paste0("No comments could be collected from the given video Ids: ", videoIDs, "\n", sep = ""))
  }

  if (verbose) {
    cat("Cleaning and structuring data. Please be patient.\n")
  }
  
  ############################## Map relations between users into dataframe #############################
  
  ## For each commentsDataNames element, if any commentTexts elements pattern matches
  ## with a commentsDataNames element, then it is a reply/mention:
  
  # isReplyToAnotherUser <- c(rep("FALSE",length(dataCombined$Comment)))
  isReplyToAnotherUser <- dataCombined[, 8]
  
  ### !!!!! Escape any punctuation characters in names when using GREP!!!
  ## From: http://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
  ## This uses an R implementation of Perl's `quotemeta` function
  
  usernamesCleaned <- dataCombined$User # vector of user names (speed + readability)
  commentsTextCleaned <- dataCombined$Comment # duplicate of comment text data (speed + readability)
  
  # This function is from the library("Hmisc")
  usernamesCleaned <- escapeRegex(usernamesCleaned)
  
  # NEW WAY (OPTIMISED - better, faster, stronger...)
  dataCombined$ReplyToAnotherUser <- SearchCommentsForMentions(commentsTextCleaned, usernamesCleaned)
  
  ## Map the comment replies within PARENT COMMENT THREADS into dataframe
  
  parentsTemp <- which(dataCombined[, 7] != "None" & dataCombined[, 8] == "FALSE")
  
  if (length(parentsTemp) != 0) {
    for (i in 1:nrow(dataCombined[parentsTemp, ])) {
      
      # take the 1st match - we could try to scrape MULTIPLE REPLIES/MENTIONS, but would require re-think.
      tempMatch <- which(dataCombined[parentsTemp[i], 7] == dataCombined[, 6])[1] 
      dataCombined[parentsTemp[i], 8] <- dataCombined[tempMatch, 2]
      
    }
  }
  
  if (writeToFile) { writeOutputFile(dataCombined, "csv", "YoutubeData") }
    
  cat("Done.\n")
  flush.console()
  
  #############################################################################
  # return dataframe to environment
  
  class(dataCombined) <- append(class(dataCombined), c("dataSource", "youtube"))
  
  return(dataCombined)
  
  #############################################################################
}

## Set up a class and methods/functions for scraping
yt_scraper <- setRefClass(
  "yt_scraper", 
  fields = list(
    base_url = "character",
    api_opts = "list",
    nextPageToken = "character",
    page_count = "numeric",
    data = "list",
    unique_count = "numeric",
    done = "logical",
    core_df = "data.frame",
    verbose = "logical"),
  
  methods = list(
    # collect api results for page
    scrape = function() {
      
      # set default api request options
      opts <- api_opts
      
      if (is.null(nextPageToken) | length(trimws(nextPageToken)) == 0L | trimws(nextPageToken) == "") {
        if (page_count >= 1) {
          if (verbose) {
            cat(paste0("-- No nextPageToken. Returning. page_count is: ", page_count, "\n"))
          }
          # return no threads collected to signal done
          return(0)
        } else {
          if (verbose) {
            cat("-- First thread page. No pageToken.\n")
          }
        }
      } else {
        opts$pageToken <- trimws(nextPageToken)
        
        if (verbose) {
          cat(paste0("-- Value of pageToken: ", opts$pageToken, "\n"))
        }
      }
      
      page_count <<- page_count + 1
      
      res <- httr::content(httr::GET(base_url, query = opts))
      
      if (is.null(res$nextPageToken)) {
        nextPageToken <<- ""
      } else {
        nextPageToken <<- res$nextPageToken
      }
      
      # add threads to data list
      data <<- c(data, res$items)
      
      # return count of threads collected from page
      return(length(res$items))
    },
    
    # collect all video threads until done or max comments reached
    scrape_all = function(maxComments) {
      cat(paste0("** video Id: ", api_opts$videoId ,"\n", sep = ""))
      if (verbose) {
        cat(paste0("   [results per page: ", api_opts$maxResults, " | max comments per video: ", maxComments, "]\n", 
                   sep = ""))
      }
      
      thread_count <- 0
      
      while (TRUE) {
        # collect threads for current page
        thread_count <- scrape()
        
        if (verbose) {
          cat(paste0("-- Collected threads from page: ", thread_count, "\n", sep = ""))
        }        
        
        if (thread_count == 0 | length(data) > maxComments) {
          done <<- TRUE
          nextPageToken <<- ""
          
          if (length(data) > maxComments) {
            cat(paste0("-- API returned more than max comments. Results truncated to first ", maxComments, 
                       " threads.\n", sep = ""))
            
            data <<- data[1:maxComments]
          }
          
          if (verbose) { cat(paste0("-- Done collecting threads.\n", sep = "")) }
          
          break
        }
      }
      if (verbose) {
        cat(paste0("** Results page count: ", page_count, "\n", sep = ""))
      }
      cat(paste0("** Collected threads: ", length(data), "\n", sep = ""))
    },
    
    initialize = function(videoIDs, apiKey, k, verbose = FALSE) {
      base_url <<- "https://www.googleapis.com/youtube/v3/commentThreads/"
      api_opts <<- list(part = "snippet",
                        maxResults = 100,
                        textFormat = "plainText",
                        videoId = videoIDs[k],
                        key = apiKey,
                        fields = "items,nextPageToken",
                        orderBy = "published")
      page_count <<- 0
      nextPageToken <<- ""
      data <<- list()
      unique_count <<- 0
      done <<- FALSE
      core_df <<- data.frame()
      verbose <<- verbose
    },
    
    reset = function() {
      data <<- list()
      page_count <<- 0
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
            stringsAsFactors = FALSE)
        })
        core_df <<- do.call("rbind", sub_data)
      } else {
        message("core_df is already up to date.\n")
      }
    }
  )
)
