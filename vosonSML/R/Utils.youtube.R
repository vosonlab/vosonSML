#' Extract the ids from a list of youtube video urls
#'
#' This function reads youtube video urls from a list and or a text file and converts them to a vector of video ids. 
#' For example, url "https://www.youtube.com/watch?v=73I5dRucCds" returns the id "73I5dRucCds". This function can be 
#' used to create a vector for the youtube \code{Collect} methods \code{videoIDs} parameter.
#' 
#' @param urls Character vector. List of youtube urls.
#' @param file Character string. Text file containing youtube urls.
#' 
#' @return A vector of youtube video ids as character strings that were extracted from input video urls.
#' 
#' @seealso \code{Collect}
#' @keywords collect youtube video id
#' 
#' @export
GetYoutubeVideoIDs <- function(urls = NULL, file = NULL) {
  video_ids <- c()
  
  if (is.null(urls) & is.null(file)) {
    cat("Please provide a vector and or file of youtube video urls.\n")
    return(video_ids)
  }
  
  if (!is.null(urls)) {
    video_ids <- append(video_ids, sapply(urls, GetYoutubeVideoID(url)))
  }
  
  if (!is.null(file)) {
    video_ids_file <- tryCatch({
      read.table(file, sep = "\n", strip.white = TRUE)
    }, error = function(e) {
      cat(paste0(e))
      return(NULL)
    })
    
    if (!is.null(video_ids_file)) {
      video_ids_file <- as.vector(video_ids_file$V1)
      video_ids <- append(video_ids, sapply(urls, GetYoutubeVideoID(url)))
    }
  }
  
  if (length(video_ids) < 1) {
    cat("No youtube video ids found.\n")
  } else {
    cat(paste0("Extracted ", length(video_ids), " video ids.\n"))
  }
  
  return(video_ids)
}

# extract the id from a youtube video url
GetYoutubeVideoID <- function(url) {
  id_pattern <- "^[0-9A-Za-z_\\-]+$"
  
  # already an id
  if (grepl(id_pattern, url, ignore.case = TRUE, perl = TRUE)) {
    return(url)
  }  
  
  url <- parse_url(url)
  video_id <- NULL
  
  if (is.null(url$hostname)) {
    return(NULL)
  }
  
  # url format https://youtu.be/xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "youtu.be") {
    if (length(url$path) > 0) {
      video_id <- url$path[1]
    }
  }
  
  # url format https://www.youtube.com/watch?v=xxxxxxxxxxx
  if (tolower(trimws(url$hostname)) == "www.youtube.com") {
    if (!is.null(url$query$v)) {
      video_id <- url$query$v
    }
  }
  
  if (!grepl(id_pattern, video_id, ignore.case = TRUE, perl = TRUE)) {
    return(NULL)
  }
  
  return(video_id)
}

# search youtube video comments text for mentions of other users
SearchCommentsForMentions <- function(commentsTextCleaned, usernamesCleaned) {

  matchTemp <- lapply(commentsTextCleaned, function(x) {
    tempResult <- lapply(usernamesCleaned, function(y) {
      foo <- grep(paste("(\\+|\\@)", y, sep=""), x)
      
      if(length(foo) > 0) {
        return(y)
      } else {
        return("FALSE")
      }
    })
  })
  
  matchTemp <- unlist(matchTemp)
  
  # have to split matchTemp into as many groups as there are rows i.e. comment texts
  matchTemp2 <- split(matchTemp, ceiling(seq_along(matchTemp) / length(commentsTextCleaned)))
  
  # we want to retrieve the username with MAX CHARACTERS that was mentioned, or if all values were "FALSE" then 
  # just return a single "FALSE" value.
  
  # THE REASON IS:
  # if we have the following comment text: "+Timothy some text", and there are two users in the data, namely "Tim" 
  # and "Timothy", the grep will have matched both of these in the comment text.
  # so, we want to ensure it takes the username with more characters (i.e. "Timothy"), rather than the subset 
  # match (i.e. "Tim").
  
  matchTemp3 <- tryCatch({
    lapply(matchTemp2, function(x) {
      
      # if all elements are "FALSE" then return "FALSE"
      if (length(x[which(x == "FALSE")]) == length(x)) {
        return("FALSE")
      }
      
      # if all elements except one are "FALSE" then return the non false element
      # e.g. c("FALSE", "FALSE", "Timothy", "FALSE") -> returns "Timothy"
      if (length(x[which(x != "FALSE")]) == 1) {
        
        return(x[which(x != "FALSE")])
        
      } else {
        tempResult <- x[which(x != "FALSE")]
        # if two duplicate results (e.g. "Timothy" and "Timothy"), then just return the first one
        tempResult <- x[which(nchar(x) == max(nchar(x)))][1]
        
        return(tempResult)

      }
    })
  }, error = function(e) {
    # error handler picks up where error was generated
    print(paste0("Are there any mentions/replies between users in the comments for your videos?\n", e))
    return(matchTemp2) # if it catches an error, we just return the original object
  })
  
  finalMatchesTemp <- as.vector(unlist(matchTemp3))
  
  # convert back (or 'de-regex') the username characters
  finalMatches <- gsub("\\\\","",finalMatchesTemp)
  
  return (finalMatches)
}
