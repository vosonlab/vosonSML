searchCommentsForMentions <-
function (commentsTextCleaned,usernamesCleaned) {

  ptm <- proc.time() # Start the timer # DEBUG

  matchTemp <- lapply(commentsTextCleaned, function(x) {

      tempResult <- lapply(usernamesCleaned, function(y) {

        foo <- grep(paste("(\\+|\\@)", y, sep=""),x)

          if(length(foo)>0){
            return(y)
          }
          else {
            return("FALSE")
          }

        }
      )
    }
  )

  matchTemp <- unlist(matchTemp)
  # matchTemp <- as.vector(matchTemp)
  # matchTemp <- iconv(matchTemp, to = 'UTF-8')

  # have to split `matchTemp` into as many groups as there are rows (i.e. comment texts)
  matchTemp2 <- split(matchTemp, ceiling(seq_along(matchTemp)/length(commentsTextCleaned)))

  # Now we want to retrieve the username with MAX CHARACTERS that was mentioned,
  # or if all values were "FALSE" then just return a single "FALSE" value.
  # THE REASON IS:
  # If we have the following comment text: "+Timothy some text",
  # and there are two users in the data, namely "Tim" and "Timothy",
  # the `grep` will have matched both of these in the comment text.
  # So, we want to ensure it takes the username with more characters (i.e. "Timothy"),
  # rather than the subset match (i.e. "Tim").

  matchTemp3 <- tryCatch({

      lapply(matchTemp2, function(x) {

        # if length of element is 0 then return FALSE
          # if (length(x)==0) {
          #  return("FALSE")
          # }

        # if all elements == "FALSE" then just return "FALSE"
          if (length(x[which(x=="FALSE")])==length(x)) {
    # cat("\nAll elements of list slice are FALSE\n")               # DEBUG
            return("FALSE")
          }

        # if all elements except one == "FALSE" then return the 'non false' element
        # e.g. c("FALSE", "FALSE", "Timothy", "FALSE") ---> returns "Timothy"
          if (length(x[which(x!="FALSE")])==1){
    # cat("\nFound 1 non-false ELEMENT:\n")                         # DEBUG
    # cat(paste0(x[which(x!="FALSE")],"\n"))                        # DEBUG
            return(x[which(x!="FALSE")])
          }

          else {
            tempResult <- x[which(x!="FALSE")]
            tempResult <- x[which(nchar(x)==max(nchar(x)))][1] # if two duplicate results (e.g. "Timothy" and "Timothy"), then just return the 1st
    # cat("\nTwo or more results found:\n")                         # DEBUG
    # cat("\nTwo or more results found:\n")                         # DEBUG
    # cat(x[which(x!="FALSE")])
    # cat("\n")
            return(tempResult)
            # return(max(nchar(x))) #DEBUG
          }
          })

  }, error = function(err) {

    # error handler picks up where error was generated
    print(paste("\nI caught an error (are there mentions/replies between users in the comments for your video(s)? :\n",err))
    return(matchTemp2) # if it catches an error, we just return the original object

  }) # END tryCatch

  # debugResultDF <- data.frame(commentsTextCleaned,usernamesCleaned,unlist(matchTemp3)) #DEBUG
  finalMatchesTemp <- as.vector(unlist(matchTemp3))

  # convert back (or 'de-regex') the username characters
  finalMatches <- gsub("\\\\","",finalMatchesTemp)

  #functionRunTime <- proc.time() - ptm                # DEBUG
  #print("Runtime of FindMentions function was:")      # DEBUG
  #flush.console()                                     # DEBUG
  #print(functionRunTime)                              # DEBUG
  #flush.console()                                     # DEBUG

  return (finalMatches)

}
