#' Create YouTube Actor Network
#'
#' Creates a unimodal actor network based on comments and replies to one or more youtube videos.
#'
#' @param x dataframe containing comments data collected and structured by CollectDataYoutube.
#' @param writeToFile boolean, if TRUE then igraph data is saved to a file in the current working directory in 
#' graphml format. The file name will contain the current system time. Default is FALSE.
#' 
#' @return igraph object containing the actor network with edge attribute comment id
#' 
#' @noRd
CreateActorNetwork.youtube <- function(x, writeToFile) {

  if (missing(writeToFile)) {
    writeToFile <- FALSE
  }

  returnedScrapedComments <- x # match the variable names to avoid warnings in package compilation

  # returnedScrapedComments dataframe columns:
  # 1 Comment
  # 2 User
  # 3 ReplyCount
  # 4 LikeCount
  # 5 PublishTime
  # 6 CommentId
  # 7 ParentID
  # 8 ReplyToAnotherUser
  # 9 VideoID
  
  df <- returnedScrapedComments
  
  if (nrow(df) == 0) {
    cat(paste0("\nOops! There are no user comments to make a network from.\nPlease find video(s) where users have",
               " commented on a video or to each other.\nReturning...\n"))
    return()
  }
  
  # direct comments which are not replies to others to the video id (treating it as a user)
  # in the graph the user will appear as id VIDEO:XXxxX
  notReplies <- which(df$ReplyToAnotherUser == "FALSE" & df$ParentID == "None")
  df$ReplyToAnotherUser[notReplies] <- paste0("VIDEO:", df$VideoID[notReplies])

  usersTemp <- df[, 2]
  mentionedUsersTemp <- df[, 8]
  commentId <- df[, 6]

  # create pairs of users from, to
  dfActorNetwork1 <- data.frame(usersTemp, mentionedUsersTemp, commentId)

  # make a vector of all the unique actors in the network1
  actorsNames <- unique(factor(c(as.character(unique(dfActorNetwork1[, 1])), 
                                 as.character(unique(dfActorNetwork1[, 2])))))  

  # make a dataframe of the relations between actors
  relations <- data.frame(from = dfActorNetwork1[, 1], to = dfActorNetwork1[, 2], commentId = dfActorNetwork1[, 3])

  # convert into a graph
  g <- graph.data.frame(relations, directed = TRUE, vertices = actorsNames)

  # add node labels
  V(g)$label <- V(g)$name

  # output the final network to a graphml file
  if (isTrueValue(writeToFile)) {
    writeOutputFile(g, "graphml", "YoutubeActorNetwork")
  }

  cat("\nDone!\n")
  flush.console()

  return(g)
}
