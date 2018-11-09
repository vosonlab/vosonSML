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

  df_comments <- x # match the variable names to avoid warnings in package compilation

  # df_comments columns:
  # 1 Comment      4 LikeCount     7 ParentID
  # 2 User         5 PublishTime   8 ReplyToAnotherUser
  # 3 ReplyCount   6 CommentId     9 VideoID

  if (nrow(df_comments) == 0) {
    cat(paste0("\nOops! There are no user comments to make a network from.\nPlease find video(s) where users have",
               " commented on a video or to each other.\nReturning...\n"))
    return()
  }

  # direct comments which are not replies to others to a video id node
  # in the graph the video nodes will appear as VIDEO:AbCxYz where AbCxYz is the id
  not_replies <- which(df_comments$ReplyToAnotherUser == "FALSE" & df_comments$ParentID == "None")
  df_comments$ReplyToAnotherUser[not_replies] <- paste0("VIDEO:", df_comments$VideoID[not_replies])

  comment_users <- df_comments[, 2]
  mentioned_users <- df_comments[, 8]
  comment_ids <- df_comments[, 6]

  # create network of users that commented on videos as from user, to user, comment id
  df_actor_network <- data.frame(comment_users, mentioned_users, comment_ids)

  # make a vector of all the unique actors in the network
  actor_names <- unique(factor(c(as.character(unique(df_actor_network[, 1])),
                                 as.character(unique(df_actor_network[, 2])))))

  # make a dataframe of the relations between actors
  relations <- data.frame(from = df_actor_network[, 1], to = df_actor_network[, 2], commentId = df_actor_network[, 3])

  # convert into a graph
  g <- graph.data.frame(relations, directed = TRUE, vertices = actor_names)

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
