# Create youtube actor network
# 
# Creates a unimodal actor network based on comments and replies to one or more youtube videos.
#
#' @return A youtube actor network as igraph object.
#' 
#' @rdname CreateActorNetwork
#' @export
CreateActorNetwork.youtube <- function(x, writeToFile = FALSE, ...) {

  df_comments <- x # match the variable names to avoid warnings in package compilation

  # df_comments columns:
  # 1 Comment      4 LikeCount     7 ParentID
  # 2 User         5 PublishTime   8 ReplyToAnotherUser
  # 3 ReplyCount   6 CommentId     9 VideoID

  cat("Generating youtube actor network...\n")
  flush.console()
  
  if (nrow(df_comments) == 0) {
    stop(paste0("There are no user comments to make a network from, please check that the videos selected ",
               "for collection have comments.\n"), call. = FALSE)
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
  if (writeToFile) { writeOutputFile(g, "graphml", "YoutubeActorNetwork") }

  cat("Done.\n")
  flush.console()

  return(g)
}
