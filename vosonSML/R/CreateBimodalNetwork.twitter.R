# Create twitter bimodal network
# 
# Creates a bimodal network from collected tweets.
#
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' 
#' @return A twitter bimodal network as igraph object.
#' 
#' @rdname CreateBimodalNetwork
#' @export
CreateBimodalNetwork.twitter <- function(x, removeTermsOrHashtags = NULL, writeToFile = FALSE, verbose = FALSE, ...) {
  
  from <- to <- edge_type <- timestamp <- status_id <- NULL

  if (is.null(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- "#fake_hashtag_foobar42_1234567890"
  } else {
    removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) # coerce to vector to be sure
  }
  
  df <- x
  df <- data.table(df)

  df_stats <- networkStats(NULL, "collected tweets", nrow(df))

  cat("Generating twitter bimodal network...\n")
  flush.console()

  df_entities <- data.table("entity_id" = character(0), "display_name" = character(0))

  # for speed we will pre-allocate dt_combined to a very large size (more rows than needed)
  # and after everything is finished we will delete the unused rows
  dt_combined <- data.table(
    from = as.character(c(rep("NA_f00", 20000000))),
    to = as.character(c(rep("NA_f00", 20000000))),
    edge_type = as.character(c(rep("NA_f00", 20000000))),
    timestamp = as.character(c(rep("NA_f00", 20000000))),
    status_id = as.character(c(rep("NA_f00", 20000000)))
  )
  
  setkey(dt_combined, from) # set the key value of the data table
  
  nextEmptyRow <- 1 # so we can update rows in 'dt_combined' in a relatively efficient way
  
  # we only need to do the 'hashtag' data (currently)
  count <- 0
  hashtag_count <- 0
  for (i in 1:nrow(df)) {
    if (length(df$hashtags[[i]]) > 0) { # skip any rows where no hashtags were used # hashtags_used
      if (length(df$hashtags[[i]]) == 1 & is.na(df$hashtags[[i]][1])) {
        next 
      }
      
      count <- count + 1
      df_entities <- rbind(df_entities, list(df$user_id[i][[1]], df$screen_name[i][[1]]), stringsAsFactors = FALSE)
      
      for (j in 1:length(df$hashtags[[i]])) { # for each hashtag in list
        
        tag <- paste0("#", df$hashtags[[i]][j])
        
        dt_combined[nextEmptyRow, from:= as.character(df$user_id[i][[1]])]
        dt_combined[nextEmptyRow, to := as.character(tag)]
        dt_combined[nextEmptyRow, edge_type := as.character("hashtag")]
        dt_combined[nextEmptyRow, timestamp := as.character(df$created_at[i][[1]])]
        dt_combined[nextEmptyRow, status_id := as.character(df$status_id[i][[1]])]
        
        df_entities <- rbind(df_entities, list(tag, tag), stringsAsFactors = FALSE)

        hashtag_count = hashtag_count + 1 
        nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dt_combined`
      }
    }
  }
  df_stats <- networkStats(df_stats, "tweets with hashtags", count, TRUE)
  df_stats <- networkStats(df_stats, "hashtags", hashtag_count, TRUE)
  
  dt_combined <- dt_combined[edge_type != "NA_f00"]
  
  df_entities <- unique(df_entities)
  
  df_stats <- networkStats(df_stats, "nodes", nrow(df_entities))
  df_stats <- networkStats(df_stats, "edges", sum(df_stats$count[df_stats$edge_count == TRUE]))
  if (verbose) {
    networkStats(df_stats, print = TRUE) 
  }
  
  relations <- data.frame(
    from = dt_combined$from,
    to = dt_combined$to,
    edge_type = dt_combined$edge_type,
    timestamp = dt_combined$timestamp,
    status_id = dt_combined$status_id)
  
  g <- graph.data.frame(relations, directed = TRUE, vertices = df_entities)
  
  V(g)$display_name <- ifelse(is.na(V(g)$display_name), paste0("ID:", V(g)$name), V(g)$display_name)
  
  # remove the search term / hashtags, if user specified it:
  if (removeTermsOrHashtags[1] != "#fake_hashtag_foobar42_1234567890") {
    # we force to lowercase because all terms/hashtags are already converted to lowercase
    toDel <- match(tolower(removeTermsOrHashtags), V(g)$name)
    # in case of user error (i.e. trying to delete terms/hashtags that don't exist in the data)
    toDel <- toDel[!is.na(toDel)]
    g <- delete.vertices(g, toDel)
  }
  
  V(g)$label <- V(g)$display_name
  
  if (writeToFile) { writeOutputFile(g, "graphml", "TwitterBimodalNetwork") }
  
  cat("Done.\n")
  flush.console()
  
  return(g)
}
