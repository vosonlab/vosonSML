#' @title Create twitter bimodal network
#' 
#' @description Creates a bimodal network from tweets returned from the twitter search query. In this network there are 
#' two types of nodes, twitter users who have tweeted (actors) and hashtags found within their tweets. Network edges 
#' are weighted and represent hashtag usage by the actor or specifically their tweets that contain a hashtag matching 
#' the name of the node they are directed towards.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"bimodal"}.
#' @param removeTermsOrHashtags Character vector. Terms or hashtags to remove from the bimodal network. For example, 
#' this parameter could be used to remove the search term or hashtag that was used to collect the data by removing any
#' nodes with matching name. Default is \code{NULL} to remove none.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # create a twitter bimodal network graph removing the hashtag '#auspol' as it was used in 
#' # the twitter search query
#' bimodalNetwork <- twitterData %>% 
#'                   Create("bimodal", removeTermsOrHashtags = c("#auspol"), verbose = TRUE)
#' 
#' # network
#' # bimodalNetwork$nodes
#' # bimodalNetwork$edges
#' }
#' 
#' @export
Create.bimodal.twitter <- function(datasource, type, removeTermsOrHashtags = NULL, verbose = FALSE, ...) {
  
  from <- to <- edge_type <- timestamp <- status_id <- NULL

  if (is.null(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- "#fake_hashtag_foobar42_1234567890"
  } else {
    removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) # coerce to vector to be sure
  }
  
  df <- datasource
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
  
  # df_entities <- unique(df_entities)
  df_entities %<>% distinct(.data$entity_id, .keep_all = TRUE)
  
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
  
  # remove the search term / hashtags, if user specified it:
  if (removeTermsOrHashtags[1] != "#fake_hashtag_foobar42_1234567890") {
    # remove hashtags
    relations %<>% dplyr::filter(!.data$to %in% removeTermsOrHashtags)
    df_entities %<>% dplyr::filter(!.data$entity_id %in% removeTermsOrHashtags)
    
    # # we force to lowercase because all terms/hashtags are already converted to lowercase
    # toDel <- match(tolower(removeTermsOrHashtags), V(g)$name)
    # # in case of user error (i.e. trying to delete terms/hashtags that don't exist in the data)
    # toDel <- toDel[!is.na(toDel)]
    # g <- delete.vertices(g, toDel)
  }
  
  cat("Done.\n")
  flush.console()
  
  func_output <- list(
    "nodes" = tibble::as_tibble(df_entities),
    "edges" = tibble::as_tibble(relations)
  )
  
  class(func_output) <- union(class(func_output), c("network", "bimodal", "twitter"))
  
  func_output
}
