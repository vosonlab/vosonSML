# @title Create reddit activity network
# 
# @export
Create.activity.reddit <- function(datasource, type, writeToFile = FALSE, verbose = TRUE, ...) {
  df <- tibble::as_tibble(datasource) 
  
  df_stats <- networkStats(NULL, "collected reddit comments", nrow(df))
  
  # cat("Generating youtube activity network...\n")
  # flush.console()
  
  # edges
  # df_relations <- df %>% 
  #   dplyr::select(.data$id, .data$subreddit, .data$thread_id, .data$structure, .data$user) %>% 
  #   dplyr::rename("comment_id" = .data$id)
}