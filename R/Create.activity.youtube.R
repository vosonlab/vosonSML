#' @title Create youtube activity network
#' 
#' @export
Create.activity.youtube <- function(datasource, type, writeToFile = FALSE, ...) {
  df_comments <- tibble::as_tibble(datasource) 
  
  cat("Generating youtube activity network...\n")
  flush.console()
  
  # df_relations
  
}
