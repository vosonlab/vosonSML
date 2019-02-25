#' @title Import collected data previously saved to file
#'
#' @description Imports collected data from file into a dataframe of class \code{datasource} and specified 
#' \code{socialmedia} type that is usable by \code{\link{Create}} functions. 
#' 
#' @param file Character string. Collected data file path.
#' @param type Character string. Type of file or file format of file to import \code{csv} or \code{rds}. Default is 
#' \code{csv}.
#' @param socialmedia Character string. Social media type of collected data \code{twitter}, \code{youtube} or 
#' \code{reddit}.
#' 
#' @return A dataframe with datasource class attributes.
#' 
#' @aliases ImportData
#' @name vosonSML::ImportData
#' @export
ImportData <- function(file, type = "csv", socialmedia) {
  
  if (missing(file)) {
    stop("Please provide file path of data to import.", .call = FALSE) 
  }
  
  if (missing(socialmedia)) {
    stop("Please provide the social media type of data to import.", .call = FALSE) 
  }
  
  supported_types <- c("csv", "rds")
  
  if (!type %in% supported_types) {
    err <- paste0("File format not supported. please choose from: ", paste0(supported_types, collapse = ", "), ".")
    stop(err, .call = FALSE) 
  }
  
  df <- tryCatch({
    switch(type,
      "csv" = read.csv(file),
      "rds" = readRDS(file = file))
    
  }, error = function(e) {
    err <- paste0("Could not read: ", file, ".\n", gsub("^Error:\\s", "", paste0(e)))
    stop(err, .call = FALSE)
  })
  
  cat(paste0(toupper(type), " file read: ", file))
  
  class(df) <- c("data.table", "data.frame", "datasource")
  
  socialmedia <- tolower(socialmedia)
  switch(socialmedia,
    twitter = { class(df) <- append(class(df), c("twitter")) },
    youtube = { class(df) <- append(class(df), c("youtube")) },
    reddit = { class(df) <- append(class(df), c("reddit")) },
    stop("Unknown social media type provided as datasource.", .call = FALSE))
  
  return(df)
}
