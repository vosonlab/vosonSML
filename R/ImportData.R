#' @title Import collected data previously saved to file
#'
#' @description Imports collected data from file into a dataframe of class \code{datasource} and specified 
#' \code{socialmedia} type that is usable by \code{\link{Create}} functions. 
#' 
#' @param path Character string. Collected data file path.
#' @param socialmedia Character string. Social media type of collected data \code{twitter}, \code{youtube} or 
#' \code{reddit}.
#' @param type Character string. Type of file or file format of file to import \code{csv} or \code{rds}. Default is 
#' \code{NULL} to use extension.
#' 
#' @return A dataframe with datasource class attributes.
#' 
#' @aliases ImportData
#' @name vosonSML::ImportData
#' @export
ImportData <- function(path, socialmedia, type = NULL) {
  
  # expected import types
  supported_types <- c("csv", "rds")
  
  if (missing(path)) {
    stop("Please provide file path of data to import.", call. = FALSE) 
  }
  
  if (missing(socialmedia)) {
    stop("Please provide the social media type of data to import.", call. = FALSE) 
  }
  
  # if type is null check file extension
  if (is.null(type)) {
    type <- gsub(".*\\.([A-Za-z]{3})$", "\\1", path, ignore.case = TRUE, perl = TRUE)
  }
  
  type <- tolower(trimws(type))
  if (!type %in% supported_types) {
    stop(paste0("File format not supported. please choose from: ", paste0(supported_types, collapse = ", "), "."), 
         call. = FALSE) 
  }
  
  df <- tryCatch({
    switch(type,
      "csv" = read.csv(path),
      "rds" = readRDS(file = path))
    
  }, error = function(e) {
    stop(paste0("Could not read: ", path, ".\n", gsub("^Error:\\s", "", paste0(e))), call. = FALSE)
  })
  
  cat(paste0(toupper(type), " file read: ", path), "\n")
  
  if (!is.data.frame(df)) {
    stop("Read data is not a dataframe.", call. = FALSE)
  }
  
  class(df) <- union(c("datasource"), class(df))
  class(df) <-switch(tolower(trimws(socialmedia)),
    twitter = { union(c("twitter"), class(df)) },
    youtube = { union(c("youtube"), class(df)) },
    reddit = { union(c("reddit"), class(df)) },
    stop("Unknown social media type provided as datasource.", call. = FALSE))
  
  df
}
