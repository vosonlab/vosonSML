#' @title Import collected data previously saved to file
#'
#' @description Imports collected data from file or dataframe. Ensures \code{datasource} and specified 
#' \code{socialmedia} type are set so data is usable by \code{\link{Create}} functions. Not required
#' if collected data was collected by \code{vosonSML} and saved as an \code{rds} file, use \code{\link{readRDS}}
#' instead.
#' 
#' @param file Character string or dataframe. Collected data file path or dataframe object.
#' @param socialmedia Character string. Social media type of collected data \code{twitter}, \code{youtube} or 
#' \code{reddit}.
#' @param type Character string. Type of file or file format of file to import \code{csv} or \code{rds}. Default is 
#' \code{NULL} to use extension.
#' 
#' @return A dataframe with datasource and socialmedia class attributes.
#' 
#' @examples
#' \dontrun{
#' # import collected data from file
#' twitterData <- ImportData("./data.csv", "twitter")
#' 
#' # import collected data from dataframe
#' twitterData <- ImportData(rtweetData, "twitter")
#' 
#' @aliases ImportData
#' @name vosonSML::ImportData
#' @export
ImportData <- function(file, socialmedia, type = NULL) {
  
  # expected import types
  supported_types <- c("csv", "rds")
  
  if (missing(file) || (!is.character(file) && !inherits(file, "data.frame"))) {
    stop("Please provide file path or dataframe to import.", call. = FALSE) 
  }
  
  if (is.character(file) && !file.exists(file)) {
    stop("Import file not found.", call. = FALSE) 
  }
  
  if (missing(socialmedia)) {
    stop("Please provide the social media type of data to import.", call. = FALSE) 
  }
  
  if (!inherits(file, "data.frame")) {
    
    # if type is null check file extension
    if (is.null(type)) {
      type <- gsub(".*\\.([A-Za-z]{3})$", "\\1", file, ignore.case = TRUE, perl = TRUE)
    }
    
    type <- tolower(trimws(type))
    if (!type %in% supported_types) {
      stop(paste0("File format not supported. please choose from: ", paste0(supported_types, collapse = ", "), "."), 
           call. = FALSE) 
    }
    
    df <- tryCatch({
      switch(type,
        "csv" = read.csv(file),
        "rds" = readRDS(file = file))
      
    }, error = function(e) {
      stop(paste0("Could not read: ", file, ".\n", gsub("^Error:\\s", "", paste0(e))), call. = FALSE)
    })
    
    cat(paste0(toupper(type), " file read: ", file), "\n")
  } else {
    df <- file
  }
  
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
