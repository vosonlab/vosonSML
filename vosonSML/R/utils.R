# vosonSML utils

# check for a range of true values
isTrueValue <- function(x) {
  if (x == "TRUE" || x == "true" || x == "T" || x == TRUE) {
    return(TRUE)
  }
  return(FALSE)
}

# return a friendly file name with system time prefix
systemTimeFilename <- function(name_suffix, name_ext, current_time = NULL, clean = FALSE) {
  if (missing(current_time) || is.null(current_time)) {
    current_time <- Sys.time()
  }
  
  if (!missing(clean) && clean == TRUE) {
    name_suffix <- gsub("\\s+", "_", name_suffix, perl = TRUE)
    name_suffix <- gsub(":", "_", name_suffix, perl = TRUE)
    
    name_ext <- gsub("\\s+", "", name_ext, perl = TRUE)
    name_ext <- gsub(":", "", name_ext, perl = TRUE)  
    name_ext <- gsub("\\.", "", name_ext, perl = TRUE)    
  }
  
  file_name <- paste0(format(current_time, "%Y-%m-%d_%H%M%S"), "-", name_suffix, ".", name_ext, sep = "")
}

# write data to file as type
writeOutputFile <- function(data, type, name, msg = TRUE) {
  # package <- environmentName(environment(writeOutputFile))
  
  if (missing(type)) {
    type <- "rds"
  }
  
  supported_types <- c("graphml", "csv", "rds")
  
  if (!type %in% supported_types) {
    cat(paste0("File output not supported. please choose from: ", paste0(supported_types, collapse = ", "), "\n"))
    return(NA)
  }
  
  if (missing(name)) {
    name <- "File"
  }
  
  name <- systemTimeFilename(name, type)
  
  if (missing(msg)) {
    msg <- TRUE
  }
  
  path <- paste0(getwd(), "/", name, "\n", sep = "")
  
  result <- tryCatch({
    switch(type,
           "graphml" = write.graph(data, name, format = "graphml"),
           "csv" = write.csv(data, name),
           saveRDS(data, file = name))
    
    if (msg) {
      cat(paste0(toupper(type), " file written: "))
      cat(path)
    }
  },
  error = function(e) {
    cat(paste0("Error writing: ", path, "\n  "))
    message(e)
    return(NULL)
  })
}
