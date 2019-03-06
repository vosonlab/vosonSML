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

# write data to file in supported file format
writeOutputFile <- function(data, type = "rds", name = "File", datetime = TRUE, msg = TRUE) {
  
  supported_types <- c("graphml", "csv", "rds")
  
  if (!type %in% supported_types) {
    cat(paste0("File output not supported. please choose from: ", paste0(supported_types, collapse = ", "), "\n"))
    return(NULL)
  }
  
  if (datetime) {
    name <- systemTimeFilename(name, type) 
  }
  
  path <- paste0(getwd(), "/", name)
  con <- file(path, open = "wb", encoding = "native.enc")
  result <- tryCatch({
    switch(type,
      "graphml" = {
        write.graph(data, file = con, format = "graphml")
      }, "csv" = {
        write.csv(data, file = con)
      }, {
        saveRDS(data, file = con)
      })
  }, error = function(e) {
    cat(paste0("Error writing: ", path, "\n", e))
  }, finally = { })
  close(con)
  
  if (length(result) == 0) {
    if (msg) {
      cat(paste0(toupper(type), " file written: "))
      cat(paste0(path, "\n"))
    }    
  } else {
    if (msg) {
      cat(paste0("File unable to be written.\n"))
    }
  }
}

# installs and loads a package if necessary
EnsurePackage <- function(x) {
  x <- as.character(x)
  if (!require(x, character.only = TRUE)) {
    install.packages(pkgs = x, repos = "http://cran.r-project.org")
    require(x, character.only = TRUE)
  }
}

quiet <-function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

# get the length of the longest character value in a list
maxCharLength <- function(data) { 
  i <- 0
  sapply(data, function(x) { if (nchar(as.character(x)) > i) i <<- nchar(x) })
  return(i)
}

# pad a character string with spaces
padChar2Length <- function(value, length) {
  value_length <- nchar(as.character(value))
  if (value_length < length) {
    return(paste0(value, paste0(replicate(length - value_length, ""), collapse = " "), " "))
  } else {
    return(as.character(value))
  }
}

# format and print a data frame
printResultTable <- function(df) {
  col_names <- colnames(df)
  col_len <- sapply(col_names, nchar)
  
  for (i in 1:length(col_names)) {
    temp_len <- maxCharLength(df[[i]])
    col_len[i] <- ifelse(temp_len > col_len[i], temp_len, col_len[i])
  }
  
  header <- paste0(sapply(col_names, function(x) padChar2Length(x, col_len[match(c(x), col_names)])), collapse = " | ")
  header <- paste0(header, "\n", paste0(replicate(nchar(header), ""), collapse = "-"), "-\n")
  cat(header)
  for (i in 1:nrow(df)) {
    line <- ""
    values <- as.character(as.vector(df[i, ]))
    for (j in 1:length(values)) {
      line <- paste0(line, padChar2Length(values[j], col_len[j]), ifelse(j < length(values), " | ", ""))
    }
    cat(paste0(line, "\n"))
  }
}
