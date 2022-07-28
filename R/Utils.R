#' @title Set function output to use the cat method instead of message
#'
#' @param flag Logical. Set the \code{voson.msg = "cat"} option. Set to \code{FALSE} or \code{NULL} to
#'   clear. Default is \code{TRUE}.
#'
#' @aliases SetOutputCat
#' @name SetOutputCat
#' @export
SetOutputCat <- function(flag = TRUE) {
  if (is.null(flag)) {
    options(voson.msg = NULL)
  }

  if (is.logical(flag)) {
    if (flag) {
      options(voson.msg = "cat")
    } else {
      options(voson.msg = NULL)
    }
  }
}

# package version
get_version <- function() {
  if ("vosonSML" %in% loadedNamespaces()) {
    return(utils::packageVersion("vosonSML"))
  }
  "_"
}

# return a file name with system time prefix
sys_time_filename <-
  function(name_suffix,
           name_ext,
           current_time = NULL,
           clean = FALSE) {
    if (is.null(current_time)) {
      current_time <- Sys.time()
    }

    if (clean) {
      name_suffix <- stringr::str_replace_all(name_suffix, "[\\s:]", "_")
      name_ext <- stringr::str_remove_all(name_ext, "[\\s:\\.]")
    }

    paste0(format(current_time, "%Y-%m-%d_%H%M%S"),
           "-",
           name_suffix,
           ".",
           name_ext)
  }

# write data to file in supported file format
write_output_file <-
  function(data,
           type = "rds",
           name = "File",
           datetime = TRUE,
           verbose = FALSE) {

    msg <- f_verbose(verbose)

    supported_types <- c("graphml", "csv", "rds")

    if (!type %in% supported_types) {
      msg(paste0(
        "File output not supported. please choose from: ",
        paste0(supported_types, collapse = ", "),
        "\n"
      ))
      return(NULL)
    }

    if (type == "csv") {
      stop_req_pkgs(c("readr"))
    }

    if (datetime) {
      name <- sys_time_filename(name, type)
    }

    path <- name

    if (type == "rds") {
      result <- tryCatch({
        saveRDS(data, path)
      }, error = function(e) {
        msg(paste0("Error writing rds: ", path, "\n", e))
      })
    } else {
      con <- file(path, open = "wb", encoding = "native.enc")
      result <- tryCatch({
        switch(type,
               "graphml" = {
                 igraph::write_graph(data, file = con, format = "graphml")
               },
               "csv" = {
                 readr::write_csv(data, file = con)
               })
      }, error = function(e) {
        msg(paste0("Error writing: ", path, "\n", e))
      }, finally = {

      })
      close(con)
    }

    if (length(result) == 0) {
      if (verbose) {
        msg(paste0(toupper(type), " file written: "))
        msg(paste0(path, "\n"))
      }
    } else {
      if (verbose) {
        msg(paste0("File unable to be written.\n"))
      }
    }
  }

# get the length of the longest character value in a list
max_chrlen_in_list <- function(lst) {
  i <- 0
  sapply(lst, function(x) {
    if (nchar(as.character(x)) > i)
      i <<- nchar(x)
  })
  i
}

# pad a character string with spaces
pad_to_len <- function(value, to_len) {
  value_len <- nchar(as.character(value))
  if (value_len < to_len) {
    return(paste0(value, paste0(
      replicate(to_len - value_len, ""), collapse = " "
    ), " "))
  }
  as.character(value)
}

# format and print summary dataframe
print_summary <- function(df) {
  col_names <- colnames(df)
  col_len <- sapply(col_names, nchar)

  for (i in 1:length(col_names)) {
    df[[i]] <- sapply(df[[i]], function(x) {
      ifelse(is.na(x), "-", x)
    })
    temp_len <- max_chrlen_in_list(df[[i]])
    col_len[i] <-
      ifelse(temp_len > col_len[i], temp_len, col_len[i])
  }

  header <-
    paste0(sapply(col_names, function(x) {
      pad_to_len(x, col_len[match(c(x), col_names)])
    }), collapse = " | ")

  header <-
    paste0(header, "\n", paste0(replicate(nchar(header), ""), collapse = "-"), "-\n")

  lines <- ""
  for (i in 1:nrow(df)) {
    line <- ""
    values <- as.character(as.vector(df[i, ]))
    for (j in 1:length(values)) {
      line <-
        paste0(line,
               pad_to_len(values[j], col_len[j]),
               ifelse(j < length(values), " | ", ""))
    }
    lines <- paste0(lines, line, "\n")
  }

  paste0(header, lines)
}

# accepts a dataframe to add or increment a field value with count
# if print is true then prints formatted field values
network_stats <-
  function(df,
           field,
           count,
           edge = FALSE,
           print = FALSE) {
    if (print == TRUE) {
      out <- ""
      if (!is.null(df) && nrow(df) > 0) {
        out <- paste0(out, "-------------------------\n")
        lf <- lc <- 0
        for (i in 1:nrow(df)) {
          lf <- ifelse(nchar(df$field[i]) > lf, nchar(df$field[i]), lf)
          lc <-
            ifelse(nchar(df$count[i]) > lc, nchar(df$count[i]), lc)
        }

        for (i in 1:nrow(df)) {
          lfm <- lf
          if (nchar(df$field[i]) != lf) {
            lfm <- lf + 1
          }
          line <-
            paste0(df$field[i], paste0(replicate(lfm - nchar(df$field[i]), ""), collapse = " "), " | ")
          line <-
            paste0(line, df$count[i], paste0(replicate(lc - nchar(df$count[i]), ""), collapse = " "), "\n")
          out <- paste0(out, line)
        }
        out <- paste0(out, "-------------------------\n")
      }

      return(out)
    }

    if (is.null(df)) {
      df  <-
        data.frame(
          "field" = character(0),
          "count" = character(0),
          "edge_count" = character(0),
          stringsAsFactors = FALSE
        )
    }
    rbind(df,
          list(
            field = field,
            count = count,
            edge_count = edge
          ),
          stringsAsFactors = FALSE)
  }

# remove collect classes from list
rm_collect_cls <- function(cls_lst) {
  cls_lst[!cls_lst %in% c("datasource", "twitter", "youtube", "reddit", "web")]
}

# check for required packages and stop with a message if missing
stop_req_pkgs <- function(pkgs, from = "this function") {
  # load the namespace of pkgs loadedNamespaces()
  req <-
    sapply(pkgs, function(x) {
      requireNamespace(x, quietly = TRUE)
    })
  if (any(req == FALSE)) {
    stop(
      paste0(
        "Please install ",
        paste0(names(which(req == FALSE)), collapse = ", "),
        " package",
        ifelse(length(which(req == FALSE)) > 1, "s", ""),
        " before calling ",
        from,
        ".",
        call. = FALSE
      )
    )
  }
}

# escape value for use as literal in a regex
escape_regex <- function(x) {
  gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", x)
}

# check debug parameter
lgl_debug <- function(x) {
  lgl <- FALSE
  if (!is.null(x)) {
    if (is.logical(x)) {
      lgl <- x
    }
  }

  lgl
}

# assign message function for verbose output
# can use options(voson.msg = "message")
f_verbose <- function(x) {
  opt_msg <- getOption("voson.msg")

  opt_f <- vsml_msg
  if (!is.null(opt_msg) && !is.na(opt_msg)) {
    if (is.character(opt_msg)) {
      if (tolower(opt_msg) == "cat") {
        opt_f <- vsml_cat
      }
    }
  }

  f <- vsml_silent
  if (!is.null(x)) {
    if (is.logical(x)) {
      f <- ifelse(x, opt_f, vsml_silent)
    }
  }
  f
}

# base function
msg <- function(x) {
  vsml_msg(x)
}

# message output
vsml_msg <- function(x) {
  message(x, appendLF = FALSE)
}

# cat output
vsml_cat <- function(x) {
  cat(x)
  flush.console()
  Sys.sleep(0.05)
}

# silent output
vsml_silent <- function(x) {
  return()
}

# check logical input
check_lgl <- function(x, param = "value") {
  if (!is.null(x)) {
    if (!is.na(x)) {
      if (is.logical(x)) {
        return(x)
      }
    }
  }

  stop(paste0(param, " must be logical."), call. = FALSE)
}

# check numeric input
check_num <- function(x, param = "value", double = FALSE, null.ok = FALSE, na.ok = FALSE) {
  if (!is.null(x)) {
    if (!is.na(x)) {
      if (is.numeric(x)) {
        if (is.double(x)) {
          if (double) {
            return(x)
          } else {
            stop(paste0(param, " must not be a double type."), call. = FALSE)
          }
        }

        return(x)
      }
    } else {
      if (na.ok) {
        return(x)
      }
    }
  } else {
    if (null.ok) {
      return(x)
    }
  }

  stop(paste0(param, " must be numeric."), call. = FALSE)
}

# check character input
check_chr <- function(x, param = "value", accept = c(), case = "lower", null.ok = FALSE, na.ok = FALSE) {
  if (!is.null(x)) {
    if (!is.na(x)) {
      if (is.character(x)) {
        if (length(accept)) {
          if (case == "lower") {
            x <- tolower(x)
          } else if (case == "upper") {
            x <- toupper(x)
          }
          if (x %in% accept) {
            return(trimws(x))
          } else {
            stop(paste0(param, " must be in ", paste0(accept, collapse = ",")), call. = FALSE)
          }
        }

        return(trimws(x))
      }
    } else {
      if (na.ok) {
        return(x)
      }
    }
  } else {
    if (null.ok) {
      return(x)
    }
  }

  stop(paste0(param, " must be a character value."), call. = FALSE)
}
