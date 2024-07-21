# package version
get_version <- function() {
  if ("vosonSML" %in% loadedNamespaces()) return(utils::packageVersion("vosonSML"))
  "_"
}

# get package ua
vsml_ua <- function() {
  gum("vosonSML v{get_version()}",
      " (R{as.character(R.version['major'])}.{as.character(R.version['minor'])};",
      "{as.character(R.version['platform'])})")
}

# save collect opts
get_env_opts <- function() {
  opts <- list(
    enc = getOption("encoding"),
    ua = getOption("HTTPUserAgent"),
    tz = Sys.timezone()
  )
}

# set collect opts
set_collect_opts <- function(opts = NULL) {
  if (is.null(opts)) {
    options(encoding = "UTF-8")
    options(HTTPUserAgent = vsml_ua())
    Sys.setenv(TZ = "Etc/UTC")
  } else {
    options(encoding = opts$enc)
    options(HTTPUserAgent = opts$ua)
    Sys.setenv(TZ = opts$tz)
  }
  invisible()
}

# return a file name with system time prefix
sys_time_filename <- function(name_suffix, name_ext, current_time = NULL, clean = FALSE) {
    
    if (is.null(current_time)) current_time <- Sys.time()

    if (clean) {
      name_suffix <- stringr::str_replace_all(name_suffix, "[\\s:]", "_")
      name_ext <- stringr::str_remove_all(name_ext, "[\\s:\\.]")
    }

    paste0(format(current_time, "%Y-%m-%d_%H%M%S"), "-", name_suffix, ".", name_ext)
  }

data_path <- function(x = getOption("voson.data")) {
  if (is.null(x)) return(NULL)

  path <- gsub("\\\\", "/", x)
  path <- sub("(\\/)+$", "", path)
  path <- paste0(path, "/")

  if (!dir.exists(path)) dir.create(path, showWarnings = TRUE)
  if (dir.exists(path)) return(path)
  
  NULL
}

# check writeToFile option
write_opt <- function(opt = "voson.save", default = FALSE) {
  opt_save <- getOption(opt)
  if (!is.na(opt_save) && is.logical(opt_save)) return(opt_save)
  default
}

# write data to file in supported file format
write_output_file <-
  function(data,
           type = "rds",
           name = "File",
           datetime = TRUE,
           verbose = FALSE,
           log = NULL) {

    set_path <- function(x) x
    data_path <- data_path()
    if (!is.null(data_path)) {
      set_path <- function(x) paste0(data_path, x)
    }

    supported_types <- c("graphml", "csv", "rds")

    if (!type %in% supported_types) {
      msg(paste0(
        "File output not supported. please choose from: ",
        paste0(supported_types, collapse = ", "),
        "\n"
      ))
      return(NULL)
    }

    if (type == "csv") stop_req_pkgs(c("readr"))
    if (datetime) name <- sys_time_filename(name, type)

    path <- name

    if (type == "rds") {
      result <- tryCatch({
        saveRDS(data, set_path(path))
      }, error = function(e) {
        msg(paste0("Error writing rds: ", set_path(path), "\n", e))
      })
    } else {
      con <- file(set_path(path), open = "wb", encoding = "native.enc")
      result <- tryCatch({
        switch(type,
               "graphml" = {
                 igraph::write_graph(data, file = con, format = "graphml")
               },
               "csv" = {
                 readr::write_csv(data, file = con)
               })
      }, error = function(e) {
        msg(paste0("Error writing: ", set_path(path), "\n", e))
      }, finally = {

      })
      close(con)
    }

    if (length(result) == 0) {
      if (verbose) {
        msg(paste0(toupper(type), " file written: "))
        msg(paste0(set_path(path), "\n"))
      }
    } else {
      if (verbose) msg(paste0("File unable to be written.\n"))
    }
    
    # write a simple log file
    if (!is.null(log)) {
      log_path <- paste0(set_path(path), ".txt")
      tryCatch({
        con <- file(log_path)
        writeLines(paste0(log, collapse = "\n"), con)
        close(con)
      }, error = function(e) {
        msg(paste0("Error writing log file.\n", e))
      })
    }
  }

# get the length of the longest character value in a list
max_chrlen_in_list <- function(x) max(stringr::str_length(x))

# pad a character string with spaces
pad_to_len <- function(x, to_len) stringr::str_pad(x, to_len, side = "right")

# format and print summary dataframe
print_summary <- function(df) {
  col_names <- colnames(df)
  col_len <- sapply(col_names, nchar)

  for (i in 1:length(col_names)) {
    df[[i]] <- sapply(df[[i]], function(x) {
      ifelse(is.na(x), "-", x)
    })
    temp_len <- max_chrlen_in_list(df[[i]])
    col_len[i] <- ifelse(temp_len > col_len[i], temp_len, col_len[i])
  }

  header <- paste0(
    sapply(col_names, function(x) {
      pad_to_len(x, col_len[match(c(x), col_names)])
    }), collapse = " | ")

  header <-
    paste0(header, "\n", paste0(replicate(nchar(header), ""), collapse = "-"), "-\n")

  lines <- ""
  for (i in 1:nrow(df)) {
    line <- ""
    values <- as.character(as.vector(df[i,]))
    for (j in 1:length(values)) {
      line <- paste0(line, pad_to_len(values[j], col_len[j]), ifelse(j < length(values), " | ", ""))
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
        tibble::tibble(
          "field" = character(0),
          "count" = character(0),
          "edge_count" = character(0)
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

# check packages and prompt to install if interactive
# stop if not installed
prompt_and_stop <- function(pkgs, f) {
  rlang::check_installed(pkgs, paste0("for ", f))
  stop_req_pkgs(pkgs, f)
}

# check for required packages and stop with a message if missing
stop_req_pkgs <- function(pkgs, from = "this function") {
  # load the namespace of pkgs loadedNamespaces()
  req <- sapply(pkgs, function(x) requireNamespace(x, quietly = TRUE))
  
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

## -- output messaging

# shorter glue
gum <- stringr::str_glue
gum_data <- stringr::str_glue_data

msg_check_opts <- function(opt = "voson.cat", default = FALSE) {
  opt_cat <- getOption(opt)
  if (!is.na(opt_cat) && is.logical(opt_cat)) return(opt_cat)
  default
}

# check voson.cat options
msg_use_cat <- function() {
  opt_msg <- msg_check_opts(opt = "voson.msg") # previous
  opt_cat <- msg_check_opts(opt = "voson.cat")
  
  if (opt_msg == FALSE & opt_cat == TRUE) return(TRUE)
  FALSE
}

# base output function
msg <- function(x, ..., .x = "-", .xenvir = parent.frame(), .verbose = NULL) {
  
  if (!is.null(.verbose)) {
    verbose <- .verbose
  } else {
    verbose <- .xenvir$verbose
  }

  # if not verbose only continue if warning or danger
  if (is.null(verbose) || verbose == FALSE) {
    if (!.x %in% c("warning", "danger")) return(invisible(x))
  }
  
  # interpolate
  # x_ <- gum(x, ..., .envir = .xenvir)
  x_ <- x
  
  if (!msg_use_cat()) {
    vsml_msg(x_, .x, appendLF = FALSE)
    
    # if cat output
  } else {
    vsml_cat(x_, .x)
  }
  
  invisible(x_)
}

# message output
vsml_msg <- function(x, .x = "-", ...) {
  if (.x %in% c("warning", "danger")) {
    warning(x, call. = FALSE, noBreaks. = TRUE)
  } else {
    # appendLF = FALSE
    message(x, ...)
  }
  invisible(x)
}

# cat output
vsml_cat <- function(x, .x = "-") {
  if (.x %in% c("warning", "danger")) x <- paste0("!", .x, ":", x)
  cat(x)
  flush.console()
  Sys.sleep(0.05)
  invisible(x)
}

# check debug parameter
lgl_debug <- function(x) {
  lgl <- FALSE
  if (!is.null(x)) {
    if (is.logical(x)) lgl <- x
  }

  lgl
}

## -- check input values

# check logical input
check_lgl <- function(x, param = "value") {
  if (!is.null(x)) {
    if (!is.na(x)) {
      if (is.logical(x)) return(x)
    }
  }

  stop(paste0(param, " must be logical."), call. = FALSE)
}

# check numeric input
check_num <-
  function(x,
           param = "value",
           double = FALSE,
           null.ok = FALSE,
           na.ok = FALSE,
           inf.ok = FALSE,
           gte = NULL) {

    if (null.ok == FALSE) {
      if (any(is.null(x))) {
        stop(paste0(param, " must be numeric with no NULL values."), call. = FALSE)
      }
    }
    
    y <- x[!sapply(x, is.null)]
    if (length(y) < 1) return(x)

    if (na.ok == FALSE) {
      if (any(is.na(y))) {
        stop(paste0(param, " must be numeric with no NA values."), call. = FALSE)
      }
    }
    
    z <- y[!sapply(y, is.na)]
    if (length(z) < 1) return(x)
    
    if (any(!is.numeric(z))) {
      stop(paste0(param, " must be numeric."), call. = FALSE)
    }
    
    if (inf.ok == FALSE) {
      if (any(is.infinite(z))) {
        stop(paste0(param, " must be numeric with no Inf values."), call. = FALSE)
      }
    }

    zz <- z[!sapply(z, is.infinite)]
    if (length(zz) < 1) return(x)
    
    if (double == FALSE) {
      if (!all.equal(zz, as.integer(zz))) {
        stop(paste0(param, " must be numeric with no double type."), call. = FALSE)
      }
    }
    
    if (!is.null(gte)) {
      if (any(zz < gte)) {
        stop(paste0(param, " must be greater than or equal to ", gte, "."), call. = FALSE)
      }
    }
    
    x
  }

# check percentage
check_perc <- function(x, param = "value") {
  if (all(is.numeric(x))) {
    if (all(x >= 0) & all(x <= 100)) {
      return(round(x, digits = 0))
    }
  }

  stop(paste0(param, " must be a number between 0 and 100."), call. = FALSE)
}

# extract a value from dots parameter
check_dots <- function(x, ...) {
  as.list(match.call(expand.dots = TRUE))[[x]]
}

# check is dataframe and n rows
check_df_n <- function(x) {
  if (!inherits(x, "data.frame")) return(-1)

  nrow(x)
}

# compare values
cmp_values <- function(x, y, param = "value", n = NULL, lc = TRUE) {
  if (!is.null(n)) {
    if (!is.vector(x) || !length(x) %in% c(1, n)) {
      stop(paste0("Please provide a ", param, " value that is length 1 or ", n, "."), call. = FALSE)
    }
  }
  
  if (lc) x <- tolower(x)
  if (!all(x %in% y)) {
    stop(paste0("Please provide ", param, " values in ", paste0(y, collapse = ", "), "."), call. = FALSE)
  }
}

# check character input
check_chr <-
  function(x,
           param = "value",
           min = NULL,
           accept = c(),
           case = "lower",
           null.ok = FALSE,
           na.ok = FALSE) {

    if (all(!is.null(x))) {
      if (all(!is.na(x))) {
        if (all(is.character(x))) {
          if (!is.null(min)) {
            if (any(nchar(x) < min)) {
              if (min == 1) {
                stop(paste0(param, " must not be an empty string."), call. = FALSE)
              } else {
                stop(paste0(param, " must be ", min, " or more characters long."), call. = FALSE)
              }

            }
          }

          if (length(accept)) {
            if (case == "lower") {
              x <- tolower(x)
            } else if (case == "upper") {
              x <- toupper(x)
            }
            if (all(x %in% accept)) {
              return(trimws(x))
            } else {
              stop(paste0(param, " must be in ", paste0(accept, collapse = ","), "."), call. = FALSE)
            }
          }

          return(trimws(x))
        }
      } else {
        if (na.ok) return(x)
      }
    } else {
      if (null.ok) return(x)
    }

    stop(paste0(param, " must be of character type."), call. = FALSE)
  }

rev_row_order <- function(x) {
  x[nrow(x):1, ]
}
