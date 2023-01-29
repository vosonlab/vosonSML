# is stringi international components for unicode (ICU) standard version < 72.0
check_stri_icu_version <- function() {
  info <- stringi::stri_info()
  if (info$ICU.version >= 72.0) {
    stop(
      paste0(
        "Unicode ICU (International Components for Unicode) standard version ",
        info$ICU.version,
        " is not currently supported for semantic and 2mode networks in this version of vosonSML. (>= 72.0)"
      ),
      call. = FALSE
    )
  }
}

# unnest_tweets method adapted from the tidytext v0.3.4 package on github
# https://github.com/juliasilge/tidytext/blob/v0.3.4/R/unnest_tweets.R
# original function authored by @juliasilge tidytext
# removed from tidytext v0.4
unnest_tweets <- function(tbl,
                          output,
                          input,
                          strip_punct = TRUE,
                          strip_url = FALSE,
                          to_lower = TRUE,
                          drop = TRUE,
                          ...) {
  output <- rlang::enquo(output)
  input <- rlang::enquo(input)
  
  col <- dplyr::pull(tbl,!!input)
  
  output_lst <- tokenize_tweets(col, lowercase = to_lower, ...)
  
  output <- rlang::quo_name(output)
  input <- rlang::quo_name(input)
  
  tbl_indices <- vctrs::vec_rep_each(seq_len(nrow(tbl)), lengths(output_lst))
  ret <- vctrs::vec_slice(tbl, tbl_indices)
  ret[[output]] <- rlang::flatten_chr(output_lst)
  
  if (to_lower) ret[[output]] <- stringr::str_to_lower(ret[[output]])
  if (drop && output != input) ret[[input]] <- NULL
  
  ret
}

# tokenize_tweets methods from the ropensci tokenizers v0.2.1 package on github
# https://github.com/ropensci/tokenizers/blob/v0.2.1/R/tokenize_tweets.R
# original functions authored by @kbenoit @lmullen tokenizers
# removed from tokenizers v0.3

# ICU versions >=72 issue yet to be resolved
# https://github.com/ropensci/tokenizers/issues/82
tokenize_tweets <- function(x,
                            lowercase = TRUE,
                            stopwords = NULL,
                            strip_punct = TRUE,
                            strip_url = FALSE,
                            simplify = FALSE, ...) {
  
  named <- names(x)
  
  # split on white space
  out <- stringi::stri_split_charclass(x, "\\p{WHITE_SPACE}")
  
  # get document indexes to vectorize tokens
  doc_lengths <- cumsum(lengths(out))
  docindex <- c(0, doc_lengths)
  # convert the list into a vector - avoids all those mapplys
  out <- unlist(out)
  
  # get the index of twitter hashtags and usernames
  index_twitter <- stringi::stri_detect_regex(out, "^#[A-Za-z]+\\w*|^@\\w+")
  # get the index of http(s) URLs
  index_url <- stringi::stri_detect_regex(out, "^http")
  
  if (strip_url) {
    out[index_url] <- ""
  }
  
  if (lowercase) {
    out[!(index_twitter | index_url)] <-
      stringi::stri_trans_tolower(out[!(index_twitter | index_url)])
  }
  
  if (!is.null(stopwords))
    out <- sapply(out, remove_stopwords, stopwords, USE.NAMES = FALSE)
  
  if (strip_punct) {
    # split all except URLs, twitter hashtags, and usernames into words.
    out[!index_url & !index_twitter] <-
      stringi::stri_split_boundaries(out[!index_url & !index_twitter], type = "word")
    
    out[!index_url & !index_twitter] <-
      lapply(out[!index_url & !index_twitter], function(toks) {
        stringi::stri_replace_all_charclass(toks, "\\p{P}", "")
      })
    
    # preserve twitter characters, strip all punctuations,
    # then put back the twitter characters.
    twitter_chars <- stringi::stri_sub(out[index_twitter], 1, 1)
    out[index_twitter] <-
      stringi::stri_replace_all_charclass(out[index_twitter], "\\p{P}", "")
    out[index_twitter] <- paste0(twitter_chars, out[index_twitter])
  }
  else {
    # split all except URLs.
    out[!index_url] <-
      stringi::stri_split_boundaries(out[!index_url], type = "word")
    # rejoin the hashtags and usernames
    out[index_twitter] <-
      lapply(out[index_twitter], function(toks) {
        toks[2] <- paste0(toks[1], toks[2])
        toks[-1]
      })
  }
  
  # convert the vector back to a list
  out <- split(out,
               cut(
                 seq_along(out),
                 docindex,
                 include.lowest = TRUE,
                 labels = named
               ))
  # in case !strip_punct, otherwise has no effect
  out <- lapply(out, unlist)
  
  names(out) <- named
  
  # remove any blanks (from removing URLs)
  out <- lapply(out, function(toks)
    toks[toks != "" & !is.na(toks)])
  
  simplify_list(out, simplify)
}

is_corpus_df <- function(corpus) {
  stopifnot(inherits(corpus, "data.frame"),
            ncol(corpus) >= 2,
            all(names(corpus)[1L:2L] == c("doc_id", "text")),
            is.character(corpus$doc_id),
            is.character(corpus$doc_id),
            nrow(corpus) > 0)
  TRUE # if it doesn't fail from the tests above then it fits the standard
}

corpus_df_as_corpus_vector <- function(corpus) {
  if (is_corpus_df(corpus)) {
    out <- corpus$text
    names(out) <- corpus$doc_id
  } else {
    stop("Not a corpus data.frame")
  }
  out
}

simplify_list <- function(x, simplify) {
  stopifnot(is.logical(simplify))
  if (simplify && length(x) == 1) x[[1]] else x
}

remove_stopwords <- function(x, stopwords) {
  out <- x[!x %in% stopwords]
  if (!length(out)) return(NA_character_)
  out
}
