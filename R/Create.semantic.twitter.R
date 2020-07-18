#' @title Create twitter semantic network
#' 
#' @description Creates a semantic network from tweets returned from the twitter search query. Semantic networks 
#' describe the semantic relationships between concepts. In this network the concepts are significant words and 
#' hashtags extracted from the tweet text. Network edges are weighted and represent occurrence of words and
#' hashtags in the same tweets.
#' 
#' The creation of twitter semantic networks requires text processing and the tokenization of tweets. As such
#' this function requires the additional installation of the \pkg{tidyr} and \pkg{tidytext} packages to achieve
#' this.
#' 
#' @note The words and hashtags passed to the function in the \code{removeTermsOrHashtags} parameter are removed
#' before word frequencies are calculated and are therefore excluded from top percentage of most frequent terms
#' completely rather than simply filtered out of the final network.
#' 
#' The top percentage of frequently occurring hashtags \code{hashtagFreq} and words \code{termFreq} are calculated to a
#' minimum frequency and all terms that have an equal or greater frequency than the minimum are included in the network
#' as nodes. For example, of unique hashtags of varying frequencies in a dataset the top 50% of total
#' frequency or most common hashtags may calculate to being the first 20 hashtags. The frequency of the 20th hashtag is
#' then used as the minimum and all hashtags of equal or greater frequency are included as part of the top 50%
#' most frequently occurring hashtags. So the number of top hashtags may end up being greater than 20 if there is more
#' than one hashtag that has frequency matching the minimum. The exception to this is if the minimum frequency is 1
#' and the \code{hashtagFreq} is set to less than 100, in this case only the first 20 hashtags will be included.
#' 
#' Hashtags and words in the top percentages are included in the network as isolates if there are no instances of
#' them occurring in tweet text with other top percentage frequency terms.
#' 
#' @param datasource Collected social media data with \code{"datasource"} and \code{"twitter"} class names.
#' @param type Character string. Type of network to be created, set to \code{"semantic"}.
#' @param removeTermsOrHashtags Character vector. Words or hashtags to remove from the semantic network. For example, 
#' this parameter could be used to remove the search term or hashtag that was used to collect the data by removing any
#' nodes with matching name. Default is \code{NULL} to remove none.
#' @param stopwords Logical. Removes stopwords from the tweet data. Default is \code{TRUE}.
#' @param stopwordsLang Character string. Language of stopwords to use. Refer to the \pkg{stopwords} package for
#' further information on supported languages. Default is \code{"en"}.
#' @param stopwordsSrc Character string. Source of stopwords list. Refer to the \pkg{stopwords} package for
#' further information on supported sources. Default is \code{"smart"}.
#' @param removeNumbers Logical. Removes whole numerical tokens from the tweet text. For example, a year value
#' such as \code{2020} will be removed but not mixed values such as \code{G20}. Default is \code{TRUE}.
#' @param removeUrls Logical. Removes twitter shortened URL tokens from the tweet text. Default is \code{TRUE}.
#' @param termFreq Numeric integer. Specifies the percentage of most frequent words to include. For example,
#' \code{termFreq = 20} means that the 20 percent most frequently occurring \code{words} will be included in the 
#' semantic network as nodes. A larger percentage will increase the number of nodes and therefore the size of graph. 
#' The default value is \code{5}, meaning the top 5 percent most frequent words are used.
#' @param hashtagFreq Numeric integer. Specifies the percentage of most frequent \code{hashtags} to include. For 
#' example, \code{hashtagFreq = 20} means that the 20 percent most frequently occurring hashtags will be included 
#' in the semantic network as nodes. The default value is \code{50}.
#' @param assoc Character string. Association of nodes. A value of \code{"limited"} includes only edges between
#' most frequently occurring hashtags and terms. A value of \code{"full"} includes ties between most frequently
#' occurring hashtags and terms, hashtags and hashtags, and terms and terms. Default is \code{"limited"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{FALSE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#' 
#' @examples
#' \dontrun{
#' # twitter semantic network creation additionally requires the tidyr, tidytext and stopwords packages
#' # for working with text data
#' install.packages(c("tidyr", "tidytext", "stopwords"))
#' 
#' # create a twitter semantic network graph removing the hashtag '#auspol' and using the
#' # top 2% frequently occurring words and 10% most frequently occurring hashtags as nodes
#' semanticNetwork <- twitterData %>% 
#'                    Create("semantic", removeTermsOrHashtags = c("#auspol"),
#'                           termFreq = 2, hashtagFreq = 10, verbose = TRUE)
#' 
#' # network
#' # semanticNetwork$nodes
#' # semanticNetwork$edges
#' }
#' 
#' @export
Create.semantic.twitter <- function(datasource, type, removeTermsOrHashtags = NULL,
                                    stopwords = TRUE, stopwordsLang = "en", stopwordsSrc = "smart",
                                    removeNumbers = TRUE, removeUrls = TRUE,
                                    termFreq = 5, hashtagFreq = 50,
                                    assoc = "limited",
                                    verbose = FALSE, ...) {
  
  cat("Generating twitter semantic network...")
  if (verbose) { cat("\n") }

  requiredPackages <- sapply(c("tidyr", "tidytext"), function(x) { requireNamespace(x, quietly = TRUE) })  
  if (any(requiredPackages == FALSE)) {
    stop(paste0("Please install ", paste0(names(which(requiredPackages == FALSE)), collapse = ', '),
                " package", ifelse(length(which(requiredPackages == FALSE)) > 1, "s", ""),
                " before calling Create.semantic.twitter.", call. = FALSE))
  }
  
  isPerc <- function(x, desc) {
    if (!is.numeric(x) || x > 100 || x < 1) {
      stop(paste0(desc, " must be a number between 1 and 100."), call. = FALSE)
    }
  }
  
  isPerc(termFreq, "termFreq")
  isPerc(hashtagFreq, "hashtagFreq")
  
  if (stopwords) {
    tryCatch({
      rem_stopwords <- tidytext::get_stopwords(language = stopwordsLang, source = stopwordsSrc)
    }, error = function(e) {
      stop(gsub("^Error:\\s", "", paste0(e)), call. = FALSE)
    })
  }
  
  if (verbose) { df_stats <- networkStats(NULL, "collected tweets", nrow(datasource)) }
  
  # datasource <- tibble::as_tibble(datasource)
  class(datasource) <- rmCustCls(class(datasource))
  
  datasource <- datasource %>% dplyr::select(.data$status_id, .data$text, .data$hashtags)
  datasource$text = HTMLdecode(datasource$text)
  
  capture.output(
    tokens_df <- datasource %>% tidytext::unnest_tokens(.data$word, .data$text, token = "tweets", to_lower = TRUE)
  , type = "output")
  
  if (verbose) { df_stats <- networkStats(df_stats, "tokens", nrow(tokens_df), FALSE) }

  # removal of words and hashtags before frequencies are calculated
  # in future may want an option to do this at the end to simply filter words from result set
  if (!is.null(removeTermsOrHashtags) && length(removeTermsOrHashtags) > 0) {
    removeTermsOrHashtags <- unlist(lapply(removeTermsOrHashtags, tolower))
    token_count <- nrow(tokens_df)
    if (verbose) {
      cat(paste0("Removing terms and hashtags: ", paste0(as.character(removeTermsOrHashtags), collapse = ", "), "\n"))
    }
    tokens_df %<>% dplyr::filter(!(.data$word %in% removeTermsOrHashtags))
    
    if (verbose) {
      df_stats <- networkStats(df_stats, "removed specified", token_count - nrow(tokens_df), FALSE)
    }
  }
  
  if (stopwords) {
    if (verbose) { cat("Removing stopwords.\n") }
    capture.output(
      tokens_df %<>% dplyr::anti_join(rem_stopwords)
    , type = "output")
  }
  
  freq_df <- tokens_df %>% dplyr::count(.data$word, sort = TRUE)
  # clasify words as hashtags, users, numbers, urls and terms
  freq_df %<>% dplyr::mutate(type = if_else(grepl("^#.*", .data$word), "hashtag", 
                                    if_else(grepl("^@.*", .data$word), "user", 
                                    if_else(grepl("^[[:digit:]]+$", .data$word), if_else(removeNumbers, "number", "term"),
                                    if_else(grepl("^https?://t\\.co/", .data$word), if_else(removeUrls, "url", "term"), 
                                    "term")))))
  
  if (removeNumbers) { freq_df %<>% dplyr::filter(type != "number") }
  if (removeUrls) { freq_df %<>% dplyr::filter(type != "url") }
  
  type_tally <- freq_df %>% dplyr::group_by(type) %>% dplyr::tally(n) %>% tibble::deframe()
  
  # remove users
  freq_df %<>% dplyr::filter(type != "user")
  
  if (verbose) {
    # tidytext unnest_tokens is changing hashtags starting with digits such as #9news to words i.e 9news
    # this causes a discrepancy between generated tokens and original hashtags data field count
    
    unique_hashtags <- nrow(freq_df %>% dplyr::filter(type == "hashtag"))
  }
  
  if (verbose) {
    if ("user" %in% names(type_tally)) {
      df_stats <- networkStats(df_stats, "removed users", type_tally[["user"]], FALSE)
    }
    if ("hashtag" %in% names(type_tally)) {
      df_stats <- networkStats(df_stats, "hashtag count", type_tally[["hashtag"]], FALSE)
    }
    df_stats <- networkStats(df_stats, "unique hashtags", unique_hashtags, FALSE)
  }
  
  rm_words <- function(rm_type, keep_perc) {
    type_df <- freq_df %>% dplyr::filter(type == rm_type) %>% dplyr::arrange(dplyr::desc(n))
    
    if (nrow(type_df) > 0) {
      keep_count <- round(((nrow(type_df) / 100) * keep_perc), digits = 0)
      n_value <- type_df[keep_count, ]$n
      
      # keep top number of rows
      if (n_value <= 1 & keep_perc != 100) {
        rm_values <- type_df %>% dplyr::slice(keep_count + 1:n())
        if (verbose) {
          df_stats <<- networkStats(df_stats, paste0("top ", keep_perc , "% ", rm_type, "s"), keep_count, FALSE)
        }
      } else {
      
        # keep tokens above n value cutoff
        rm_values <- type_df %>% dplyr::filter(.data$n < n_value)
        if (verbose) {
          df_stats <<- networkStats(df_stats, paste0("top ", keep_perc , "% ", rm_type, "s (freq>=", n_value, ")"),
                                    nrow(type_df %>% dplyr::filter(.data$n >= n_value)), FALSE)
        }
      }
      
      freq_df %>% dplyr::filter(!(.data$word %in% rm_values$word))
    } else {
      freq_df
    }
  }
  
  # keep top % of hashtags and terms
  freq_df <- rm_words("hashtag", hashtagFreq)
  
  if (verbose) {
    if ("term" %in% names(type_tally)) { df_stats <- networkStats(df_stats, "term count", type_tally[["term"]], FALSE) }
    
    unique_terms <- nrow(dplyr::distinct(freq_df %>% dplyr::filter(type == "term"), .data$word))
    df_stats <- networkStats(df_stats, "unique terms", unique_terms, FALSE)
  }
  
  freq_df <- rm_words("term", termFreq) 
  freq_df %<>% dplyr::arrange(dplyr::desc(n))
  
  if (tolower(assoc) == "full") {
    edges <- dplyr::inner_join((tokens_df %>% dplyr::select(-.data$hashtags)), 
                              (freq_df %>% dplyr::select(-.data$n, -.data$type)), by = "word")
    
    edges <- dplyr::inner_join(edges, (edges %>% dplyr::select(.data$status_id, .data$word)), by = "status_id") %>%
      dplyr::group_by(.data$status_id) %>%
      dplyr::filter(.data$word.x != .data$word.y) %>%
      dplyr::ungroup() %>%
      
      dplyr::select(-.data$status_id) %>%
      dplyr::mutate(from = .data$word.x, to = .data$word.y) %>%
      group_by(.data$from, .data$to) %>%
      summarise(weight = dplyr::n())
  } else {
    keep_hashtags <- freq_df %>% dplyr::filter(type == "hashtag")
    keep_terms <- freq_df %>% dplyr::filter(type == "term")
    
    class(tokens_df) <- c("tbl_df", "tbl", "data.frame") # unnest had a problem with vosonsml classes in class list
      
    edges <- dplyr::inner_join(tokens_df, freq_df, by = "word") %>%
      tidyr::unnest(.data$hashtags) %>%
      dplyr::mutate(hashtags = ifelse(is.na(.data$hashtags), NA, paste0("#", tolower(.data$hashtags))))
    
    edges %<>% dplyr::filter(.data$hashtags %in% unique(keep_hashtags$word) &
                              .data$word %in% unique(keep_terms$word)) %>%
      dplyr::select(-.data$n, -.data$type)
    
    edges %<>% dplyr::select(.data$hashtags, .data$word) %>%
      dplyr::mutate(from = .data$hashtags, to = .data$word) %>%
      group_by(.data$from, .data$to) %>% summarise(weight = dplyr::n())
  }
  
  if (verbose) {
    df_stats <- networkStats(df_stats, "nodes", nrow(freq_df))
    df_stats <- networkStats(df_stats, "edges", nrow(edges))
    
    networkStats(df_stats, print = TRUE)
  } 
  
  func_output <- list(
    "nodes" = freq_df,
    "edges" = edges
  )
  
  class(func_output) <- append(class(func_output), c("network", "semantic", "twitter"))
  cat("Done.\n")
  
  func_output
}
