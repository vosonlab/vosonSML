#' @export
CreateSemanticNetwork.twitter <-
function(x,writeToFile,termFreq,hashtagFreq,removeTermsOrHashtags,stopwordsEnglish)
{

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (missing(stopwordsEnglish)) {
    stopwordsEnglish <- TRUE # default to true, because most English users will probably want this
  }

  if (missing(termFreq)) {
    termFreq <- 5 # default to the top 5% most frequent terms. reduces size of graph.
  }

  if (missing(hashtagFreq)) {
    hashtagFreq <- 50 # default to the top 50% hashtags. reduces size of graph. hashtags are 50% because they are much less frequent than terms.
  }

  if (!missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- as.vector(removeTermsOrHashtags) #coerce to vector... to be sure
  }

  if (missing(removeTermsOrHashtags)) {
    removeTermsOrHashtags <- "foobar"
  }


  df <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # if `df` is a list of dataframes, then need to convert these into one dataframe
  suppressWarnings(
    if (class(df)=="list") {
    df <- do.call("rbind", df)
    }
  )
  
  EnsurePackage("igraph")

      # Now create the dfSemanticNetwork3,
      # a dataframe of relations between hashtags and terms
      # (i.e. hashtag i and term j both occurred in same tweet
      # (weight = n occurrences))

      print("Generating Twitter semantic network...")  ### DEBUG
      flush.console()

      # convert the hashtags to lowercase here (before using tm_map later)
      # but first deal with character encoding:
      macMatch <- grep("darwin",R.Version()$os)
      if (length(macMatch)!=0) {
        # df$hashtags_used <- iconv(df$hashtags_used,to="utf-8-mac")
        df$hashtags_used <- lapply(df$hashtags_used, function(x) TrimOddCharMac(x))
      }
      if (length(macMatch)==0) {
        df$hashtags_used <- lapply(df$hashtags_used, function(x) TrimOddChar(x))
      }
      # ... and then convert to lowercase:
      df$hashtags_used <- lapply(df$hashtags_used,tolower)

      # do the same for the comment text, but first deal with character encoding!
      # we need to change value of `to` argument in `iconv` depending on OS, or else errors can occur
      macMatch <- grep("darwin",R.Version()$os)
      if (length(macMatch)!=0) {
        df$text <- iconv(df$text,to="utf-8-mac")
      }
      if (length(macMatch)==0) {
        df$text <- iconv(df$text,to="utf-8")
      }
      # ... and then convert to lowercase:
      df$text <- tolower(df$text)

      hashtagsUsedTemp <- c() # temp var to store output

      # The 'hashtags_used' column in the 'df' dataframe
      # is slightly problematic (i.e. not straightforward)
      # because each cell in this column contains a
      # LIST, itself containing 1 or more char vectors
      # (which are unique hashtags found in the tweet text; empty if no hashtags used).
      # So, need to extract each list item out,
      # and put it into its own row in a new dataframe:

      for (i in 1:nrow(df)) {
        if (length(df$hashtags_used[[i]]) > 0) { # skip any rows where NO HASHTAGS were used
          for (j in 1:length(df$hashtags_used[[i]])) {
            #commonTermsTemp <- c(commonTermsTemp, df$from_user[i])
            hashtagsUsedTemp <- c(hashtagsUsedTemp,df$hashtags_used[[i]][j])
          }
        }
      }   # NOTE: try and vectorise this in future work to improve speed.

      hashtagsUsedTemp <- unique(hashtagsUsedTemp)

### delete hashtags that contain 'horizontal ellipses'

      # delEllipses <- grep("\u2026",hashtagsUsedTemp)
      # cat(paste("\nNumber of hashtags with ellipses: ",length(delEllipses),"\n"))
      # cat(paste("\nThe offending hashtags:\n",hashtagsUsedTemp[delEllipses],"\n"))
      # cat("Original:\n")
      # cat(hashtagsUsedTemp)
      # hashtagsUsedTemp <- hashtagsUsedTemp[-delEllipses]
      # cat("Fixed:\n")
      # cat(hashtagsUsedTemp)

########

      hashtagsUsedTempFrequency <- c()
      # potentially do not want EVERY hashtag - just the top N% (most common):
      for (i in 1: length(hashtagsUsedTemp)) {
          hashtagsUsedTempFrequency[i] <- length(grep(hashtagsUsedTemp[i],df$text))
      }
      mTemp <- cbind(hashtagsUsedTemp, hashtagsUsedTempFrequency)
      mTemp2 <- as.matrix(as.numeric(mTemp[,2]))
      names(mTemp2) <- mTemp[,1]
      vTemp <- sort(mTemp2, decreasing=TRUE)
      hashtagsUsedTemp <- names(head(vTemp, (length(vTemp) / 100) * hashtagFreq))
      ################################ ^^^^ this defaults to top 50% hashtags

      # we need to remove all punctuation EXCEPT HASHES (!)
      # (e.g. both #auspol and auspol will appear in data)
      df$text <- gsub("[^[:alnum:][:space:]#]", "", df$text)

      ## Find the most frequent terms across the tweet text corpus
      commonTermsTemp <- df$text

      corpusTweetText <- Corpus(VectorSource(commonTermsTemp))

      ## add usernames to stopwords

      mach_usernames <- sapply(df$screen_name, function(x) TrimOddChar(x))
      mach_usernames <- unique(mach_usernames)
      if (length(macMatch)!=0) {
        mach_usernames <- iconv(mach_usernames,to="utf-8-mac")
      }
      if (length(macMatch)==0) {
        mach_usernames <- iconv(mach_usernames,to="utf-8")
      }

      # we remove the usernames from the text (so they don't appear in data/network)
      my_stopwords <- mach_usernames
      corpusTweetText <- tm_map(corpusTweetText, removeWords, my_stopwords)

      # convert to all lowercase (WE WILL DO THIS AGAIN BELOW, SO REMOVE THIS DUPLICATE)
      # corpusTweetText <- tm_map(corpusTweetText, content_transformer(tolower))

      # remove English stop words (IF THE USER HAS SPECIFIED!)
      if (stopwordsEnglish) {
        corpusTweetText <- tm_map(corpusTweetText, removeWords, stopwords("english"))
      }

      # eliminate extra whitespace
      corpusTweetText <- tm_map(corpusTweetText, stripWhitespace)

      # create document term matrix applying some transformations
      # note: applying too many transformations here (duplicating...) - need to fix
      tdm = TermDocumentMatrix(corpusTweetText,
         control = list(removeNumbers = TRUE, tolower = TRUE))

      # create a vector of the common terms, finding the top N% terms
      # N will need to be adjusted according to network / user requirements.

      mTemp <- as.matrix(tdm)
      vTemp <- sort(rowSums(mTemp), decreasing=TRUE)
      commonTerms <- names(head(vTemp, (length(vTemp) / 100) * termFreq))
      ################################ ^^^^ the default finds top 5% terms

      toDel <- grep("http",commonTerms) # !! still picking up junk terms (FIX)
      if(length(toDel) > 0) {
        commonTerms <- commonTerms[-toDel] # delete these junk terms
      }

      # create the "semantic hashtag-term network" dataframe
      # (i.e. pairs of hashtags / terms)

      termAssociatedWithHashtag <- c() # temp var to store output
      hashtagAssociatedWithTerm <- c() # temp var to store output

      for (i in 1:nrow(df)) {
        if (length(df$hashtags_used[[i]]) > 0) { # skip any rows where NO HASHTAGS were used
          for (j in 1:length(df$hashtags_used[[i]])) {
            for (k in 1:length(commonTerms)) {
              match <- grep(commonTerms[k],df$text[i])
              if (length(match) > 0) {

                termAssociatedWithHashtag <- c(termAssociatedWithHashtag,commonTerms[k])
                hashtagAssociatedWithTerm <- c(hashtagAssociatedWithTerm,df$hashtags_used[[i]][j])

              }
            }
          }
        }
      }   # THIS IS A *HORRIBLE* LOOPED APPROACH. NEED TO VECTORISE!!!

      # this needs to be changed to termAssociatedWithHashtag and hashtagAssociatedWithTerm
      dfSemanticNetwork3 <- data.frame(hashtagAssociatedWithTerm, termAssociatedWithHashtag)

      # OK, now extract only the UNIQUE pairs (i.e. rows)
      # But, also create a WEIGHT value for usages of the same hashtag.
          # NOTE: This edge weights approach might be problematic for TEMPORAL networks, because each edge (with weight > 1) may represent usage of hashtags at DIFFERENT TIMES.
          # NOTE: A possible workaround could be to include an edge attribute that is a set of timestamp elements, showing the date/time of each instance of usage of a hashtag.
          # NOTE: For example, in a temporal visualisation, the first timestamp might 'pop in' the edge to the graph, which then might start to 'fade out' over time (or just 'pop out' of graph after N seconds) if there are no more timestamps indicating activity (i.e. a user using a hashtag).
          # NOTE: So, a 'timestamps' edge attribute could factor into a kind of 'entropy' based approach to evolving the network visually over time.

      # unique pairs:
      unique_dfSemanticNetwork3 <- unique(dfSemanticNetwork3) # hmm, need this still?

      # number of times hashtag was used per user/hashtag pair (i.e. edge weight):
      for (i in 1:nrow(unique_dfSemanticNetwork3)) {
        unique_dfSemanticNetwork3$numHashtagTermOccurrences[i] <- sum(
          hashtagAssociatedWithTerm==unique_dfSemanticNetwork3[i,1] &
          termAssociatedWithHashtag==unique_dfSemanticNetwork3[i,2])
      }

      # make a dataframe of the relations between actors
      relations <- data.frame(from=as.character(unique_dfSemanticNetwork3[,1]),to=as.character(unique_dfSemanticNetwork3[,2]),weight=unique_dfSemanticNetwork3$numHashtagTermOccurrences)
      relations$from <- as.factor(relations$from)
      relations$to <- as.factor(relations$to)

      actorsFixed <- rbind(as.character(unique_dfSemanticNetwork3[,1]),as.character(unique_dfSemanticNetwork3[,2]))
      actorsFixed <- as.factor(actorsFixed)
      actorsFixed <- unique(actorsFixed)

      ##### STEP FOUR #####

      # convert into a graph
      suppressWarnings(
        g <- graph.data.frame(relations, directed=FALSE, vertices=actorsFixed)
      )
      # we need to simplify the graph because multiple use of same term
      # in one tweet will cause self-loops, etc
      # g <- simplify(g)

      # Make the node labels play nice with Gephi
      V(g)$label <- V(g)$name

      # remove the search term / hashtags, if user specified it:
      if (removeTermsOrHashtags[1]!="foobar") {
          toDel <- match(tolower(removeTermsOrHashtags),V(g)$name) # we force to lowercase because all terms/hashtags are already converted to lowercase
          toDel <- toDel[!is.na(toDel)] # in case of user error (i.e. trying to delete terms/hashtags that don't exist in the data)
          g <- delete.vertices(g, toDel)
      }

      if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
        # Output the final network to a graphml file, to import directly into Gephi
        currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
        currTime <- gsub(":","_",currTime)
        write.graph(g,paste0(currTime,"_TwitterSemanticNetwork.graphml"),format="graphml")
        cat("Twitter semantic network was written to current working directory, with filename:\n")
        cat(paste0(currTime,"_TwitterSemanticNetwork.graphml"))
      }

      cat("\nDone.") ### DEBUG
      flush.console()

    return(g)

}
