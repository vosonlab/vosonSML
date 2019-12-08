CleanRedditText <- function(comments) {

  # json encoding issues should be tackled upstream
  
  # xml 1.0
  # allowed #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
  # [\x00-\x1F] ^\xE000-\xFFFD^\x10000-\x10FFFF
  # [^\x09^\x0A^\x0D^\x20-\xD7FF^\xE000-\xFFFD]
  # [\u0000-\u0008,\u000B,\u000C,\u000E-\u001F]
  
  # decode html encoding as not required
  # df$vosonTxt_comment <- textutils::HTMLdecode(df$vosonTxt_comment)
  
  # take care of a few known encoding issues
  comments <- gsub("([\u0019])", "'", comments, perl = TRUE, useBytes = TRUE)
  comments <- gsub("([\u0023])", "#", comments, perl = TRUE, useBytes = TRUE)
  comments <- gsub("([&#x200B;])", " ", comments, perl = TRUE, useBytes = TRUE)
  
  # replace chars outside of allowed xml 1.0 spec
  comments <- gsub("([\u0001-\u0008\u000B\u000C\u000E-\u001F])", "", comments, perl = TRUE, useBytes = TRUE)
}

FullCleanText <- function(sentences) {
  # remove any characters that are not in punctuation, alphanumeric classes or spaces
  sentences <- gsub("[^[:punct:]^[:alnum:]^\\s^\\n]", "", sentences, perl = TRUE, useBytes = TRUE)
}

# modified RedditExtractoR reddit_content function
# original author @ivan-rivera
mod_reddit_content = function(URL, wait_time = 2, data = NULL) {
  
  if(is.null(URL) | length(URL)==0 | !is.character(URL)) { stop("invalid URL parameter") }
  
  # setting up a function for extraction of comment specific information:
  GetAttribute  = function(node, feature){
    filter = is.null(node$data[["author"]])
    
    Attribute   = node$data[[feature]]
    replies     = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    
    return(list(Attribute, 
                lapply(reply.nodes, function(x) { GetAttribute(x, feature) })))  
  }
  
  get.structure = function(node, depth = 0) {
    if (is.null(node)) { return(list()) }
    
    filter = is.null(node$data$author)
    replies = node$data$replies
    reply.nodes = if (is.list(replies)) replies$data$children else NULL
    
    return(list(paste0(filter, " ", depth), 
                lapply(1:length(reply.nodes), function(x) get.structure(reply.nodes[[x]], paste0(depth, "_", x)))))
  }
  
  # setting up the data frame
  data_extract = data.frame(id               = numeric(),
                            structure        = character(),
                            post_date        = character(),
                            post_date_unix   = numeric(),
                            comm_id          = character(),
                            comm_date        = character(),
                            comm_date_unix   = numeric(),
                            num_comments     = numeric(),
                            subreddit        = character(),
                            upvote_prop      = numeric(),
                            post_score       = numeric(),
                            author           = character(),
                            user             = character(),
                            comment_score    = numeric(),
                            controversiality = numeric(),
                            comment          = character(),
                            title            = character(),
                            post_text        = character(),
                            link             = character(),
                            domain           = character(),
                            URL              = character())
  
  rem_indexes <- function(index_list, data_list) { 
    if (length(index_list) == 0) {
      return(data_list)
    } 
    data_list[-index_list] 
  }
  
  for(i in seq(URL)){
    
    if(!grepl("^https?://(.*)",URL[i])) URL[i] = paste0("https://www.",gsub("^.*(reddit\\..*$)","\\1",URL[i]))
    if(!grepl("\\?ref=search_posts$",URL[i])) URL[i] = paste0(gsub("/$","",URL[i]),"/?ref=search_posts")
    
    X = paste0(gsub("\\?ref=search_posts$","",URL[i]),".json?limit=500&enc=utf-8") # 500 is the maximum &enc=utf-8
    
    if (is.null(data)) {
    raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)),error = function(e) NULL)
    
    # try again if it fails
    if(is.null(raw_data)){
      Sys.sleep(min(1, wait_time))
      raw_data = tryCatch(RJSONIO::fromJSON(readLines(X, warn = FALSE)), error = function(e) NULL)
    }
    }
    
    if (!is.null(data)) {
      raw_data <- data
    } 
    
    if(is.null(raw_data) == FALSE){
      
      # extracting comment specific information:
      meta.node = raw_data[[1]]$data$children[[1]]$data
      main.node = raw_data[[2]]$data$children
      
      if(min(length(meta.node),length(main.node)) > 0){
        
        structure <- unlist(lapply(1:length(main.node), function(x) get.structure(main.node[[x]], x)))
        
        no_auth_inds <- which(grepl("TRUE", structure))
        
        TEMP = data.frame(id               = NA,
                          structure        = gsub("FALSE " , "", structure[!grepl("TRUE", structure)]),
                          post_date        = as.character(
                            as_datetime(as.numeric(meta.node$created_utc), tz = "UTC")
                          ),
                          post_date_unix   = as.numeric(meta.node$created_utc),
                          comm_id          = rem_indexes(no_auth_inds, 
                                                         unlist(lapply(main.node, function(x) { GetAttribute(x, "id") }))),
                          comm_date        = as.character(
                            as_datetime(as.numeric(unlist(lapply(main.node, function(x) { GetAttribute(x, "created_utc") }))), tz = "UTC")
                          ),
                          comm_date_unix   = as.numeric(unlist(lapply(main.node, function(x) { GetAttribute(x, "created_utc") }))),                                            
                          num_comments     = meta.node$num_comments,
                          subreddit        = ifelse(is.null(meta.node$subreddit), "UNKNOWN", meta.node$subreddit),
                          upvote_prop      = meta.node$upvote_ratio,
                          post_score       = meta.node$score,
                          author           = meta.node$author,
                          user             = unlist(lapply(main.node, function(x) { GetAttribute(x, "author") })),
                          comment_score    = unlist(lapply(main.node, function(x) { GetAttribute(x, "score") })),
                          controversiality = unlist(lapply(main.node, function(x) { GetAttribute(x, "controversiality") })),
                          comment          = unlist(lapply(main.node, function(x) { GetAttribute(x, "body") })),
                          title            = meta.node$title,
                          post_text        = meta.node$selftext,
                          link             = meta.node$url,
                          domain           = meta.node$domain,
                          URL              = URL[i],
                          stringsAsFactors = FALSE)
        
        TEMP$id = 1:nrow(TEMP)
        
        if (dim(TEMP)[1]>0 & dim(TEMP)[2]>0) data_extract = rbind(TEMP, data_extract)
        else print(paste("missed", i, ":", URL[i]))
      }
    }
    
    Sys.sleep(min(2, wait_time))
  }

  data_extract
}
