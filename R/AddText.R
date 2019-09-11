#' @export
AddText <- function(net, ...) {
  # searches the class list of net for matching method
  UseMethod("AddText", net)
}

#' @noRd
#' @export
AddText.default <- function(net, ...) {
  stop("Unknown network type passed to addtext.", call. = FALSE) 
}

#' @noRd
#' @method AddText activity
#' @export
AddText.activity <- function(net, ...) {
  UseMethod("AddText.activity", net)
}

#' @noRd
#' @export
AddText.activity.default <- function(net, ...) {
  stop("Unknown social media type passed to addtext.", call. = FALSE)
}

#' @export
AddText.activity.twitter <- function(net, data, ...) {
  net$nodes <- dplyr::left_join(net$nodes, dplyr::select(data, .data$status_id, .data$text), by = "status_id")
  net$nodes <- dplyr::left_join(net$nodes, 
                                dplyr::select(data, .data$quoted_status_id, .data$quoted_text) %>%
                                  dplyr::rename(status_id =.data$quoted_status_id, qtext = .data$quoted_text) %>%
                                  dplyr::distinct(), by = "status_id")
  net$nodes <- dplyr::left_join(net$nodes, 
                                dplyr::select(data, .data$retweet_status_id, .data$retweet_text) %>%
                                  dplyr::rename(status_id =.data$retweet_status_id, rtext = .data$retweet_text) %>%
                                  dplyr::distinct(), by = "status_id")
  net$nodes <- dplyr::mutate(net$nodes, text = ifelse(!is.na(.data$text), .data$text,
                                                      ifelse(!is.na(.data$qtext), .data$qtext, .data$rtext))) %>%
    dplyr::select(-c(.data$qtext, .data$rtext)) %>% dplyr::rename(vosonTxt_tweet = .data$text)
  
  net
}

#' @export
AddText.activity.youtube <- function(net, data, ...) {
  net$nodes <- dplyr::left_join(net$nodes, dplyr::select(data, .data$CommentId, .data$Comment), by = "CommentId") %>%
    dplyr::rename(vosonTxt_comment = .data$Comment)
  
  net
}

#' @export
AddText.activity.reddit <- function(net, data, cleanText = TRUE, ...) {
  net$nodes <- dplyr::left_join(net$nodes, 
                                dplyr::mutate(data, id = paste0(.data$thread_id, ".", .data$structure)) %>%
                                  dplyr::select(.data$id, .data$subreddit, .data$comment), 
                                by = c("id", "subreddit"))
  
  threads <- dplyr::select(data, .data$subreddit, .data$thread_id, .data$title, .data$post_text) %>%
    dplyr::distinct() %>% dplyr::mutate(id = paste0(.data$thread_id, ".0"), thread_id = NULL)
  
  net$nodes <- dplyr::left_join(net$nodes, threads, by = c("id", "subreddit")) %>%
    dplyr::mutate(comment = ifelse(.data$node_type == "thread", .data$post_text, .data$comment)) %>%
    dplyr::select(-c(.data$post_text)) %>% dplyr::rename(vosonTxt_comment = .data$comment)
  
  if (cleanText) {
    net$nodes$vosonTxt_comment <- CleanRedditText(net$nodes$vosonTxt_comment)
    net$nodes$title <- CleanRedditText(net$nodes$title)
  }  
  
  net
}

#' @noRd
#' @method AddText actor
#' @export
AddText.actor <- function(net, ...) {

  UseMethod("AddText.actor", net)
}

#' @noRd
#' @export
AddText.actor.default <- function(net, ...) {
  stop("Unknown social media type passed to addtext.", call. = FALSE)
}

#' @export
AddText.actor.reddit <- function(net, data, cleanText = TRUE, ...) {

  # rename the edge attribute containing the thread comment
  net$edges <- dplyr::left_join(net$edges,
                                dplyr::select(data, .data$subreddit, .data$thread_id, .data$id, .data$comment),
                                by = c("subreddit", "thread_id", "comment_id" = "id")) %>%
               dplyr::rename(vosonTxt_comment = .data$comment)

  authors <- dplyr::select(data, .data$subreddit, .data$thread_id, .data$title, .data$post_text) %>% 
    dplyr::distinct() %>% dplyr::mutate(comment_id = 0)
  
  net$edges <- dplyr::left_join(net$edges, authors, by = c("subreddit", "thread_id", "comment_id")) %>%
    dplyr::mutate(vosonTxt_comment = ifelse(.data$comment_id == 0, .data$post_text, .data$vosonTxt_comment), 
                  post_text = NULL)
  
  net$edges$vosonTxt_comment <- ifelse(trimws(net$edges$vosonTxt_comment) == "", NA, net$edges$vosonTxt_comment)
  
  if (cleanText) {
    net$edges$vosonTxt_comment <- CleanRedditText(net$edges$vosonTxt_comment)
    net$edges$title <- CleanRedditText(net$edges$title)
  }
  
  net
}
