AddText <- function(df, net) {
  if ("twitter" %in% class(net) && "activity" %in% class(net)) {
    net$nodes <- dplyr::left_join(net$nodes, dplyr::select(df, .data$status_id, .data$text), by = "status_id")
    net$nodes <- dplyr::left_join(net$nodes, 
                                  dplyr::select(df, .data$quoted_status_id, .data$quoted_text) %>%
                                  dplyr::rename(status_id =.data$quoted_status_id, qtext = .data$quoted_text) %>%
                                  dplyr::distinct(), by = "status_id")
    net$nodes <- dplyr::left_join(net$nodes, 
                                  dplyr::select(df, .data$retweet_status_id, .data$retweet_text) %>%
                                  dplyr::rename(status_id =.data$retweet_status_id, rtext = .data$retweet_text) %>%
                                  dplyr::distinct(), by = "status_id")
    net$nodes <- dplyr::mutate(net$nodes, text = ifelse(!is.na(.data$text), .data$text,
                                                    ifelse(!is.na(.data$qtext), .data$qtext, .data$rtext))) %>%
      dplyr::select(-c(.data$qtext, .data$rtext)) %>% dplyr::rename(vosonTxt_tweet = .data$text)
    
    
    net$graph <- igraph::graph_from_data_frame(d = net$edges, directed = TRUE, vertices = net$nodes)
    
    igraph::V(net$graph)$label <- ifelse(!is.na(igraph::V(net$graph)$screen_name), 
                                 paste0(igraph::V(net$graph)$name, " (", igraph::V(net$graph)$screen_name, ")"), 
                                 igraph::V(net$graph)$name)
    
    net$graph <- igraph::set_graph_attr(net$graph, "type", "twitter")
    
    return(net)
  }
  
  if ("youtube" %in% class(net) && "activity" %in% class(net)) {
    net$nodes <- dplyr::left_join(net$nodes, dplyr::select(df, .data$CommentId, .data$Comment), by = "CommentId") %>%
      dplyr::rename(vosonTxt_comment = .data$Comment)
    
    net$graph <- igraph::graph_from_data_frame(d = net$edges, directed = TRUE, vertices = net$nodes)
    
    igraph::V(net$graph)$label <- ifelse(!is.na(igraph::V(net$graph)$User), 
                                         paste0(igraph::V(net$graph)$name, " (", igraph::V(net$graph)$User, ")"), 
                                         igraph::V(net$graph)$name)
    
    net$graph <- igraph::set_graph_attr(net$graph, "type", "youtube")
    
    return(net)
  }  
  
  if ("reddit" %in% class(net) && "activity" %in% class(net)) {
    
    net$graph <- igraph::set_graph_attr(net$graph, "type", "reddit")
    
    return(net)
  }  
}
