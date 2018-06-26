#' @export
CreateBimodalNetwork.instagram <-
function(x,writeToFile, ...)
{

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  dataCombined <- x # match the variable names (this must be used to avoid warnings in package compilation)

  # if `dataCombined` is a list of dataframes, then need to convert these into one dataframe
  suppressWarnings(
    if (class(dataCombined)=="list") {
    dataCombined <- do.call("rbind", dataCombined)
    }
  )
  
  cat("\nCreating Instagram bimodal network...\n")

  actors_users <- data.table(id=dataCombined$from_userID,
     node_type="User",
     name=dataCombined$from_username,
     full_name=dataCombined$from_username,
     profile_picture=dataCombined$from_profile_picture,
     post_created_time_UNIX_epoch = NA,
     post_created_time = NA,
     post_type = NA,
     post_longitude = NA,
     post_latitude = NA,
     post_location_name = NA,
     post_location_id = NA,
     post_link = NA,
     post_image_URL = NA,
     post_caption = NA,
     post_username = NA,
     post_user_ID = NA,
     post_user_fullname = NA
  )

  toDel <- which(duplicated(actors_users$id))
  actors_users <- actors_users[-toDel,]

  actors_posts <- data.table(id=dataCombined$to_post_id,
     node_type="Post",
     name=dataCombined$to_post_id, # we will just make the name = post id
     full_name=NA,
     profile_picture=NA,
     post_created_time_UNIX_epoch = NA,
     # post_created_time = as.POSIXct(dataCombined$post_created_time, origin="1970-01-01"),
     # post_created_time = NA,
     post_created_time = dataCombined$post_created_time,
     post_type = dataCombined$post_type,
     post_longitude = dataCombined$post_longitude,
     post_latitude = dataCombined$post_latitude,
     post_location_name = dataCombined$post_location_name,
     post_location_id = dataCombined$post_location_id,
     post_link = dataCombined$post_link,
     post_image_URL = dataCombined$post_image_URL,
     post_caption = dataCombined$post_caption,
     post_username = dataCombined$post_username,
     post_user_ID = dataCombined$post_user_ID,
     post_user_fullname = dataCombined$post_user_fullname
  )

  # actors_posts <- unique(actors_posts)

  actors <- rbind(actors_users,actors_posts)
  actors <- unique(actors)
  actors <- data.frame(actors)

  relations <- data.frame(from=dataCombined$from_userID,
                          to=dataCombined$to_post_id,
                          edge_type=dataCombined$edge_type)

  # relations <- unique(relations)

  g <- graph.data.frame(relations, directed=TRUE, vertices=actors)

  # Make the node labels play nice with Gephi
  V(g)$label <- V(g)$name

  if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
    # Output the final network to a graphml file, to import directly into Gephi
    currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
    currTime <- gsub(":","_",currTime)
    write.graph(g,paste0(currTime,"_Instagram_Bimodal_Network.graphml"),format="graphml")
    cat("Instagram bimodal network was written to current working directory, with filename:\n")
    cat(paste0(currTime,"_Instagram_Bimodal_Network.graphml"))
  }

  V(g)$post_created_time <- as.POSIXct(V(g)$post_created_time, origin="1970-01-01")

  cat("\nDone!\n") ### DEBUG
  flush.console()

  return(g)

}
