#' Create 'ego' networks from social media data
#' 
#' This function creates 'ego' networks from social media data as a data.frame
#' of class \code{dataSource.*.ego}. The networks are igraph objects.
#' 
#' 
#' @param x a data.frame of class \code{dataSource.*.ego}
#' @param writeToFile logical. If \code{TRUE} then the network is saved to file
#' in current working directory (GRAPHML format), with filename denoting the
#' current date/time and the type of network.
#' @return An igraph graph object, with directed and weighted edges.
#' @note Similarly named function \code{CreateEgoNetwork} is a function of both
#' collecting data from social media and creating ego network. The current
#' function only creates ego network out of collected social media data.
#' @author Timothy Graham <timothy.graham@@anu.edu.au>, Robert
#' Ackland<robert.ackland@@anu.edu.au> & Chung-hong Chan
#' <chainsawtiney@@gmail.com>
#' @examples
#' 
#' \dontrun{
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#' instagram_oauth_token <- AuthenticateWithInstagramAPI(appID=myAppID,
#' appSecret=myAppSecret, useCachedToken=TRUE)
#' myUsernames <- c("senjohnmccain","obama")
#' instagramEgodata <- CollectEgoInstgram(username=myUsernames,
#' verbose=TRUE,degreeEgoNet=1,
#' waitForRateLimit=TRUE,getFollows=FALSE)
#' CreateEgoNetoworkFromData(instagramEgodata)
#' ## the same as Create(instagramEgodata) or Create(instagramEgodata, "ego")
#' }
#' 
CreateEgoNetworkFromData <-
function(x, writeToFile) {
    if (missing(writeToFile)) {
        writeToFile <- FALSE # default = not write to file
    }
    UseMethod("CreateEgoNetworkFromData", x)
}

CreateEgoNetworkFromData.default <-
function(x, writeToFile) {
    if (missing(writeToFile)) {
        writeToFile <- FALSE # default = not write to file
    }
    cat("Error. Cannot operate on this data.\nUse the `CollectEgo` family of functions in the vosonSMLs package (or Collect(ego = TRUE) )to collect ego data for generating ego networks.\n")
}

#' @export
CreateEgoNetworkFromData.instagram <- function(x, writeToFile) {
    egoName = profile_picture = full_name = id = . = NULL # please the rcheck god
    if (missing(writeToFile)) {
        writeToFile <- FALSE # default = not write to file
    }
    dataCombinedFinal <- x # match the variable names (this must be used to avoid warnings in package compilation)

    ## make sure user is not trying to create a ego network,
    ## i.e. using data that is not ego

    if (!inherits(dataCombinedFinal, "ego")) {
        stop("Ego networks require ego data. Use the 'ego = TRUE' argument when collecting data, e.g. when calling the Collect function.\n")
    }
    
    actors <- dataCombinedFinal[,.(username,profile_picture,full_name,id)] # we want the 'actors' from username column
    actors2 <- dataCombinedFinal[,.(egoName,profile_picture,full_name,id)] # we want the 'actors' from egoName column
    setnames(actors2,"egoName","username") # make sure the column names match before rbind
    actors <- rbind(actors,actors2)
    toDel <- which(duplicated(actors$username))
    actors <- actors[-toDel,]
    actors <- as.data.frame(actors)
    ## actors <- unique(actors)
    username <- unique(dataCombinedFinal$egoName) ## regenerate the original username from CollectEgoInstagram
    relations <- data.frame(from=dataCombinedFinal$username[(length(username)+1):nrow(dataCombinedFinal)],to=dataCombinedFinal$egoName[(length(username)+1):nrow(dataCombinedFinal)],ringset=dataCombinedFinal$ringset[(length(username)+1):nrow(dataCombinedFinal)])
    ## relations <- data.frame(from=dataCombinedFinal$username,to=dataCombinedFinal$egoName)
    ## g <- graph.data.frame(relations, directed=TRUE, vertices=unique(c(unique(dataCombinedFinal$egoName),unique(dataCombinedFinal$username))))
    g <- graph.data.frame(relations, directed=TRUE, vertices=actors)
    ## Make the node labels play nice with Gephi
    V(g)$label <- V(g)$name
    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
        currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
        currTime <- gsub(":","_",currTime)
        write.graph(g, paste0("Instagram_Ego_Network_",currTime,".graphml"), format="graphml")
        cat("Instagram ego network was written to current working directory (in GRAPHML format), with filename:\n")
        cat(paste0("Instagram_Ego_Network_",currTime,".graphml"))
    }

  # if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
  #   currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
  #   currTime <- gsub(":","_",currTime)
  #   write.csv(actors,paste0("Instagram_Ego_Network_Data_",currTime,".csv"))
  #   cat("Instagram ego network data was written to current working directory, with filename:\n")
  #   cat(paste0("Instagram_Ego_Network_Data_",currTime,".csv"))
  # }
    return(g)
}
