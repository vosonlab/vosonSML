#' Note: this function is DEPRECATED and will be removed in a future release.
#' Please use the \code{Collect} function
#'
#' Collect data from Instagram for generating different types of networks
#'
#' This function collects data from Instagram using either hashtag (e.g.
#' #obama) or search coordinates (latitude and longitude), and structures the
#' data into a data frame of class \code{dataSource.Instagram}, ready for
#' creating networks for further analysis. This function draws heavily on the
#' `searchInstagram` function (from the 'instaR' package) and includes all the
#' arguments for collecting Instagram data using that function, as well as
#' additional arguments listed below.
#'
#' \code{CollectDataInstagram} collects public pictures and videos from
#' Instagram, and also collects the maximum amount of comments and 'likes' for
#' each post. It draws on and extends the `searchInstagram` function from the
#' 'instaR' package.
#'
#' As the 'instaR' documentation describes, it is only possible to apply one
#' filter at a time: either search by hashtag OR search by coordinates. The
#' 'mindate' and 'maxdata' search parameters only work when searching by
#' location, not when searching by tag.
#'
#' After the data are collected, the function finds and maps the relationships
#' between users and posts, and structures these relationships into a format
#' suitable for creating bimodal networks using \code{CreateBimodalNetwork}.
#'
#' @param tag character string. Hashtag used to filter media. It is only
#' possible for a single hashtag.
#' @param n numeric. Maximum number of media to return.
#' @param lat numeric. Latitude of the center search coordinate.
#' @param lng numeric. Longitude of the center search coordinate.
#' @param distance numeric. Default is 1km (distance=1000), max distance is
#' 5km.
#' @param folder character string. If different than \code{NULL}, will download
#' all pictures to this folder.
#' @param mindate character string. Minimum date for search period.
#' @param maxdate character string. Maximum date for search period.
#' @param verbose logical. If \code{TRUE} then this function will output
#' runtime information to the console as it computes. Useful diagnostic tool
#' for long computations. Default is \code{FALSE}.
#' @param sleep numeric, Number of seconds between API calls (default is 0).
#' @param writeToFile logical. If \code{TRUE} then the data is saved to file in
#' current working directory (CSV format), with filename denoting the current
#' time/date.
#' @param waitForRateLimit logical. If \code{TRUE} then it will try to observe
#' the API rate limit by ensuring that no more than 5000 API calls are made per
#' hour (the current rate limit). If more than 5000 calls are made within a 60
#' minute window, then all operates will suspend for 60 minutes, and resume
#' afterwards. Note: API calls are only tracked within the scope of this
#' function.
#' @param credential Optional, a \code{credential} object created by \code{Authenticate}.
#' @return A data frame object of class \code{dataSource.Instagram} that can be
#' used with \code{CreateBimodalNetwork}.
#' @note The current implementation only supports creating bimodal networks.
#' Other network types will be added in the near future.
#'
#' - bimodal networks; \code{CreateBimodalNetwork}
#'
#' A bimodal (or two-mode) network means that there are two types of vertices
#' present in the network (i.e. Instagram users and Instagram posts), with
#' edges representing user i 'commenting' or 'liking' post j.
#' @author Timothy Graham <timothy.graham@@anu.edu.au> & Robert Ackland
#' <robert.ackland@@anu.edu.au>
#' @seealso \code{AuthenticateWithInstagramAPI} must be run first or no data
#' can be collected through the API.
#' @keywords instagram data mining SNA
#' @examples
#'
#' \dontrun{
#' ## Use your own values for myAppID and myAppSecret
#' myAppID <- "123456789098765"
#' myAppSecret <- "abc123abc123abc123abc123abc123ab"
#'
#' # Authenticate with the Instagram API using `AuthenticateWithInstagramAPI`
#' instagram_oauth_token <- AuthenticateWithInstagramAPI(appID=app_id, appSecret=app_secret,
#'   useCachedToken=TRUE)
#'
#' # EXAMPLE 1
#'
#' # Run the `CollectDataInstagram` function and store the results in variable `myInstagramData`
#' # (searching by hashtag)
#' myInstagramData <- CollectDataInstagram(tag="obama", distance=5000, n=100, folder=NULL,
#' verbose=TRUE, waitForRateLimit=TRUE)
#'
#' # Create a 'bimodal' network using \code{CreateBimodalNetwork}
#' g_bimodal_instagram_obama <- CreateBimodalNetwork(myInstagramData,writeToFile=F)
#'
#' # View descriptive information about the bimodal network
#' g_bimodal_instagram_obama
#'
#' # EXAMPLE 2
#'
#' # Run the `CollectDataInstagram` function and store the results in variable `myInstagramData`
#' # (searching by coordinates in Brisbane (Australia) with a radius of 5km)
#' myInstagramData <- CollectDataInstagram(lat=-27.4701, lng=153.0220, distance=5000, n=100,
#'   folder=NULL, verbose=TRUE, waitForRateLimit=TRUE)
#'
#' # Create a 'bimodal' network using \code{CreateBimodalNetwork}
#' g_bimodal_instagram_brisbane <- CreateBimodalNetwork(myInstagramData,writeToFile=F)
#'
#' # View descriptive information about the bimodal network
#' g_bimodal_instagram_brisbane
#' }
#' @export
CollectDataInstagram <-
function(tag, n, lat, lng, distance, folder, mindate, maxdate, verbose, sleep, writeToFile, waitForRateLimit, credential = NULL) {

  from_userID = from_username = from_full_name = from_profile_picture = edge_type = to_post_id = post_created_time = post_type = post_longitude = post_latitude = post_location_name = post_location_id = post_link = post_image_URL = post_caption = post_username = post_user_ID = post_user_fullname = comment_created_time = comment_text = commentsData = likesData = NULL

  if(!(exists("instagram_oauth_token")) & is.null(credential)) {
      instagram_oauth_token <- NULL
  }
  ## not using side effect
  if (!is.null(credential)) {
      instagram_oauth_token <- credential$auth
  }
  # handle the arguments

  if (missing(verbose)) {
    verbose <- FALSE # default = not verbose
  }

  if (missing(writeToFile)) {
    writeToFile <- FALSE # default = not write to file
  }

  if (verbose=="TRUE" | verbose=="true" | verbose=="T" | verbose==TRUE) {
    verbose <- TRUE
  }
  else {verbose <- FALSE}

  if (missing(n)) {
    n <- 100 # default to 100 max posts
  }

  if (missing(tag)) {
    tag <- NULL
  }

  if (missing(lat)) {
    lat <- NULL
  }

  if (missing(lng)) {
    lng <- NULL
  }

  if (missing(distance)) {
    distance=1000
  }

  if (distance < 1 | distance > 5000) {
    cat("\nNote: maximum distance for searching posts is 5000 (metres). Value changed to 5000.")
    distance <- 5000
  }

  if (missing(folder)) {
    folder <- NULL
  }

  if (missing(mindate)) {
    mindate <- NULL
  }

  if (missing(maxdate)) {
    maxdate <- NULL
  }

  if (missing(sleep)) {
    sleep <- 0 # time to sleep in seconds between API calls
  }

  if (missing(waitForRateLimit)) {
    waitForRateLimit <- FALSE # the default is NOT to wait for the rate limit. This will probably result in an error if the rate limit is maxed out.
  }

  # Start data collection

  rateLimitHourTimer <- proc.time() # start the 60 minute timer (for minding the rate limit if waitForRateLimit==TRUE)
  totalCallsToAPI <- 0

  testData <- searchInstagram(tag=tag, n=n, lat=lat, lng=lng, distance=distance, folder=folder, mindate=mindate, maxdate=maxdate, verbose=verbose, sleep=sleep, token=instagram_oauth_token)
  if (n <= 50) {totalCallsToAPI <- totalCallsToAPI + 1} # increment by 1
  if (n > 50) {totalCallsToAPI <- totalCallsToAPI + round(n / 100)} # increment 1 call for every 100 posts requested (calls are made in batches of 100)

  # create a new column (list) to store the comments data
  testData$commentsData <- vector("list",nrow(testData))

  # create a new column (list) to store the likes data
  testData$likesData <- vector("list",nrow(testData))

  # (2) get all the comments for all posts in the dataset
  # and store the results back into the dataframe
  # NOTE: implicitly convert the data to data.table objects
  for (i in 1:nrow(testData)) {
    if (testData$comments_count[i] != 0) { # check to make sure there are comments to collect

      if (verbose & (totalCallsToAPI %% 100 == 0)) { # inform the user every 100 calls
        cat(paste0("\nNumber of API calls used so far is: ",totalCallsToAPI,"\n"))
      }

      # stop it at 4990, just shy of 5000 (margin of error)
      if (waitForRateLimit & totalCallsToAPI==4990 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
        cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
        Sys.sleep(3600)
#cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
# Sys.sleep(120) # DEBUG
        cat("Waking up now! Back to work...\n")
        totalCallsToAPI <- 0 # reset number of calls
        rateLimitHourTimer <- proc.time() # reset hourly timer
      }

      possibleError <- tryCatch({ # we also need to catch errors, e.g. if there is one comment but it gets deleted in between calls
        testData$commentsData[i][[1]] <- getComments(testData$id[i], instagram_oauth_token)
        totalCallsToAPI <- totalCallsToAPI + 1
        testData$commentsData[i][[1]] <- data.table(testData$commentsData[i][[1]]) # convert to data.table
        },
        error=function(e) e
      )
      if(inherits(possibleError, "error")) {
        cat(paste0("\n I caught an error collecting comments data... (row ",i,")\n"))
        next
        }
    }
  }

  # (3) get all the likes for all posts in the dataset
  # and store the results back into the dataframe
  # NOTE: implicitly convert the data to data.table objects
  for (i in 1:nrow(testData)) {
    if (testData$likes_count[i] != 0) { # check to make sure there are likes to collect

      if (verbose & (totalCallsToAPI %% 100 == 0)) { # notify every 100 calls
        cat(paste0("\nNumber of API calls is: ",totalCallsToAPI,"\n"))
      }

      # stop it at 4990, just shy of 5000 (margin of error)
      if (waitForRateLimit & totalCallsToAPI==4990 & (proc.time() - rateLimitHourTimer)[3] < 3600) { # if we have hit the limit within one hour
        cat("\nThe rate limit has reached maximum capacity! Sleeping now for 60 minutes...\n")
        Sys.sleep(3600)
# cat("\nThe rate limit has reached 60! Sleeping now for 2 minutes...\n") # DEBUG
# Sys.sleep(120) # DEBUG
        cat("Waking up now! Back to work...\n")
        totalCallsToAPI <- 0 # reset number of calls
        rateLimitHourTimer <- proc.time() # reset hourly timer
      }

      possibleError <- tryCatch({ # we also need to catch errors, e.g. if there is one like but it gets deleted in between calls
        testData$likesData[i][[1]] <- getLikes(testData$id[i], instagram_oauth_token)
        totalCallsToAPI <- totalCallsToAPI + 1
          testData$likesData[i][[1]] <- data.table(testData$likesData[i][[1]]) # convert to data.table
          },
        error=function(e) e
      )
      if(inherits(possibleError, "error")) {
        cat(paste0("\n I caught an error collecting likes data... (row ",i,")\n"))
        next
      }
    }
  }

  # now we convert the dataframe to a data.table to make processing FASTER
  # (this doesn't convert the dataframes in the list vectors, but we have already done that)
  testDataTable <- data.table(testData)

  ## STEP 2 - sort through the data and make a data.table of relations

  # for each row in testDataTable we extract data from `testDataTable$commentsData` and `testDataTable$likesData`
  # and put this into a data.table `dataCombined` that we can then construct a bimodal graph out of.

  # for speed we will pre-allocate `dataCombined` to a very large size (more rows than needed)
  # and after everything is finished we will delete the unused rows

  dataCombined <- data.table(
    from_userID = rep("NA_f00",20000000),
    from_username = rep("NA_f00",20000000),
    from_full_name = rep("NA_f00",20000000),
    from_profile_picture = rep("NA_f00",20000000),
    edge_type = rep("NA_f00",20000000),
    to_post_id = rep("NA_f00",20000000),

    post_created_time = rep(Inf,20000000),
    post_type = rep("NA_f00",20000000),
    post_longitude = rep(Inf,20000000),
    post_latitude = rep(Inf,20000000),
    post_location_name = rep("NA_f00",20000000),
    post_location_id = rep(Inf,20000000),
    post_link = rep("NA_f00",20000000),
    post_image_URL = rep("NA_f00",20000000),
    post_caption = rep("NA_f00",20000000),
    post_username = rep("NA_f00",20000000),
    post_user_ID = rep("NA_f00",20000000),
    post_user_fullname = rep("NA_f00",20000000),

    comment_created_time = rep(Inf,20000000),
    comment_text = rep("NA_f00",20000000)
  )

  setkey(dataCombined,from_userID) # set the key value of the data table

  nextEmptyRow <- 1 # so we can update rows in `dataCombined` in a relatively efficient way

  # We firstly do the comments data
  for (i in 1:nrow(testDataTable)) {

    if (is.null(testDataTable[i,commentsData][[1]])) {next} # we check if there are comments, if not skip to next row

    for (j in 1:nrow(testDataTable$commentsData[i][[1]])){ # for each row of the comments data for post i

      # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

      dataCombined[nextEmptyRow, from_userID := testDataTable$commentsData[i][[1]]$from_id[j]]
      dataCombined[nextEmptyRow, from_username := testDataTable$commentsData[i][[1]]$from_username[j]]
      dataCombined[nextEmptyRow, from_full_name := testDataTable$commentsData[i][[1]]$from_full_name[j]]
      dataCombined[nextEmptyRow, from_profile_picture := testDataTable$commentsData[i][[1]]$from_profile_picture[j]]
      dataCombined[nextEmptyRow, edge_type := "comment"]
      dataCombined[nextEmptyRow, to_post_id := testDataTable$id[i]] # here we want the post id that the person commented on

      dataCombined[nextEmptyRow, post_created_time := testDataTable$created_time[i]]
      dataCombined[nextEmptyRow, post_type := testDataTable$type[i]]
      dataCombined[nextEmptyRow, post_longitude := testDataTable$longitude[i]]
      dataCombined[nextEmptyRow, post_latitude := testDataTable$latitude[i]]
      dataCombined[nextEmptyRow, post_location_name := testDataTable$location_name[i]]
      dataCombined[nextEmptyRow, post_location_id := testDataTable$location_id[i]]
      dataCombined[nextEmptyRow, post_link := testDataTable$link[i]]
      dataCombined[nextEmptyRow, post_image_URL := testDataTable$image_url[i]]
      dataCombined[nextEmptyRow, post_caption := testDataTable$caption[i]]
      dataCombined[nextEmptyRow, post_username := testDataTable$username[i]]
      dataCombined[nextEmptyRow, post_user_ID := testDataTable$user_id[i]]
      dataCombined[nextEmptyRow, post_user_fullname := testDataTable$user_fullname[i]]

      dataCombined[nextEmptyRow, comment_created_time := testDataTable$commentsData[i][[1]]$created_time[j]]
      dataCombined[nextEmptyRow, comment_text := testDataTable$commentsData[i][[1]]$text[j]]

      nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

    }

  }

  # Next we do the likes data
  for (i in 1:nrow(testDataTable)) {

    if (is.null(testDataTable[i,likesData][[1]])) {next} # we check if there are likes, if not skip to next row

    for (j in 1:nrow(testDataTable$likesData[i][[1]])){ # for each row of the likes data for post i

      # nextEmptyRow <- dataCombined[  , .I[from_userID=="NA_f00"] ][1] # we get index of the next 'empty' row to put data into # NOT NEEDED NOW, BUT USEFUL FOR LATER

      dataCombined[nextEmptyRow, from_userID := testDataTable$likesData[i][[1]]$id[j]]
      dataCombined[nextEmptyRow, from_username := testDataTable$likesData[i][[1]]$username[j]]
      dataCombined[nextEmptyRow, from_full_name := testDataTable$likesData[i][[1]]$full_name[j]]
      dataCombined[nextEmptyRow, from_profile_picture := testDataTable$likesData[i][[1]]$profile_picture[j]]
      dataCombined[nextEmptyRow, edge_type := "like"]
      dataCombined[nextEmptyRow, to_post_id := testDataTable$id[i]] # here we want the post id that the person commented on

      dataCombined[nextEmptyRow, post_created_time := testDataTable$created_time[i]]
      dataCombined[nextEmptyRow, post_type := testDataTable$type[i]]
      dataCombined[nextEmptyRow, post_longitude := testDataTable$longitude[i]]
      dataCombined[nextEmptyRow, post_latitude := testDataTable$latitude[i]]
      dataCombined[nextEmptyRow, post_location_name := testDataTable$location_name[i]]
      dataCombined[nextEmptyRow, post_location_id := testDataTable$location_id[i]]
      dataCombined[nextEmptyRow, post_link := testDataTable$link[i]]
      dataCombined[nextEmptyRow, post_image_URL := testDataTable$image_url[i]]
      dataCombined[nextEmptyRow, post_caption := testDataTable$caption[i]]
      dataCombined[nextEmptyRow, post_username := testDataTable$username[i]]
      dataCombined[nextEmptyRow, post_user_ID := testDataTable$user_id[i]]
      dataCombined[nextEmptyRow, post_user_fullname := testDataTable$user_fullname[i]]

      dataCombined[nextEmptyRow, comment_created_time := NA] # not applicable because this is 'likes' data
      dataCombined[nextEmptyRow, comment_text := NA] # not applicable because this is 'likes' data

      nextEmptyRow <- nextEmptyRow + 1 # increment the row to update in `dataCombined`

    }

  }

  # we now delete all the rows at the end of `dataCombined` that are unused
  dataCombined <- dataCombined[from_profile_picture != "NA_f00"] # we just keep the rows that are unchanged from the original dummy data values

  # finish up and return...

    class(dataCombined) <- append(class(dataCombined),c("dataSource","instagram"))

    if (writeToFile=="TRUE" | writeToFile=="true" | writeToFile=="T" | writeToFile==TRUE) {
      currTime <- format(Sys.time(), "%b_%d_%X_%Y_%Z")
      currTime <- gsub(":","_",currTime)
      write.csv(dataCombined,paste0("Instagram_Data_",currTime,".csv"))
      cat("Instagram data was written to current working directory, with filename:\n")
      cat(paste0("Instagram_Data_",currTime,".csv"))
    }

    cat("\n")

    return(dataCombined)

}
