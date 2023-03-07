#' @title Collect comments data for YouTube videos
#'
#' @description This function collects public comments data for one or more YouTube videos using the YouTube Data API v3
#'   and structures the data into a dataframe with the class names \code{"datasource"} and \code{"youtube"}.
#'
#'   YouTube has a quota unit system as a rate limit with most developers having either 10,000 or 1,000,000 units per
#'   day. Many read operations cost a base of 1 unit such as retrieving individual comments, plus 1 or 2 units for text
#'   snippets. Retrieving threads or top-level comments with text costs 3 units per request (maximum 100 comments per
#'   request). Using this function a video with 250 top-level comments and 10 of those having reply comments of up to
#'   100 each, should cost (9 + 20) 29 quota units and return between 260 and 1260 total comments. There is currently a
#'   limit of 100 reply comments collected per top-level comment.
#'
#'   More information about the YouTube Data API v3 can be found here:
#'   \url{https://developers.google.com/youtube/v3/getting-started}
#'
#' @note Due to specifications of the YouTube Data API it is currently not efficient to specify the exact number of
#'   comments to return from the API using \code{maxComments} parameter. The \code{maxComments} parameter is applied to
#'   top-level comments only and not the replies to these comments. As such the number of comments collected is usually
#'   greater than expected. For example, if \code{maxComments} is set to 10 and one of the videos 10 top-level comments
#'   has 5 reply comments then the total number of comments collected will be 15 for that video. Comments data for
#'   multiple YouTube videos can be requested in a single operation, \code{maxComments} is applied to each individual
#'   video and not the combined total of comments.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"youtube"}.
#' @param videoIDs Character vector. Specifies YouTube video URLs or IDs. For example, if the video URL is
#'   \code{https://www.youtube.com/watch?v=xxxxxxxxxxx} then use URL or ID \code{videoIDs = c("xxxxxxxxxxx")}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{FALSE}.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param maxComments Numeric integer. Specifies how many top-level comments to collect from each video. This value does
#'   not consider replies to top-level comments. The total number of comments returned for a video will usually be
#'   greater than \code{maxComments} depending on the number of reply comments present.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A tibble object with class names \code{"datasource"} and \code{"youtube"}.
#'
#' @examples
#' \dontrun{
#' # list of YouTube video urls or ids to collect
#' video_ids <- c("https://www.youtube.com/watch?v=xxxxxxxx",
#'                "https://youtu.be/xxxxxxxx",
#'                "xxxxxxx")
#'
#' # collect approximately 200 threads/comments for each YouTube video
#' youtubeData <- youtubeAuth |>
#'   Collect(videoIDs = video_ids, writeToFile = TRUE, verbose = FALSE, maxComments = 200)
#' }
#'
#' @export
Collect.youtube <-
  function(credential,
           videoIDs = c(),
           verbose = FALSE,
           writeToFile = FALSE,
           maxComments = 1e10,
           ...) {

    dbg <- lgl_debug(list(...)$debug)

    msg("Collecting comment threads for YouTube videos...\n")

    apiKey <- credential$auth
    if (is.null(apiKey) || nchar(apiKey) < 1) {
      stop("Please provide a valid YouTube api key.", call. = FALSE)
    }

    if (!is.vector(videoIDs) || length(videoIDs) < 1) {
      stop("Please provide a vector of YouTube video urls or ids.",
           call. = FALSE)
    }

    video_ids <- get_yt_video_ids(videoIDs)

    if (length(video_ids) < 1) {
      stop("Failed to extract any YouTube video ids.", call. = FALSE)
    }

    api_cost <- total_api_cost <- 0

    # Start data collection

    # Create a dataframe to iteratively store comments from all the videos that the user wants to scrape
    # (i.e. specified in videoIDs) uses 'dummy' data in first row (which is removed later)
    dataCombined <- tibble::tibble(
      Comment = "foo",
      AuthorDisplayName = "bar",
      AuthorProfileImageUrl = NA,
      AuthorChannelUrl = NA,
      AuthorChannelID = NA,
      ReplyCount = "99999999",
      LikeCount = "99999999",
      PublishedAt = "timestamp",
      UpdatedAt = "timestamp",
      CommentID = "99999999123456789",
      ParentID = "foobar",
      VideoID = "foobarfoobar"
    )

    # Iterate through the videos in video_ids, adding to dataCombined.
    for (k in 1:length(video_ids)) {
      msg(paste0("Video ", k, " of ", length(video_ids), "\n", sep = "")) # DEBUG
      msg("---------------------------------------------------------------\n")

      ############################## Collect comment threads #############################

      rObj <- yt_scraper(video_ids, apiKey, k, verbose = dbg)

      rObj$scrape_all(maxComments)

      api_cost <- rObj$api_cost

      # skip if no threads
      if (length(rObj$data) == 0)  {
        msg("\n")
        next
      }

      ## Make a dataframe out of the results

      msg(paste0("** Creating dataframe from threads of ", video_ids[k], ".\n"))

      tempData <- lapply(rObj$data, function(x) {
        tibble::tibble(
          Comment = x$snippet$topLevelComment$snippet$textDisplay,
          AuthorDisplayName = x$snippet$topLevelComment$snippet$authorDisplayName,
          AuthorProfileImageUrl = x$snippet$topLevelComment$snippet$authorProfileImageUrl,
          AuthorChannelUrl = x$snippet$topLevelComment$snippet$authorChannelUrl,
          AuthorChannelID = ifelse(
            is.null(
              x$snippet$topLevelComment$snippet$authorChannelId$value
            ),
            "[NoChannelId]",
            x$snippet$topLevelComment$snippet$authorChannelId$value
          ),
          ReplyCount = as.character(x$snippet$totalReplyCount),
          LikeCount = as.character(x$snippet$topLevelComment$snippet$likeCount),
          PublishedAt = x$snippet$topLevelComment$snippet$publishedAt,
          UpdatedAt = x$snippet$topLevelComment$snippet$updatedAt,
          CommentID = x$snippet$topLevelComment$id,
          ParentID = NA,
          VideoID = video_ids[k]
          # actual reference to API data is:
          # x$snippet$topLevelComment$snippet$video_ids[k]
        )
      })

      # core_df <- do.call("rbind", tempData)
      core_df <- dplyr::bind_rows(tempData)

      ############################## Collect comment replies #############################

      commentIDs <- core_df$CommentID

      # only attempt to collect replies for comments we know have replies
      commentIDs_with_replies <-
        core_df[which(as.numeric(core_df$ReplyCount) > 0), ] # column 6
      commentIDs_with_replies <- commentIDs_with_replies$CommentID

      if (length(commentIDs_with_replies) > 0) {
        msg(
          paste0(
            "** Collecting replies for ",
            length(commentIDs_with_replies),
            " threads with replies. Please be patient.\n"
          )
        ) # commentIDs

        base_url <- "https://www.googleapis.com/youtube/v3/comments"

        # 'dummy' first row of dataframe, for DEBUG purposes (fix later..)
        dataRepliesAll <- tibble::tibble(
          Comment = "foo",
          AuthorDisplayName = "bar",
          AuthorProfileImageUrl = NA,
          AuthorChannelUrl = NA,
          AuthorChannelID = NA,
          ReplyCount = "99999999",
          LikeCount = "99999999",
          PublishedAt = "timestamp",
          UpdatedAt = "timestamp",
          CommentID = "99999999123456789",
          ParentID = "foobar",
          VideoID = video_ids[k]
          # API DOESN'T SEEM TO RETURN HERE, no matter anyway
        )

        # for each thread
        # ** doesnt have paging - possibly wont get all comments if > 100 per thread
        # need to do same as with threads
        total_replies <- 0

        for (i in 1:length(commentIDs_with_replies)) {
          # commentIDs
          api_opts <- list(
            part = "snippet",
            textFormat = "plainText",
            parentId = commentIDs_with_replies[i],
            # commentIDs
            key = apiKey
          )

          # api cost 1 + 1 = 2 per request
          # init_results <- httr::content(httr::GET(base_url, query = api_opts))
          # TODO: should die when there is error

          req <- httr::GET(base_url, query = api_opts)
          init_results <- httr::content(req)

          err <- FALSE
          if (req$status_code != 200) {
            err <- TRUE
            msg(
              paste0(
                "\nComment error: ",
                init_results$error$code,
                "\nDetail: ",
                init_results$error$message,
                "\n"
              )
            )
            msg(paste0("parentId: ", commentIDs_with_replies[i], "\n\n"))
          } else {
            api_cost <- api_cost + 2
          }

          num_items <- length(init_results$items)

          if (verbose) {
            if (i == 1) {
              msg("Comment replies ")
            }

            msg(paste(num_items, ""))

          } else {
            msg(".")

          }

          total_replies <- total_replies + num_items

          tempDataReplies <-
            lapply(init_results$items, function(x) {
              tibble::tibble(
                Comment = x$snippet$textDisplay,
                AuthorDisplayName = x$snippet$authorDisplayName,
                AuthorProfileImageUrl = x$snippet$authorProfileImageUrl,
                AuthorChannelUrl = x$snippet$authorChannelUrl,
                AuthorChannelID = ifelse(
                  is.null(x$snippet$authorChannelId$value),
                  "[NoChannelId]",
                  x$snippet$authorChannelId$value
                ),
                ReplyCount = "0",
                # there is no ReplyCount returned for replies (API specs)
                LikeCount = as.character(x$snippet$likeCount),
                PublishedAt = x$snippet$publishedAt,
                UpdatedAt = x$snippet$updatedAt,
                CommentID = x$id,
                ParentID = x$snippet$parentId,
                VideoID = video_ids[k]
              )
            })

          tempDataRepliesBinded <- do.call("rbind", tempDataReplies)

          dataRepliesAll <-
            rbind(dataRepliesAll, tempDataRepliesBinded)

          if (err || rObj$api_error) {
            break
          }
        }

        total_api_cost <- total_api_cost + api_cost

        msg(paste0("\n** Collected replies: ", total_replies, "\n"))
        msg(paste0(
          "** Total video comments: ",
          length(commentIDs) + total_replies,
          "\n"
        ))
        if (verbose) {
          msg(paste0("(Video API unit cost: ", api_cost, ")\n"))
        }
        msg("---------------------------------------------------------------\n")

        ############################## Combine comment threads and replies #############################

        # get rid of "dummy" first row
        dataRepliesAll <- dataRepliesAll[-1, ]

        # combine the comments and replies dataframes
        # dataCombinedTemp <- rbind(core_df, dataRepliesAll)
        dataCombinedTemp <- dplyr::bind_rows(core_df, dataRepliesAll)

        dataCombined <- dplyr::bind_rows(dataCombined, dataCombinedTemp)

        # no threads with reply comments for video
      } else {
        total_api_cost <- total_api_cost + api_cost
        dataCombined <- dplyr::bind_rows(dataCombined, core_df)
        msg("\n")
      }

    } # end for (k in 1:length(video_ids))

    msg(paste0(
      "** Total comments collected for all videos ",
      nrow(dataCombined) - 1,
      ".\n",
      sep = ""
    ))

    # Remove 'dummy' first row
    dataCombined <- dataCombined[-1, ]

    ## Throw Error when no comment can be collected
    if (nrow(dataCombined) == 0) {
      stop(
        paste0(
          "No comments could be collected from the given video Ids: ",
          paste0(video_ids, collapse = ", "),
          "\n"
        ),
        call. = FALSE
      )
    } else {
      msg(paste0("(Estimated API unit cost: ", total_api_cost, ")\n"))
    }

    #############################################################################
    # return dataframe to environment

    dataCombined <-
      tibble::as_tibble(dataCombined) # convert type to tibble for package consistency
    
    class(dataCombined) <- append(c("datasource", "youtube"), class(dataCombined))
    
    meta_log <- c(collect_log, paste0(format(Sys.time(), "%a %b %d %X %Y")))
    
    if (writeToFile) write_output_file(dataCombined, "rds", "YoutubeData", verbose = verbose, log = meta_log)

    msg("Done.\n")

    dataCombined

    #############################################################################
  }

## Set up a class and methods / functions for scraping
yt_scraper <- setRefClass(
  "yt_scraper",
  fields = list(
    base_url = "character",
    api_opts = "list",
    nextPageToken = "character",
    page_count = "numeric",
    data = "list",
    unique_count = "numeric",
    done = "logical",
    core_df = "data.frame",
    verbose = "logical",
    api_cost = "numeric",
    api_error = "logical"
  ),

  methods = list(
    # collect api results for page
    scrape = function() {
      # set default api request options
      opts <- api_opts

      if (is.null(nextPageToken) ||
          length(trimws(nextPageToken)) == 0L ||
          trimws(nextPageToken) == "") {
        if (page_count >= 1) {
          if (verbose) {
            vsml_msg(paste0(
              "-- No nextPageToken. Returning. page_count is: ",
              page_count,
              "\n"
            ))
          }

          # return no threads collected to signal done
          return(0)
        } else {
          if (verbose) {
            vsml_msg("-- First thread page. No pageToken.\n")
          }
        }
      } else {
        opts$pageToken <- trimws(nextPageToken)

        if (verbose) {
          vsml_msg(paste0("-- Value of pageToken: ", opts$pageToken, "\n"))
        }
      }

      page_count <<- page_count + 1

      req <- httr::GET(base_url, query = opts)
      res <- httr::content(req)

      if (req$status_code != 200) {
        api_error <<- TRUE
        nextPageToken <<- ""
        if (verbose) {
          vsml_msg(paste0(
            "\nThread error: ",
            res$error$code,
            "\nDetail: ",
            res$error$message,
            "\n"
          ))
          vsml_msg(paste0("videoId: ", opts$videoId, "\n\n"))
        }
        return(0)
      } else {
        api_cost <<- api_cost + 3
      }

      if (is.null(res$nextPageToken)) {
        nextPageToken <<- ""
      } else {
        nextPageToken <<- res$nextPageToken
      }

      # add threads to data list
      data <<- c(data, res$items)

      # return count of threads collected from page
      return(length(res$items))
    },

    # collect all video threads until done or max comments reached
    scrape_all = function(maxComments) {
      if (verbose) {
        vsml_msg(paste0("** video Id: ", api_opts$videoId, "\n", sep = ""))
        vsml_msg(
          paste0(
            "   [results per page: ",
            api_opts$maxResults,
            " | max comments per video: ",
            maxComments,
            "]\n",
            sep = ""
          )
        )
      }

      thread_count <- 0

      while (TRUE) {
        # collect threads for current page
        thread_count <- scrape()

        if (verbose) {
          vsml_msg(paste0("-- Collected threads from page: ", thread_count, "\n", sep = ""))
        }

        if (thread_count == 0 |
            length(data) > maxComments | api_error) {
          done <<- TRUE
          nextPageToken <<- ""

          if (length(data) > maxComments) {
            if (verbose) {
              vsml_msg(
                paste0(
                  "-- API returned more than max comments. Results truncated to first ",
                  maxComments,
                  " threads.\n",
                  sep = ""
                )
              )
            }

            data <<- data[1:maxComments]
          }

          if (verbose) {
            vsml_msg(paste0("-- Done collecting threads.\n", sep = ""))
          }

          break
        }
      }

      if (verbose) {
        vsml_msg(paste0("** Results page count: ", page_count, "\n"))
      }
      if (verbose) {
        vsml_msg(paste0("** Collected threads: ", length(data), "\n"))
        vsml_msg(paste0("(Threads API unit cost: ", api_cost, ")\n"))
      }
    },

    # quota cost approx 1 per commentThreads + 2 for snippet part, 3 per page of results
    initialize = function(video_ids, apiKey, k, verbose = FALSE) {
      base_url <<- "https://www.googleapis.com/youtube/v3/commentThreads/"
      api_opts <<- list(
        part = "snippet",
        maxResults = 100,
        textFormat = "plainText",
        videoId = video_ids[k],
        key = apiKey,
        fields = "items,nextPageToken",
        orderBy = "published"
      )
      page_count <<- 0
      nextPageToken <<- ""
      data <<- list()
      unique_count <<- 0
      done <<- FALSE
      core_df <<- tibble::tibble()
      verbose <<- verbose
      api_cost <<- 0
      api_error <<- FALSE
    },

    reset = function() {
      data <<- list()
      page_count <<- 0
      nextPageToken <<- ""
      unique_count <<- 0
      done <<- FALSE
      core_df <<- tibble::tibble()
      api_cost <<- 0
      api_error <<- FALSE
    },

    cache_core_data = function() {
      if (nrow(core_df) < unique_count) {
        sub_data <- lapply(data, function(x) {
          tibble::tibble(
            Comment = x$snippet$topLevelComment$snippet$textDisplay,
            AuthorDisplayName = x$snippet$topLevelComment$snippet$authorDisplayName,
            AuthorProfileImageUrl = x$snippet$topLevelComment$snippet$authorProfileImageUrl,
            AuthorChannelUrl = x$snippet$topLevelComment$snippet$authorChannelUrl,
            AuthorChannelID = ifelse(
              is.null(
                x$snippet$topLevelComment$snippet$authorChannelId$value
              ),
              "[NoChannelId]",
              x$snippet$topLevelComment$snippet$authorChannelId$value
            ),
            ReplyCount = as.character(x$snippet$totalReplyCount),
            LikeCount = as.character(x$snippet$topLevelComment$snippet$likeCount),
            PublishedAt = x$snippet$topLevelComment$snippet$publishedAt,
            UpdatedAt = x$snippet$topLevelComment$snippet$updatedAt,
            CommentID = x$snippet$topLevelComment$id
          )
        })
        core_df <<- dplyr::bind_rows(sub_data) # do.call("rbind", sub_data)
      } else {
        if (verbose) {
          vsml_msg("core_df is already up to date.\n")
        }
      }
    }
  )
)
