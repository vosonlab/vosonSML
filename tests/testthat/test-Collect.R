context("Collect")

test_that("Collect", {
  suppress_cat(expect_error(Collect("xxx"), "Unknown social media type passed to collect."))
})

test_that("Collect.twitter input", {
  auth <- list()
  class(auth) <- append(class(auth), "twitter")
  expect_error(suppress_cat(Collect(auth)),
                "OAuth token missing. Please use the Authenticate function to create and supply a token.")
})

test_that("Collect.twitter output", {
  skip_on_cran()
  skip_if_not(file.exists("~/.twitter_auth"), message = "twitter auth file exists")

  auth <- readRDS("~/.twitter_auth")
  suppress_cat(
    data <- auth %>% Collect(searchTerm = "#auspol", searchType = "recent", numTweets = 10, verbose = FALSE,
                             includeRetweets = FALSE, retryOnRateLimit = TRUE, writeToFile = FALSE)
  )

  expect_s3_class(data, "datasource")
  expect_s3_class(data, "twitter")
  expect_equal(is.data.frame(data), TRUE)
  expect_named(data)
  expect_gt(nrow(data), 1)
})

test_that("GetYoutubeVideoIDs output", {
  expect_output(GetYoutubeVideoIDs(), "Please provide a vector and or file of youtube video urls.")
  suppress_cat(expect_match(GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=IVjZMIWhz3Y")), "IVjZMIWhz3Y"))
  suppress_cat(expect_match(GetYoutubeVideoIDs(c("https://youtu.be/IVjZMIWhz3Y")), "IVjZMIWhz3Y"))
  expect_output(GetYoutubeVideoIDs(c("https://youtu.be/IVjZMIWhz3Y")), "Extracted 1 video ids.")
  expect_output(GetYoutubeVideoIDs(c("https://youtu.be/IVjZMIWhz3Y",
                                     "https://www.youtube.com/watch?v=xxx")), "Extracted 2 video ids.")
})

test_that("Collect.youtube input", {
  auth <- list()
  class(auth) <- append(class(auth), "youtube")
  auth$auth <- NULL
  invalid_msg <- "Please provide a valid youtube api key."
  expect_error(suppress_cat(Collect(auth)), invalid_msg)
  auth$auth <- ""
  expect_error(suppress_cat(Collect(auth)), invalid_msg)

  auth$auth <- "xxx"
  expect_error(suppress_cat(Collect(auth)), "Please provide a vector of one or more youtube video ids.")
})

test_that("Collect.youtube output", {
  skip_on_cran()
  skip_if_not(file.exists("~/.youtube_auth"), message = "youtube auth file exists")

  auth <- readRDS("~/.youtube_auth")
  suppress_cat(data <- auth %>% Collect(videoIDs = c("IVjZMIWhz3Y"),
                                    maxComments = 50, writeToFile = FALSE, verbose = FALSE))

  expect_s3_class(data, "datasource")
  expect_s3_class(data, "youtube")
  expect_equal(is.data.frame(data), TRUE)
  expect_named(data)
  expect_true(all(c("Comment", "AuthorDisplayName", "AuthorProfileImageUrl", "AuthorChannelUrl", "AuthorChannelID",
                "ReplyCount", "LikeCount", "PublishedAt", "UpdatedAt", "CommentID", "ParentID", "VideoID") %in%
                names(data)))
  expect_gt(nrow(data), 1)
})

test_that("Collect.reddit output", {
  skip_on_cran()

  auth <- Authenticate("reddit")
  threads <- c("https://www.reddit.com/r/redditdev/comments/ezz3td/upcoming_api_change_post_apisubmit/")
  suppress_cat(
    data <- auth %>% Collect(threadUrls = threads, writeToFile = FALSE)
  )

  expect_s3_class(data, "datasource")
  expect_s3_class(data, "reddit")
  expect_equal(is.data.frame(data), TRUE)
  expect_named(data)
  expect_true(all(c("id", "structure", "post_date", "post_date_unix", "comm_id", "comm_date",
                     "comm_date_unix", "num_comments", "subreddit", "upvote_prop", "post_score",
                     "author", "user", "comment_score", "controversiality", "comment", "title",
                     "post_text", "link", "domain", "url", "rm", "thread_id") %in% names(data)))
  expect_gt(nrow(data), 1)
})
