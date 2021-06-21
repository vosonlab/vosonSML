context("Authenticate")

test_that("Authenticate", {
  expect_error(Authenticate(FALSE), "Authentication social media type should be a character string.")
  expect_error(Authenticate("xxx"), "Unknown social media type passed to authenticate.")
})

test_that("Authenticate.twitter", {
  auth <- Authenticate("twitter", "appName", "apiKey", "apiSecret", "accessToken", "accessTokenSecret")

  expect_type(auth, "list")
  expect_s3_class(auth, "credential")
  expect_s3_class(auth, "twitter")

  expect_type(auth$auth, "environment")
  expect_s3_class(auth$auth, "Token1.0")
  expect_s3_class(auth$auth, "R6")
  expect_equal(auth$auth$app$key, "apiKey")
  expect_equal(auth$auth$app$secret, "apiSecret")
  expect_equal(auth$auth$credentials$oauth_token, "accessToken")
  expect_equal(auth$auth$credentials$oauth_token_secret, "accessTokenSecret")
})

test_that("Authenticate.youtube", {
  expect_error(Authenticate("youtube", FALSE), "Requires YouTube API key as string.")

  auth <- Authenticate("youtube", "apiKey")

  expect_type(auth, "list")
  expect_s3_class(auth, "credential")
  expect_s3_class(auth, "youtube")
})

test_that("Authenticate.reddit", {
  auth <- Authenticate("reddit")

  expect_type(auth, "list")
  expect_s3_class(auth, "credential")
  expect_s3_class(auth, "reddit")
})
