context("ImportData")

test_csv <- "test.csv"

test_that("ImportData input", {
  expect_error(ImportData(), "Please provide file path of data to import.")
  expect_error(ImportData("xxx.rds"), "Please provide the social media type of data to import.")
  
  supported_types <- c("csv", "rds")
  not_supp_msg <- paste0("File format not supported. please choose from: ",
                         paste0(supported_types, collapse = ", "), ".")
  expect_error(ImportData("xxx", "twitter"), not_supp_msg)
  expect_error(ImportData("xxx.graphml", "twitter"), not_supp_msg)
  
  expect_error(suppressWarnings(ImportData("xxx.rds", "twitter")))
  
  if (file.exists(test_csv)) {
    expect_error(ImportData(test_csv, "twitter", "rds"))
    expect_error(capture.output(ImportData(test_csv, "xxx", "csv")),
                 "Unknown social media type provided as datasource.")
  }
})

test_that("ImportData output", {
  skip_if_not(file.exists(test_csv), message = "csv file exists")
  capture.output({ data <- ImportData(test_csv, "twitter", "csv") })
  expect_s3_class(data, "data.frame")
  expect_s3_class(data, "datasource")
  expect_s3_class(data, "twitter")
})
