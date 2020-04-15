context("Create")

test_that("Create", {
  data <- NULL
  suppress_cat(expect_error(Create(data, "activity"), "Unknown datasource passed to create activity network."))
  suppress_cat(expect_error(Create(data, "xxx"), "Unknown network type passed to create."))
})
