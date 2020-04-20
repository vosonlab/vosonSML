context("Create")

test_that("Create", {
  data <- NULL
  suppress_cat(expect_error(Create(data, "activity"), "Datasource is not a dataframe."))
  data <- tibble::tibble()
  suppress_cat(expect_error(Create(data, "activity"), "Empty datasource passed to create."))
  data <- tibble::tibble(a = c(1, 2, 3))
  suppress_cat(expect_error(Create(data, "activity"), "Unknown datasource passed to create activity network."))
  class(data) <- append(class(data), c("datasource", "twitter"))
  suppress_cat(expect_error(Create(data, "xxx"), "Unknown network type passed to create."))
})
