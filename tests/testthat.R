library(testthat)
library(vosonSML)

suppress_cat <- function(input = NULL) {
  invisible(capture.output(input, type = c("output")))
}

test_check("vosonSML")
