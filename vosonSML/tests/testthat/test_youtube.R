source("cred.R")
require(magrittr)

## "4_hHKlEZ9Go" is a closed comment video

test_that("Youtube Empty Comment Error",{
    expect_error(Authenticate("youtube", yt) %>% Collect(videoIDs = c("4_hHKlEZ9Go")), "No comment can be collected from the given videoIDs.")
### however, multiple videoIDs with only one with empty comment should not throw an error.
    borat <- Authenticate("youtube", yt) %>% Collect(videoIDs = c("4_hHKlEZ9Go", "YzdYF0r3gB4"))
    expect_true("dataSource" %in% class(borat))
})
