# vosonSML <img src="man/figures/logo.png" width="140px" align="right"/>
![Github Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github&colorB=8065ac)
![Last Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
![Downloads](https://cranlogs.r-pkg.org/badges/vosonSML)

`vosonSML` is an R package that provides a suite of tools for collecting and constructing networks from social media data. It provides easy-to-use functions for collecting data across popular platforms and generating different types of networks for analysis.

`vosonSML` is the `SocialMediaLab` package, renamed. We decided that `SocialMediaLab` was a bit too generic and also we wanted to indicate the connection to the [Virtual Observatory for the Study of Online Networks Lab](http://vosonlab.net), where this package was conceived and created.

`vosonSML` was created by [Timothy Graham](http://uq.academia.edu/TimGraham) and [Robert Ackland](https://researchers.anu.edu.au/researchers/ackland-rj), with major contributions by [Chung-hong Chan](https://github.com/chainsawriot) and Bryan Gertzel.

### Supported Social Media

`vosonSML` currently features the collection of data and generation of networks from `twitter`, `youtube` and `reddit`. 

Unfortunately we are no longer able to maintain `facebook` and `instagram` collection, however these features will still be available in [releases](https://github.com/vosonlab/vosonSML/releases) prior to version `0.25.0`.

## Installation

Install the current version from Github:
```R
# requires the 'devtools' package (or alternatively 'remotes')
library(devtools)

# optionally add the parameter 'dependencies = TRUE' to install package dependencies
devtools::install_github("vosonlab/vosonSML")
```

Install vosonSML from CRAN:
```R
install.packages("vosonSML", dependencies = TRUE)
```

## Getting started

The following usage examples will provide a great introduction to using `vosonSML`. There are also several "how to" guides, including an "Absolute Beginners Guide to vosonSML" tutorial aimed at people with little or no programming experience on the [vosonSML page of the VOSON website](http://vosonlab.net/SocialMediaLab).

### Usage

The process of authentication, data collection and creating social network in vosonSML is expressed with the three verb functions: *Authenticate*, *Collect* and *Create*. The following are some examples:

```R
library(magrittr)
library(vosonSML)

# Twitter Example

# Authenticate with twitter, Collect 100 tweets for the '#auspol' hashtag and Create an actor and 
# semantic network
myKeys <- list(appName = "vosonSML", apiKey = "xxxxxxxxxxxx", apiSecret = "xxxxxxxxxxxx", 
               accessToken = "xxxxxxxxxxxx", accessTokenSecret = "xxxxxxxxxxxx")
  
twitterAuth <- Authenticate("twitter", appName = myKeys$appName, apiKey = myKeys$apiKey, 
                            apiSecret = myKeys$apiSecret, accessToken = myKeys$accessToken,
                            accessTokenSecret = myKeys$accessTokenSecret, useCachedToken = TRUE)
                             
twitterData <- twitterAuth %>%
               Collect(searchTerm = "#auspol", searchType = "recent", numTweets = 100, 
                       includeRetweets = FALSE, retryOnRateLimit = TRUE, writeToFile = TRUE, 
                       verbose = TRUE)

actorNetwork <- twitterData %>% Create("actor", writeToFile = TRUE, verbose = TRUE)

actorGraph <- actorNetwork$graph # igraph network graph

# Optional step to add additional twitter user info to actor network graph as node attributes 
actorNetWithUserAttr <- AddUserData.twitter(twitterData, actorNetwork,
                                            lookupUsers = TRUE, 
                                            twitterAuth = twitterAuth, writeToFile = TRUE)

actorGraphWithUserAttr <- actorNetWithUserAttr$graph # igraph network graph

semanticNetwork <- twitterData %>% Create("semantic", writeToFile = TRUE)

# Youtube Example

# Authenticate with youtube, Collect comment data from videos and then Create an actor network
myYoutubeAPIKey = "xxxxxxxxxxxxxx"

myYoutubeVideoIds <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=xxxxxxxx",
                                          "https://youtu.be/xxxxxxxx"))
                                 
actorNetwork <- Authenticate("youtube", apiKey = myYoutubeAPIKey) %>%
                Collect(videoIDs = myYoutubeVideoIds) %>%
                Create("actor", writeToFile = TRUE)

# Reddit Example

## Collect reddit comment threads and Create an actor network with comment text as edge attribute
myThreadUrls <- c("https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/")

actorNetwork <- Authenticate("reddit") %>%
                Collect(threadUrls = myThreadUrls, waitTime = 5) %>%
                Create("actor", includeTextData = TRUE, writeToFile = TRUE)
```
For more detailed information and examples, please refer to the function [Reference](https://vosonlab.github.io/vosonSML/reference/index.html) page.

## Special thanks

This package would not be possible without key packages by other authors in the R community, particularly: [igraph](https://github.com/igraph/rigraph), [rtweet](https://github.com/mkearney/rtweet), [RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR), [data.table](https://github.com/Rdatatable/data.table), [tm](https://cran.r-project.org/web/packages/tm/index.html), [magrittr](https://cran.r-project.org/web/packages/magrittr/), [httr](https://github.com/hadley/httr) and [dplyr](https://github.com/hadley/dplyr).
