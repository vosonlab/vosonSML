# vosonSML
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
![Downloads](https://cranlogs.r-pkg.org/badges/vosonSML)
![Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)
![Github Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github&colorB=8065ac)
![Dev](https://img.shields.io/static/v1?label=dev&message=v0.29.2&color=orange&logo=github)
![Last Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?logo=github)

`vosonSML` is an R package that provides a suite of tools for collecting and constructing networks from social media data. It provides easy-to-use functions for collecting data across popular platforms and generating different types of networks for analysis.

`vosonSML` is the `SocialMediaLab` package, renamed. We decided that `SocialMediaLab` was a bit too generic and also we wanted to indicate the connection to the [Virtual Observatory for the Study of Online Networks Lab](http://vosonlab.net), where this package was conceived and created.

`vosonSML` was created by [Timothy Graham](http://uq.academia.edu/TimGraham) and [Robert Ackland](https://researchers.anu.edu.au/researchers/ackland-rj) with major contributions by [Chung-hong Chan](https://github.com/chainsawriot). The current lead developer and maintainer is Bryan Gertzel.

### Supported Social Media

`vosonSML` currently features the collection of data and generation of networks from `twitter`, `youtube` and `reddit`. 

Unfortunately we are no longer able to maintain `facebook` and `instagram` collection, however code for these platforms is still available in [releases](https://github.com/vosonlab/vosonSML/releases) prior to version `0.25.0`.

## Installation

Install the latest release via CRAN:
```R
install.packages("vosonSML")
```

Install the latest release via GitHub:
```R
install.packages("https://github.com/vosonlab/vosonSML/releases/download/v0.27.2/vosonSML-0.27.2.tar.gz", 
  repo = NULL, type = "source")
```

Install the latest development version:
```R
# library(devtools)
devtools::install_github("vosonlab/vosonSML")
```

## Getting started

The following usage examples will provide a great introduction to using `vosonSML`. There are also several "how to" guides, including an "Absolute Beginners Guide to vosonSML" tutorial aimed at people with little or no programming experience on the [vosonSML page of the VOSON website](http://vosonlab.net/SocialMediaLab).

Additional resources:
- [Function Reference](https://vosonlab.github.io/vosonSML/reference/index.html)
- [Updates & Changelog](https://vosonlab.github.io/vosonSML/news/index.html)

### Usage

The process of authentication, data collection and creating social network in vosonSML is expressed with the three verb functions: *Authenticate*, *Collect* and *Create*. The following are some examples:

### Twitter Example

#### 'Authenticate' with the Twitter API
```R
library(magrittr)
library(vosonSML)

myKeys <- list(appName = "My App", apiKey = "xxxxxxxxxxxx", apiSecret = "xxxxxxxxxxxx", 
               accessToken = "xxxxxxxxxxxx", accessTokenSecret = "xxxxxxxxxxxx")
  
twitterAuth <- Authenticate("twitter", appName = myKeys$appName, apiKey = myKeys$apiKey, 
                            apiSecret = myKeys$apiSecret, accessToken = myKeys$accessToken,
                            accessTokenSecret = myKeys$accessTokenSecret)

# twitter authentication creates an access token as part of the auth object
# this can and should be re-used by saving it and then loading it for future sessions
# save the auth object after authenticate 
saveRDS(twitterAuth, file = "~/.twitter_auth")

# load a previously saved auth object for use in collect
twitterAuth <- readRDS("~/.twitter_auth")
```

#### 'Collect' tweets for the '#auspol' hashtag
```R
# collect 100 recent tweets
twitterData <- twitterAuth %>%
               Collect(searchTerm = "#auspol", searchType = "recent", numTweets = 100, 
                       includeRetweets = FALSE, retryOnRateLimit = TRUE, writeToFile = TRUE, 
                       verbose = TRUE)
```

#### 'Create' twitter 'activity', 'actor', 'semantic' and 'bimodal' network graphs
```R
## activity network - nodes are tweets

activityNetwork <- twitterData %>% Create("activity")
activityGraph <- activityNetwork %>% Graph() # igraph network graph

## actor network - nodes are users who have tweeted

actorNetwork <- twitterData %>% Create("actor")
actorGraph <- actorNetwork %>% Graph() # igraph network graph

## semantic network - relationships between concepts - nodes are common terms, hashtags
## and actors

remItems <- c("#auspol", "auspol") # exclude these terms
topTerms <- 5                      # include only the top 5% most frequent terms as nodes
semanticNetwork <- twitterData %>% Create("semantic", removeTermsOrHashtags = remItems, 
                                          termFreq = topTerms)
semanticGraph <- semanticNetwork %>% Graph(writeToFile = TRUE, directed = FALSE)

## bimodal network - nodes are actors and hashtags

remItems <- c("#auspol") # exclude these hashtags
bimodalNetwork <- twitterData %>% Create("bimodal", removeTermsOrHashtags = remItems)
bimodalGraph <- bimodalNetwork %>% Graph(writeToFile = TRUE)
```

### Youtube Example

#### 'Authenticate', 'Collect' and 'Create' network graphs from youtube video comments
```R
library(magrittr)
library(vosonSML)

myYoutubeAPIKey <- "xxxxxxxxxxxxxx"

# helper to create a list of youtube video ids from urls
myYoutubeVideoIds <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=xxxxxxxx",
                                          "https://youtu.be/xxxxxxxx"))

# authenticate and collect 100 top-level comments per youtube video in list
# also collects reply-comments for each top-level comment
youtubeData <- Authenticate("youtube", apiKey = myYoutubeAPIKey) %>%
               Collect(videoIDs = myYoutubeVideoIds, maxComments = 100)

## activity network - nodes are comments and videos

activityNetwork <- youtubeData %>% Create("activity") %>% AddText(youtubeData)
activityGraph <- activityNetwork %>% Graph()

## actor network - nodes are users who have posted comments

actorGraph <- youtubeData %>% Create("actor") %>% AddText(youtubeData) %>% Graph()
```

### Reddit Example

#### 'Collect' and 'Create' reddit networks from a subreddit thread
```R
library(magrittr)
library(vosonSML)

# collect reddit comment threads
myThreadUrls <- c("https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/")

# authentication does not require credentials
redditData <- Authenticate("reddit") %>%
              Collect(threadUrls = myThreadUrls, waitTime = 5)
              
## activity network - nodes are comments and intital thread posts

activityNetwork <- redditData %>% Create("activity")
activityGraph <- activityNetwork %>% Graph(writeToFile = TRUE)

## actor network - nodes are users who have posted comments

# create an actor network with comment text as edge attribute
actorGraph <- redditData %>% Create("actor") %>% AddText(redditData) %>% Graph()
```

### Supplemental Functions

#### 'AddText' adds collected text data to networks as node or edge attributes
```R
# applies to twitter, youtube and reddit - activity and actor networks

# graph for activity network with text data added as node attribute
activityNetworkGraph <- twitterData %>% Create("activity") %>% AddText(twitterData) %>%
                        Graph()
                        
# AddText will also redirect some edges in a youtube actor network by finding user
# references at the beginning of reply comments text
# i.e a reply comment from user_B to top-level comment by user_A
#     user_B: "@user_C A very fine point!"
# this would typically create an edge between user_B -> user_A, however the parameter
# 'replies_from_text' redirects this edge instead from user_B -> user_C as per the
# reference in the comment text - set this to 'FALSE' to ignore comment references

actorNetworkGraph <- youtubeData %>% Create("actor") %>% 
                     AddText(youtubeData, replies_from_text = TRUE) %>% Graph()
```

#### 'AddUserData' requests and adds user profile data to networks
```R
# applies only to twitter actor networks

# add additional twitter user profile info to actor network graph as node attributes
actorGraphWithUserAttr <- actorNetwork %>% 
                          AddUserData(twitterData, 
                                      lookupUsers = TRUE,
                                      twitterAuth = twitterAuth) %>% Graph()
```

#### 'AddVideoData' requests and adds video data to networks
```R
# applies only to youtube actor networks

# replaces 'VIDEOID:xxxxxx' references in actor network with their publishers
# user id (channel ID) and adds additional collected youtube video info to actor
# network graph as node attributes

# if only want the video id substitution use the 'actorSubOnly = TRUE' parameter
actorGraphWithVideos <- actorNetwork %>% AddVideoData(youtubeAuth,
                                                      actorSubOnly = FALSE) %>% Graph()
```

#### Save and Load Authentication Objects

Save and reuse twitter and youtube authentication objects in future sessions.
```R
# save the object after 'Authenticate' 
saveRDS(myYoutubeAuth, file = "~/.youtube_auth")

# load a previously saved authentication object for use in 'Collect'
myYoutubeAuth <- readRDS("~/.youtube_auth")
```
For more detailed function information and examples, please refer to the  [Reference](https://vosonlab.github.io/vosonSML/reference/index.html) page.

## Special thanks

This package would not be possible without key packages by other authors in the R community, particularly: [igraph](https://github.com/igraph/rigraph), [rtweet](https://github.com/mkearney/rtweet), [RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR), [data.table](https://github.com/Rdatatable/data.table), [tm](https://CRAN.R-project.org/package=tm), [magrittr](https://CRAN.R-project.org/package=magrittr), [httr](https://github.com/hadley/httr) and [dplyr](https://github.com/hadley/dplyr).
