# vosonSML <img src="https://vosonlab.github.io/vosonSML/images/logo.png" width="140px" align="right" />
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Downloads](https://cranlogs.r-pkg.org/badges/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Github Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github&colorB=8065ac)](https://github.com/vosonlab/vosonSML/releases)
[![Dev](https://img.shields.io/static/v1?label=dev&message=v0.29.9&color=659DBD&logo=github)](https://github.com/vosonlab/vosonSML)
[![Last Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?&color=659DBD&logo=github)](https://github.com/vosonlab/vosonSML/commits/master)

`vosonSML` is an R package that provides a suite of tools for collecting and constructing networks from social media data. It provides easy-to-use functions for collecting data across popular platforms and generating different types of networks for analysis.

`vosonSML` is the `SocialMediaLab` package, with significant improvements and enhancements. We renamed the package because we thought that `SocialMediaLab` sounded a bit generic and also we wanted to indicate the connection to the [Virtual Observatory for the Study of Online Networks Lab](http://vosonlab.net), where te package was created. The original `SocialMediaLab` package was created by [Timothy Graham](https://github.com/timothyjgraham) and [Robert Ackland](https://github.com/rjackland) with major contributions by [Chung-hong Chan](https://github.com/chainsawriot). The development and maintenance of `vosonSML` is led by [Bryan Gertzel](https://github.com/mishoryu).

### Supported Social Media

`vosonSML` currently features the collection of data and generation of networks from `twitter`, `youtube` and `reddit`.

Unfortunately we are no longer able to maintain `facebook` and `instagram` collection, however code for these platforms is still available in [releases](https://github.com/vosonlab/vosonSML/releases) prior to version `0.25.0`.

## Installation

Install the latest release via CRAN (v0.29.10):
``` r
install.packages("vosonSML")
```

Install the latest release via GitHub (v0.29.10):
``` r
install.packages("https://github.com/vosonlab/vosonSML/releases/download/v0.29.10/vosonSML-0.29.10.tar.gz",
  repo = NULL, type = "source")
```

Install the latest development version (v0.29.10):
``` r
# library(devtools)
devtools::install_github("vosonlab/vosonSML")
```

## Getting started

The following usage examples will provide a quick start to using `vosonSML` functions. There are also several "how to" guides, including an "Absolute Beginners Guide to vosonSML" tutorial aimed at people with little or no programming experience on the [vosonSML page of the VOSON website](http://vosonlab.net/SocialMediaLab). Additionally there is an [Introduction to vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html) vignette included with the `vosonSML` package is that is a practical and explanatory guide to collecting data and creating networks.

Further resources:
- [Function Reference](https://vosonlab.github.io/vosonSML/reference/index.html)
- [Updates & Changelog](https://vosonlab.github.io/vosonSML/news/index.html)

### Usage

The process of authentication, data collection and creating social network in `vosonSML` is expressed with the three functions: *Authenticate*, *Collect* and *Create*. The following are some examples of their usage for supported social media:

### Twitter Examples

#### 'Authenticate' with the Twitter API

`Authenticate` is used to create an object that contains an OAuth token for accessing the twitter API. This can and should be re-used by saving it once to file after calling `Authenticate` and then by loading it again during future sessions.

``` r
library(magrittr)
library(vosonSML)

# twitter authentication creates an access token as part of the auth object
twitterAuth <- Authenticate("twitter", 
                            appName = "My App",
                            apiKey = "xxxxxxxxxxxx",
                            apiSecret = "xxxxxxxxxxxx",
                            accessToken = "xxxxxxxxxxxx",
                            accessTokenSecret = "xxxxxxxxxxxx")

# save the object to file after authenticate
saveRDS(twitterAuth, file = "~/.twitter_auth")
```
``` r
# load a previously saved auth object for use in collect
twitterAuth <- readRDS("~/.twitter_auth")
```

#### 'Collect' tweets for the '#auspol' hashtag

`Collect` can be used to perform a twitter search with a search term. The following example collects 100 `recent` tweets for the hashtag `#auspol` and creates a dataframe with the collected tweet data.

``` r
# collect 100 recent tweets for the hashtag #auspol
twitterData <- twitterAuth %>%
  Collect(searchTerm = "#auspol",
          searchType = "recent",
          numTweets = 100,
          includeRetweets = FALSE,
          retryOnRateLimit = TRUE,
          writeToFile = FALSE,
          verbose = TRUE)
```
``` r
#> Collecting tweets for search query...
#> Search term: #auspol
#> Requested 100 tweets of 18000 in this search rate limit.
#> Less tweets requested than remaining limit retryOnRateLimit set to FALSE.
#> Rate limit reset: 2020-04-20 00:13:02
#> tweet  | status_id           | created             | screen_name 
#> -----------------------------------------------------------------
#> Min ID | 125182xxxxxxxxxxxxx | 2020-04-19 13:07:11 | @xxxxxx 
#> Max ID | 125187xxxxxxxxxxxxx | 2020-04-19 13:58:00 | @xxxxxxxxxx
#> Collected 100 tweets.
#> Done.
```

#### 'Create' twitter 'activity', 'actor', 'semantic' and 'twomode' network graphs

The twitter `Create` function accepts the data from `Collect` and a type parameter of `activity`, `actor`, `semantic` or `twomode` that specifies the type of network to create from the collected data. `Create` produces two dataframes, one for network `nodes` and one for node relations or `edges` in the network. These can then undergo further processing as per the [supplemental functions](#supplemental-functions) section or be passed to the `Graph` function that creates an `igraph` object.

##### Activity network

Nodes are tweets and edges are the relationship to other tweets such as reply, retweet or quote tweets.
``` r
activityNetwork <- twitterData %>% 
  Create("activity") %>%
  Graph(writeToFile = TRUE) %>%
  summary()
```
``` r
#> Generating twitter activity network...
#> -------------------------
#> collected tweets | 100
#> quote tweets     | 29
#> reply tweets     | 34
#> tweets           | 37
#> nodes from data  | 59
#> nodes            | 159
#> edges            | 63
#> -------------------------
#> Done.
#> Creating igraph network graph...
#> GRAPHML file written: D:/wd/2020-04-20_001605-TwitterActivity.graphml
#> Done.
#> IGRAPH 4e5f4e6 DN-- 159 63 -- 
#> + attr: type (g/c), name (v/c), user_id (v/c), screen_name (v/c), created_at (v/c),
#> | label (v/c), edge_type (e/c)
```

##### Actor network

Nodes are twitter users and edges are the relationship to other users in the network such as replies, mentions, retweets and quote tweets.
``` r
actorNetwork <- twitterData %>%
  Create("actor") %>%
  Graph() %>%
  summary()
```
``` r
#> Generating twitter actor network...
#> -------------------------
#> collected tweets | 100
#> retweets         | 0 
#> quoting others   | 29
#> mentions         | 18
#> reply mentions   | 44
#> replies          | 37
#> self-loops       | 25
#> nodes            | 162
#> edges            | 153
#> -------------------------
#> Done.
#> Creating igraph network graph...Done.
#> IGRAPH 20690e6 DN-- 162 153 -- 
#> + attr: type (g/c), name (v/c), screen_name (v/c), label (v/c), edge_type (e/c),
#> | timestamp (e/c), status_id (e/c)
```

##### Semantic network

Nodes are concepts represented as common words and hashtags, edges represent the occurence of a word and hashtag in the same tweet.
``` r
install.packages("tidytext", "tidyr") # install additional required packages

# create a semantic network excluding the hashtag #auspol, include only the top 10%
# most frequent words and 20% most frequent hashtags as nodes
semanticNetwork <- twitterData %>% 
  Create("semantic",
         removeTermsOrHashtags = c("#auspol"),
         termFreq = 10,
         hashtagFreq = 20) %>%
  Graph(writeToFile = TRUE) %>%
  summary()
```
``` r
#> Generating twitter semantic network...
#> Removing terms and hashtags: #auspol
#> Removing stopwords.
#> -------------------------
#> collected tweets           | 100
#> tokens                     | 2737
#> removed specified          | 95 
#> removed users              | 97 
#> hashtag count              | 82 
#> unique hashtags            | 60 
#> top 20% hashtags (freq>=2) | 13 
#> term count                 | 1159
#> unique terms               | 836
#> top 10% terms (freq>=2)    | 186
#> nodes                      | 199
#> edges                      | 95 
#> -------------------------
#> Done.
#> Creating igraph network graph...
#> GRAPHML file written: D:/wd/2020-04-20_003304-TwitterSemantic.graphml
#> Done.
#> IGRAPH ae1da92 UNWB 199 95 -- 
#> + attr: type (g/c), name (v/c), n (v/n), type (v/c), label (v/c), weight (e/n)
```

##### Twomode network

Nodes are twitter users and hashtags, edges represent the use of a hashtag or the reference to another user in a tweet.
``` r
twomodeNetwork <- twitterData %>%
  Create("twomode", 
         removeTermsOrHashtags = c("#auspol"),
         weighted = TRUE,
         verbose = TRUE) %>%
  Graph() %>%
  summary()
```
``` r
#> Generating twitter twomode network...
#> Removing terms and hashtags: #auspol
#> -------------------------
#> collected tweets  | 100
#> removed specified | 95
#> users             | 97
#> hashtags          | 82
#> nodes             | 176
#> edges             | 166
#> -------------------------
#> Done.
#> Creating igraph network graph...
#> GRAPHML file written: D:/wd/2020-04-20_003907-TwitterTwomode.graphml
#> Done.
#> IGRAPH 86847fb DNW- 176 166 -- 
#> + attr: type (g/c), name (v/c), user_id (v/c), label (v/c), weight (e/n)
```

### Youtube Examples

#### 'Authenticate' and 'Collect' comments from youtube videos

Youtube uses an API key rather than an OAuth token and is simply set by calling `Authenticate` with the key as a parameter.
``` r
# youtube authentication sets the api key
youtubeAuth <- Authenticate("youtube", apiKey = "xxxxxxxxxxxx")
```

Once the key is set then `Collect` can be used to collect the comments from specified youtube videos. The following example collects a maximum of 100 top-level comments and all replies from each of the 2 specified video ID's. It produces a dataframe with the combined comment data.
``` r
# helper to create a list of youtube video ids from urls
youtubeVideoIds <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=xxxxxxxx",
                                        "https://youtu.be/xxxxxxxx"))

#> Extracted 2 video ids.
```
``` r
youtubeData <- youtubeAuth %>%
  Collect(videoIDs = youtubeVideoIds,
          maxComments = 100,
          verbose = FALSE)
```
``` r
#> Collecting comment threads for youtube videos...
#> Video 1 of 2
#> ---------------------------------------------------------------
#> ** video Id: xxxxxxxx
#> -- API returned more than max comments. Results truncated to first 100 threads.
#> ** Collected threads: 100
#> ** Collecting replies for 4 threads with replies. Please be patient.
#> ....
#> ** Collected replies: 25
#> ** Total video comments: 125
#> ---------------------------------------------------------------
#> Video 2 of 2
#> ---------------------------------------------------------------
#> ** video Id: xxxxxxxx
#> -- API returned more than max comments. Results truncated to first 100 threads.
#> ** Collected threads: 100
#> ** Collecting replies for 2 threads with replies. Please be patient.
#> ..
#> ** Collected replies: 22
#> ** Total video comments: 122
#> ---------------------------------------------------------------
#> ** Total comments collected for all videos 247.
#> (Estimated API unit cost: 24)
#> Done.
```

#### 'Create' youtube 'activity' and 'actor' network graphs

The youtube `Create` function accepts the data from `Collect` and a network type parameter of `activity` or `actor`.

##### Activity network

Nodes are video comments and edges represent whether they were directed to the video as a top-level comment or to another comment as a reply comment.
``` r
activityNetwork <- youtubeData %>% Create("activity") %>% Graph() %>% summary()
```
``` r
#> Generating youtube activity network...
#> -------------------------
#> collected youtube comments | 247
#> top-level comments         | 200
#> reply comments             | 47
#> videos                     | 2 
#> nodes                      | 249
#> edges                      | 247
#> -------------------------
#> Done.
#> Creating igraph network graph...Done.
#> IGRAPH 9c000b6 DN-- 249 247 -- 
#> + attr: type (g/c), name (v/c), video_id (v/c), published_at (v/c), updated_at (v/c),
#> | author_id (v/c), screen_name (v/c), node_type (v/c), label (v/c), edge_type (e/c)
```

##### Actor network

Nodes are users who have posted comments and the video publishers, edges represent comments directed at other users. 
``` r
actorNetwork <- youtubeData %>% Create("actor") %>% Graph() %>% summary()
```
``` r
#> Generating youtube actor network...Done.
#> Creating igraph network graph...Done.
#> IGRAPH 2f87356 DN-- 214 249 -- 
#> + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c), label (v/c),
#> | video_id (e/c), comment_id (e/c), edge_type (e/c)
```

### Reddit Examples

#### 'Authenticate' and 'Collect' from reddit threads

The reddit API end-point used by `vosonSML` does not require authentication but an `Authenticate` object is still used to set up the collection and creation operations as part of a reddit workflow. The reddit `Collect` function can then be used to collect comments from reddit threads specified by url's.
``` r
# specify reddit threads to collect by url
redditUrls <- c("https://www.reddit.com/r/datascience/comments/g2k5zi/xxxx_xxxx_xxxxxxxxxx/",
                "https://www.reddit.com/r/datascience/comments/g1suaz/xx_xxxx_xxx_xxxxxxx/")

# authentication does not require credentials
redditData <- Authenticate("reddit") %>% Collect(threadUrls = redditUrls)
```
``` r
#> Collecting comment threads for reddit urls...
#> Waiting between 3 and 5 seconds per thread request.
#> Request thread: r/datascience (g2k5zi)
#> Request thread: r/datascience (g1suaz)
#> HTML decoding comments.
#> thread_id | title                                         | subreddit   | count
#> -------------------------------------------------------------------------------
#> g1suaz    | xx xxxx xxx xxxxxxx xxx xx xxx xxxxxxx xxx... | datascience | 23  
#> g2k5zi    | xxxx xxxx xxxxxxxxxxx xxx xxx xxxx xxxxxxx... | datascience | 38 
#> Collected 61 total comments.
#> Done.
```

Please note that because of the API end-point used that `Collect` is limited to the first 500 comments per thread. It is therefore suited to collecting only smaller threads in their entirety.

#### 'Create' reddit 'activity' and 'actor' networks

##### Activity network

Nodes are original thread posts and comments, edges are replies directed to the original post and to comments made by others.
``` r
# create an activity network
activityNetwork <- redditData %>% Create("activity") %>% Graph() %>% summary()
```
``` r
#> Generating reddit activity network...
#> -------------------------
#> collected reddit comments | 61
#> threads                   | 2
#> comments                  | 61
#> nodes                     | 63
#> edges                     | 61
#> -------------------------
#> Done.
#> Creating igraph network graph...Done.
#> IGRAPH 8ed7423 DN-- 63 61 -- 
#> + attr: type (g/c), name (v/c), thread_id (v/c), comm_id (v/c), datetime (v/c),
#> | ts (v/n), subreddit (v/c), user (v/c), node_type (v/c), label (v/c), edge_type (e/c)
```

##### Actor network

Nodes are reddit users who have commented on threads and edges represent replies to other users.
``` r
# create an actor network
actorNetwork <- redditData %>% Create("actor") %>% Graph() %>% summary()
```
``` r
#> Generating reddit actor network...Done.
#> Creating igraph network graph...Done.
#> IGRAPH de82e44 DN-- 37 63 -- 
#> + attr: type (g/c), name (v/c), user (v/c), label (v/c), subreddit (e/c), thread_id (e/c),
#> | comment_id (e/n), comm_id (e/c)
```

### <a name="supplemental-functions"></a>Supplemental Functions

#### 'AddText' adds collected text data to networks as node or edge attributes

The `AddText` function can be used following the creation of `activity` or `actor` networks for `twitter`, `youtube` and `reddit`. It will add an attribute starting with `vosonTxt_` to nodes of `activity` networks and to edges of `actor` networks. It requires a collected `datasource` from which to extract text data.
``` r
# create activity network
activityNetwork <- twitterData %>% Create("activity", verbose = FALSE)

#> Generating twitter activity network...Done.
```
``` r
# activity network with text data added as node attribute
activityNetwork <- activityNetwork %>%
  AddText(twitterData) %>%
  Graph() %>%
  summary()
```
``` r
#> Adding text to network...Done.
#> Creating igraph network graph...Done.
#> IGRAPH 9ea0e40 DN-- 159 63 -- 
#> + attr: type (g/c), name (v/c), user_id (v/c), screen_name (v/c), created_at (v/c),
#> | vosonTxt_tweet (v/c), label (v/c), edge_type (e/c)
```

`AddText` will also redirect some edges in a youtube `actor` network by finding user references at the beginning of reply comments text using the `repliesFromText` parameter. In the following example an edge would be redirected from `UserC` to `UserB` by text reference as opposed to `UserA` who made the top-level comment both users are replying to.
``` r
# video comments
# UserA: Great tutorial.
# |- UserB: I agree, but it could have had more examples.
# |- UserC: @UserB I thought it probably had too many.
```
Redirect edge between user nodes `C -> A` to `C -> B`.
``` r
# create activity network
actorNetwork <- youtubeData %>% Create("actor")

# detects replies to users in text
actorNetwork <- actorNetwork %>%
  AddText(youtubeData,
          repliesFromText = TRUE)
```

#### 'AddUserData' requests and adds user profile data to networks

`AddUserData` adds user profile information to as many users in a twitter `actor` network as possible. If the profile information is not available in the collect data and the `lookupUsers` parameter is set then it will make twitter API requests to retrieve them. The profile information is added as additional node attributes.
``` r
# add additional twitter user profile info
actorNetwork <- actorNetwork %>%
  AddUserData(twitterData,
              lookupUsers = TRUE,
              twitterAuth = twitterAuth)

#> Adding user profile data to network...
#> Fetching user information for 80 users.
#> User information collected for 80 users.
#> Done.
```
For reference the `AddUserData` function will also add a new dataframe to the `actorNetwork` network list containing the retrieved user profiles called `users`.
``` r
names(actorNetwork)
#> [1] "edges" "nodes" "users"
nrow(actorNetwork$users)
#> [1] 80
```
``` r
actorNetwork <- actorNetwork %>% Graph() %>% summary()
```
``` r
#> Creating igraph network graph...Done.
#> IGRAPH ab0ef4c DN-- 162 153 -- 
#> + attr: type (g/c), name (v/c), screen_name (v/c), display_name (v/c), location (v/c),
#> | description (v/c), url (v/c), protected (v/c), followers_count (v/n),
#> | friends_count (v/n), listed_count (v/n), statuses_count (v/n), favourites_count (v/n),
#> | account_created_at (v/c), verified (v/c), profile_url (v/c), profile_expanded_url (v/c),
#> | account_lang (v/c), profile_banner_url (v/c), profile_background_url (v/c),
#> | profile_image_url (v/c), label (v/c), edge_type (e/c), timestamp (e/c), status_id (e/c)
```
#### 'AddVideoData' requests and adds video data to networks

`AddVideoData` adds video information as node attributes in youtube `actor` networks and replaces the video ID nodes with a user (channel owner or publisher). The `actorSubOnly` parameter can be used to only perform the ID substitution.
``` r
# replaces 'VIDEOID:xxxxxx' references in actor network with their publishers
# user id (channel ID) and adds additional collected youtube video info to actor
# network graph as node attributes
actorNetwork <- actorNetwork %>% 
  AddVideoData(youtubeAuth,
               actorSubOnly = FALSE) %>%

#> Adding video data to network...Done.
```
`AddVideoData` function will also add a new dataframe to the `actorNetwork` network list containing the retrieved video information called `videos`.
``` r
names(actorNetwork)
#> [1] "edges" "nodes" "videos"
nrow(actorNetwork$videos)
#> [1] 2
```
``` r
actorNetwork <- actorNetwork %>% Graph() %>% summary()
```
``` r
#> Creating igraph network graph...Done.
#> IGRAPH de53212 DN-- 212 249 -- 
#> + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c), label (v/c),
#> | video_id (e/c), comment_id (e/c), edge_type (e/c), video_title (e/c),
#> | video_description (e/c), video_published_at (e/c)
```

#### Save and Load Authentication Objects

Save and reuse twitter and youtube authentication objects in future sessions.
``` r
# save the object after 'Authenticate'
saveRDS(myYoutubeAuth, file = "~/.youtube_auth")

# load a previously saved authentication object for use in 'Collect'
myYoutubeAuth <- readRDS("~/.youtube_auth")
```

## Where to next?

Continue working with the network graphs using the `igraph` package and check out some examples of plots in the [Introduction to vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html) vignette. The `graphml` files produced by `vosonSML` are also easily imported into software such as [Gephi](https://gephi.org/) for further visualization and exploration of networks.

As an alternative to `vosonSML` using the R command-line interface we have also developed an R Shiny app called [VOSON Dash](https://vosonlab.github.io/VOSONDash/). It provides a user friendly GUI for the collection of data using `vosonSML` and has additional network visualization and analysis features.

For more detailed information about functions and their parameters, please refer to the [Reference](https://vosonlab.github.io/vosonSML/reference/index.html) page.

## Special thanks

This package would not be possible without key packages by other authors in the R community, particularly: [igraph](https://github.com/igraph/rigraph), [rtweet](https://github.com/mkearney/rtweet), [RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR), [data.table](https://github.com/Rdatatable/data.table), [magrittr](https://CRAN.R-project.org/package=magrittr), [httr](https://github.com/hadley/httr) and [dplyr](https://github.com/hadley/dplyr).
