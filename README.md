# vosonSML - Social Media Lab<img src="https://vosonlab.github.io/vosonSML/images/logo.png" width="140px" align="right" />
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Downloads](https://cranlogs.r-pkg.org/badges/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Github Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github)](https://github.com/vosonlab/vosonSML/releases)
[![Dev](https://img.shields.io/static/v1?label=dev&message=v0.30.5&logo=github)](https://github.com/vosonlab/vosonSML)
[![Last Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?&logo=github)](https://github.com/vosonlab/vosonSML/commits/master)
[![R build status](https://github.com/vosonlab/vosonSML/workflows/R-CMD-check/badge.svg)](https://github.com/vosonlab/vosonSML/actions)

The `vosonSML` R package is a suite of easy to use functions for collecting and generating different types of networks from social media data. The package supports the collection of data from `twitter`, `youtube` and `reddit`, as well as `hyperlinks` from web sites. Networks in the form of node and edge lists can be generated from collected data, supplemented with additional metadata and used to create graphs for Social Network Analysis.

## Installation

Install the latest release via CRAN:
``` r
install.packages("vosonSML")
```

Install the latest release via GitHub:
``` r
install.packages(
  paste0("https://github.com/vosonlab/vosonSML/releases/download/",
         "v0.29.13/vosonSML-0.29.13.tar.gz"),
  repo = NULL, type = "source")
```

Install the development version:
``` r
library(remotes)
remotes::install_github("vosonlab/vosonSML")
```

## Getting started

The following usage examples will provide a quick start to using `vosonSML` functions. Additionally there is an [Introduction to vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html) vignette that is a practical and explanatory guide to collecting data and creating networks.

### Usage

The process of authentication, data collection and creating networks in `vosonSML` is expressed with the three functions: *Authenticate*, *Collect* and *Create*. The following are some examples of their usage for supported social media:

### Twitter Examples

#### Authenticate with the Twitter API

`Authenticate` is used to create an object that contains a Twitter OAuth 1.0a user token for accessing the twitter API. This can and should be re-used by saving it once to file after calling `Authenticate` and then by loading it again during future sessions.

``` r
library(magrittr)
library(vosonSML)

# twitter authentication creates an access token as part of the auth object
twitter_auth <- Authenticate("twitter", 
                             appName = "My App",
                             apiKey = "xxxxxxxxxxxx",
                             apiSecret = "xxxxxxxxxxxx",
                             accessToken = "xxxxxxxxxxxx",
                             accessTokenSecret = "xxxxxxxxxxxx")

# save the object to file after authenticate
saveRDS(twitter_auth, file = "~/.twitter_oauth1.0a")
```
``` r
# load a previously saved auth object for use in collect
twitter_auth <- readRDS("~/.twitter_oauth1.0a")
```

#### Collect tweets for the #auspol hashtag

`Collect` can be used to perform a twitter search with a search term. The following example collects 100 `recent` tweets for the hashtag `#auspol` and creates a dataframe with the collected tweet data.

``` r
# collect 100 recent tweets for the hashtag #auspol
twitter_data <- twitter_auth %>%
  Collect(searchTerm = "#auspol",
          searchType = "recent",
          numTweets = 100,
          includeRetweets = TRUE,
          retryOnRateLimit = TRUE,
          writeToFile = FALSE,
          verbose = TRUE)

# Collecting tweets for search query...
# Search term: #auspol
# Requested 100 tweets of 18000 in this search rate limit.
# Less tweets requested than remaining limit retryOnRateLimit set to FALSE.
# Rate limit reset: 2021-06-21 12:30:34
# 
# tweet    | status_id           | created             | screen_name   
# ---------------------------------------------------------------------
# Min ID   | 1406797594622251009 | 2021-06-21 02:14:18 | rxxxxxxxxxxxxx
# Max ID   | 1406797912982523908 | 2021-06-21 02:15:34 | jxxxxxxx      
# Last Obs | 1406797594622251009 | 2021-06-21 02:14:18 | rxxxxxxxxxxxxx
# Collected 100 tweets.
# Done.
# Elapsed time: 0 hrs 0 mins 1 secs (1.42)
```

#### Create twitter activity, actor, semantic and 2-mode network graphs

The twitter `Create` function accepts the data from `Collect` and a type parameter of `activity`, `actor`, `semantic` or `twomode` that specifies the type of network to create from the collected data. `Create` produces two dataframes, one for network `nodes` and one for node relations or `edges` in the network. These can then undergo further processing as per the [supplemental functions](#supplemental-functions) section or be passed to the `Graph` function that creates an `igraph` object.

##### Activity network

Nodes are tweets and edges are the relationship to other tweets such as `reply`, `retweet` or `quote` tweets.
``` r
activity_network <- twitter_data %>% 
  Create("activity")

# Generating twitter activity network...
# -------------------------
# collected tweets | 100
# tweet            | 22
# retweet          | 71
# reply            | 5 
# quote            | 2 
# nodes            | 164
# edges            | 100
# -------------------------
# Done.
```
``` r
g <- activity_network %>% Graph(writeToFile = TRUE)

# Creating igraph network graph...
# GRAPHML file written: 2021-06-21_122758-TwitterActivity.graphml
# Done.

# IGRAPH 4ad3a17 DN-- 164 100 -- 
# + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name (v/c),
# | created_at (v/c), user_id (e/c), screen_name (e/c), created_at (e/c), edge_type
# | (e/c)
# + edges from 4ad3a17 (vertex names):
#  [1] 1406797912982523908->1406384652449845248 1406797910323257347->1406797910323257347
#  [3] 1406797909920727047->1406797800692617218 1406797907089518592->1406795212727361542
#  [5] 1406797906095476736->1406796305091813376 1406797690839601157->1406796324767375360
#  [7] 1406797736121278468->1406795813947277313 1406797661433257985->1406795727632691202
#  [9] 1406797905923432449->1406797905923432449 1406797899485179906->1406059808122294276
# [11] 1406797683130462208->1406796563133779969 1406797898122031104->1406789218794307586
# + ... omitted several edges
```

##### Actor network

Nodes are twitter users and edges are the relationship to other users in the network such as `reply`, `mention`, `retweet` and `quote` tweets. Mentions can be excluded by setting the parameter `inclMentions` to `FALSE`.
``` r
actor_network <- twitter_data %>%
  Create("actor", inclMentions = TRUE)

# Generating twitter actor network...
# -------------------------
# collected tweets | 100
# tweet mention    | 4 
# tweet            | 22
# retweet          | 71
# reply mention    | 2 
# reply            | 5 
# quote            | 2 
# nodes            | 148
# edges            | 106
# -------------------------
# Done.
```
``` r
g <- actor_network %>% Graph(writeToFile = TRUE)

# Creating igraph network graph...
# GRAPHML file written: 2021-06-21_123015-TwitterActor.graphml
# Done.

# IGRAPH 9d0c972 DN-- 148 106 -- 
# + attr: type (g/c), name (v/c), screen_name (v/c), status_id (e/c), created_at
# | (e/c), edge_type (e/c)
# + edges from 9d0c972 (vertex names):
#  [1] 1015917930         ->3288075858          216876629          ->216876629          
#  [3] 198296897          ->759204738609152000  1580665164         ->1029612428026204160
#  [5] 989078279620579328 ->18621574            989078279620579328 ->1892572652         
#  [7] 989078279620579328 ->130668639           989078279620579328 ->270343236          
#  [9] 14128457           ->14128457            2611093651         ->870629200348274688 
# [11] 2611093651         ->270670221           732808426212986882 ->1107255371482038272
# [13] 732808426212986882 ->1054893957652434944 1385453900266835971->1385453900266835971
# + ... omitted several edges
```

##### Semantic network

Nodes are concepts represented as common `words` and `hashtags`. Edges represent the occurence of a particular word and a particular hashtag in the same tweet.
``` r
install.packages(c("tidyr", "tidytext", "stopwords")) # install additional required packages

# create a semantic network excluding the hashtag #auspol, include only the top 10%
# most frequent words and 20% most frequent hashtags as nodes
semantic_network <- twitter_data %>% 
  Create("semantic",
         removeTermsOrHashtags = c("#auspol"),
         termFreq = 10,
         hashtagFreq = 20)

# Generating twitter semantic network...
# Removing terms and hashtags: #auspol
# Removing stopwords.
# -------------------------
# collected tweets           | 100
# tokens                     | 2443
# removed specified          | 98 
# removed users              | 38 
# hashtag count              | 115
# unique hashtags            | 69 
# top 20% hashtags (freq>=2) | 15 
# term count                 | 1078
# unique terms               | 678
# top 10% terms (freq>=3)    | 78 
# nodes                      | 93 
# edges                      | 71 
# -------------------------
# Done.
```
``` r
g <- semantic_network %>% Graph(writeToFile = TRUE)

# Creating igraph network graph...
# GRAPHML file written: 2021-06-21_123939-TwitterSemantic.graphml
# Done.

# IGRAPH ed2136a UNWB 93 71 -- 
# + attr: type (g/c), name (v/c), n (v/n), type (v/c), weight (e/n)
# + edges from ed2136a (vertex names):
#  [1] back      --#breaking   barnaby   --#breaking   good      --#breaking  
#  [4] joyce     --#breaking   leader    --#breaking   leadership--#breaking  
#  [7] national  --#breaking   #breaking --nationals   news      --#breaking  
# [10] party     --#breaking   time      --#breaking   back      --#hometobilo
# [13] govt      --#hometobilo talk      --#hometobilo leadership--#lnpfail   
# [16] lnp       --#lnpfail    #natspill --<U+274C>    #natspill --australia  
# [19] #natspill --australian  #natspill --back        #natspill --barnaby    
# [22] #natspill --change      #natspill --climate     #natspill --deputy     
# + ... omitted several edges
```

##### 2-mode network

Nodes are twitter `users` or `hashtags`. Edges represent the use of a `hashtag` or the reference to another `user` in a tweet.
``` r
install.packages("tidytext") # install additional required packages

twomode_network <- twitter_data %>%
  Create("twomode", 
         removeTermsOrHashtags = c("#auspol"),
         weighted = TRUE)

# Generating twitter 2-mode network...
# Removing terms and hashtags: #auspol
# -------------------------
# collected tweets  | 100
# removed specified | 98
# users             | 37
# hashtags          | 115
# nodes             | 155
# edges             | 148
# -------------------------
# Done.
```
``` r
g <- twomode_network %>% Graph(writeToFile = TRUE)

# Creating igraph network graph...
# GRAPHML file written: 2021-06-21_125234-Twitter2mode.graphml
# Done.

# IGRAPH bb244a7 DNW- 155 148 -- 
# + attr: type (g/c), name (v/c), user_id (v/c), weight (e/n)
# + edges from bb244a7 (vertex names):
#  [1] @xxxxxxxxxxxx ->#ausbiz             @xxxxxxxxxxxx ->#australia         
#  [3] @xxxxxxxxxxxx ->#johnnydepp         @xxxxxxxxxxxx ->#tomatoes          
#  [5] @xxxxxxxx     ->#barnaby            @xxxxxxxx     ->#dillspill         
#  [7] @xxxxxxxx     ->#natspill           @xxxxxxxx     ->#qt                
#  [9] @xxxxxxxx     ->#scottyfromphotoops @xxxxxxxxx    ->@liberalaus        
# [11] @xxxxxxxxx    ->@thenationals       @xxxxxxxxxxxxx->#lnpdisgrace       
# [13] @xxxxxxxxxxxxx->#lnpfail            @xxxxxxxxxxxxx->@xxxxxxxxxxxx    
# [15] @xxxxxxxxx    ->@xxxxxxxxxxxxxx     @xxxxxxxxxxxxx->#abc               
# + ... omitted several edges
```

### YouTube Examples

#### Authenticate and Collect comments from youtube videos

YouTube uses an API key rather than an OAuth token and is simply set by calling `Authenticate` with the key as a parameter.
``` r
# youtube authentication sets the api key
youtube_auth <- Authenticate("youtube", apiKey = "xxxxxxxxxxxx")
```

Once the key is set then `Collect` can be used to collect the comments from specified youtube videos. The following example collects a maximum of 100 top-level comments and all replies from each of the 2 specified video ID's. It produces a dataframe with the combined comment data.
``` r
# helper to create a list of youtube video ids from urls
youtube_video_ids <- GetYoutubeVideoIDs(c("https://www.youtube.com/watch?v=xxxxxxxx",
                                          "https://youtu.be/xxxxxxxx"))

# Extracted 2 video ids.
```
``` r
youtube_data <- youtube_auth %>%
  Collect(videoIDs = youtube_video_ids,
          maxComments = 100,
          verbose = FALSE)

# Collecting comment threads for youtube videos...
# Video 1 of 2
# ---------------------------------------------------------------
# ** video Id: xxxxxxxx
# -- API returned more than max comments. Results truncated to first 100 threads.
# ** Collected threads: 100
# ** Collecting replies for 4 threads with replies. Please be patient.
# ....
# ** Collected replies: 25
# ** Total video comments: 125
# ---------------------------------------------------------------
# Video 2 of 2
# ---------------------------------------------------------------
# ** video Id: xxxxxxxx
# -- API returned more than max comments. Results truncated to first 100 threads.
# ** Collected threads: 100
# ** Collecting replies for 2 threads with replies. Please be patient.
# ..
# ** Collected replies: 22
# ** Total video comments: 122
# ---------------------------------------------------------------
# ** Total comments collected for all videos 247.
# (Estimated API unit cost: 24)
# Done.
```

#### Create youtube activity and actor network graphs

The youtube `Create` function accepts the data from `Collect` and a network type parameter of `activity` or `actor`.

##### Activity network

Nodes are video comments and edges represent whether they were directed to the video as a top-level comment or to another comment as a reply comment.
``` r
activity_network <- youtube_data %>% Create("activity")

# Generating youtube activity network...
# -------------------------
# collected youtube comments | 247
# top-level comments         | 200
# reply comments             | 47
# videos                     | 2 
# nodes                      | 249
# edges                      | 247
# -------------------------
# Done.
```
``` r
g <- activity_network %>% Graph()

# Creating igraph network graph...Done.

# IGRAPH 9c000b6 DN-- 249 247 -- 
# + attr: type (g/c), name (v/c), video_id (v/c), published_at (v/c), updated_at (v/c),
# | author_id (v/c), screen_name (v/c), node_type (v/c), label (v/c), edge_type (e/c)
```

##### Actor network

Nodes are users who have posted comments and the video publishers, edges represent comments directed at other users. 
``` r
actor_network <- youtube_data %>% Create("actor")
```
``` r
g <- actor_network %>% Graph()

# Generating youtube actor network...Done.

# Creating igraph network graph...Done.
# IGRAPH 2f87356 DN-- 214 249 -- 
# + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c), label (v/c),
# | video_id (e/c), comment_id (e/c), edge_type (e/c)
```

### Reddit Examples

#### Authenticate and Collect from reddit threads

The reddit API end-point used by `vosonSML` does not require authentication but an `Authenticate` object is still used to set up the collection and creation operations as part of a reddit workflow. The reddit `Collect` function can then be used to collect comments from reddit threads specified by URL's.
``` r
# specify reddit threads to collect by url
reddit_thread_urls <-
  c("https://www.reddit.com/r/datascience/comments/g2k5zi/xxxx_xxxx_xxxxxxxxxx/",
    "https://www.reddit.com/r/datascience/comments/g1suaz/xx_xxxx_xxx_xxxxxxx/")

# authentication does not require credentials
reddit_data <- Authenticate("reddit") %>%
  Collect(threadUrls = reddit_thread_urls)

# Collecting comment threads for reddit urls...
# Waiting between 3 and 5 seconds per thread request.
# Request thread: r/datascience (g2k5zi)
# Request thread: r/datascience (g1suaz)
# HTML decoding comments.
# thread_id | title                                         | subreddit   | count
# -------------------------------------------------------------------------------
# g1suaz    | xx xxxx xxx xxxxxxx xxx xx xxx xxxxxxx xxx... | datascience | 23  
# g2k5zi    | xxxx xxxx xxxxxxxxxxx xxx xxx xxxx xxxxxxx... | datascience | 38 
# Collected 61 total comments.
# Done.
```

Please note that because of the API end-point used that `Collect` is limited to the first 500 comments per thread. It is therefore suited to collecting only smaller threads in their entirety.

#### Create reddit activity and actor networks

##### Activity network

Nodes are original thread posts and comments, edges are replies directed to the original post and to comments made by others.
``` r
# create an activity network
activity_network <- reddit_data %>% Create("activity")

# Generating reddit activity network...
# -------------------------
# collected reddit comments | 61
# threads                   | 2
# comments                  | 61
# nodes                     | 63
# edges                     | 61
# -------------------------
# Done.
```
``` r
g <- activity_network %>% Graph()

# Creating igraph network graph...Done.

# IGRAPH 8ed7423 DN-- 63 61 -- 
# + attr: type (g/c), name (v/c), thread_id (v/c), comm_id (v/c), datetime (v/c),
# | ts (v/n), subreddit (v/c), user (v/c), node_type (v/c), label (v/c), edge_type (e/c)
```

##### Actor network

Nodes are reddit users who have commented on threads and edges represent replies to other users.
``` r
# create an actor network
actor_network <- reddit_data %>% Create("actor")

# Generating reddit actor network...Done.
```
``` r
g <- actor_network %>% Graph()

# Creating igraph network graph...Done.

# Creating igraph network graph...Done.
# IGRAPH de82e44 DN-- 37 63 -- 
# + attr: type (g/c), name (v/c), user (v/c), label (v/c), subreddit (e/c), thread_id (e/c),
# | comment_id (e/n), comm_id (e/c)
```

### Hyperlink Examples

#### Authenticate and Collect from web sites

The `vosonSML` hyperlink collection functionality does not require authentication as it is not using any web API's, however an `Authenticate` object is still used to set up the collection and creation operations as part of the `vosonSML` workflow.

The hyperlink `Collect` function accepts a dataframe of seed web pages, as well as corresponding `type` and `max_depth` parameters for each page.

``` r
# specify seed web pages and parameters for hyperlink collection
seed_page_urls <-
  data.frame(page = c("http://vosonlab.net",
                      "https://www.oii.ox.ac.uk",
                      "https://sonic.northwestern.edu"),
             type = c("ext", "ext", "ext"),
             max_depth = c(2, 2, 2))

hyperlink_data <- Authenticate("web") %>%
  Collect(pages = seed_page_urls)

# Collecting web page hyperlinks...
# *** initial call to get urls - http://vosonlab.net
# * new domain: http://vosonlab.net 
# + http://vosonlab.net (10 secs)
# *** end initial call
# *** set depth: 2
# *** loop call to get urls - nrow: 6 depth: 2 max_depth: 2
# * new domain: http://rsss.anu.edu.au 
# + http://rsss.anu.edu.au (0.96 secs)
# ...
```

#### Create activity and actor networks

``` r
# generate a hyperlink activity network
activity_network <- Create(hyperlink_data, "activity")

# generate a hyperlink actor network
actor_network <- Create(hyperlink_data, "actor")

# Generating web actor network...
# Done.
```

### <a name="supplemental-functions"></a>Supplemental Functions

#### AddText adds collected text data to networks as node or edge attributes

The `AddText` function can be used following the creation of `activity` or `actor` networks for `twitter`, `youtube` and `reddit`. It will add an attribute starting with `vosonTxt_` to nodes of `activity` networks and to edges of `actor` networks. It requires a collected `datasource` from which to extract text data.
``` r
# create activity network
activity_network <- twitter_data %>% Create("activity")

# Generating twitter activity network...Done.
```
``` r
# activity network with text data added as node attribute
activity_network <- activity_network %>% AddText(twitter_data)

# Adding text to network...Done.
```
``` r
g <- activity_network %>% Graph()

# Creating igraph network graph...Done.

# IGRAPH 9ea0e40 DN-- 159 63 -- 
# + attr: type (g/c), name (v/c), user_id (v/c), screen_name (v/c), created_at (v/c),
# | vosonTxt_tweet (v/c), label (v/c), edge_type (e/c)
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
actor_network <- youtube_data %>% Create("actor")

# detects replies to users in text
actor_network <- actor_network %>%
  AddText(youtube_data,
          repliesFromText = TRUE)
```

#### AddUserData requests and adds user profile data to networks

`AddUserData` adds user profile information to as many users in a twitter `actor` network as possible. If the profile information is not available in the collect data and the `lookupUsers` parameter is set then it will make twitter API requests to retrieve them. The profile information is added as additional node attributes.
``` r
# add additional twitter user profile info
actor_network <- actor_network %>%
  AddUserData(twitter_data,
              lookupUsers = TRUE,
              twitterAuth = twitter_auth)

# Adding user profile data to network...
# Fetching user information for 80 users.
# User information collected for 80 users.
# Done.
```
For reference the `AddUserData` function will also add a new dataframe to the `actor_network` network list containing the retrieved user profiles called `users`.
``` r
names(actor_network)
# [1] "edges" "nodes" "users"
nrow(actor_network$users)
# [1] 80
```
``` r
actor_network <- actor_network %>% Graph()

# Creating igraph network graph...Done.

# IGRAPH ab0ef4c DN-- 162 153 -- 
# + attr: type (g/c), name (v/c), screen_name (v/c), display_name (v/c), location (v/c),
# | description (v/c), url (v/c), protected (v/c), followers_count (v/n),
# | friends_count (v/n), listed_count (v/n), statuses_count (v/n), favourites_count (v/n),
# | account_created_at (v/c), verified (v/c), profile_url (v/c), profile_expanded_url (v/c),
# | account_lang (v/c), profile_banner_url (v/c), profile_background_url (v/c),
# | profile_image_url (v/c), label (v/c), edge_type (e/c), timestamp (e/c), status_id (e/c)
```
#### AddVideoData requests and adds video data to networks

`AddVideoData` adds video information as node attributes in youtube `actor` networks and replaces the video ID nodes with a user (channel owner or publisher). The `actorSubOnly` parameter can be used to only perform the ID substitution.
``` r
# replaces VIDEOID:xxxxxx references in actor network with their publishers
# user id (channel ID) and adds additional collected youtube video info to actor
# network graph as node attributes
actor_network <- actor_network %>% 
  AddVideoData(youtube_auth,
               actorSubOnly = FALSE) %>%

# Adding video data to network...Done.
```
`AddVideoData` function will also add a new dataframe to the `actor_network` network list containing the retrieved video information called `videos`.
``` r
names(actor_network)
# [1] "edges" "nodes" "videos"
nrow(actor_network$videos)
# [1] 2
```
``` r
actor_network <- actor_network %>% Graph()

# Creating igraph network graph...Done.

# IGRAPH de53212 DN-- 212 249 -- 
# + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c), label (v/c),
# | video_id (e/c), comment_id (e/c), edge_type (e/c), video_title (e/c),
# | video_description (e/c), video_published_at (e/c)
```

#### Save and Load Authentication Objects

Save and reuse twitter and youtube authentication objects in future sessions.
``` r
# save the object after Authenticate
saveRDS(youtube_auth, file = "~/.youtube_auth")

# load a previously saved authentication object for use in Collect
youtube_auth <- readRDS("~/.youtube_auth")
```

## Where to next?

Continue working with the network graphs using the `igraph` package and check out some examples of plots in the [Introduction to vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html) vignette. The `graphml` files produced by `vosonSML` are also easily imported into software such as [Gephi](https://gephi.org/) for further visualization and exploration of networks.

As an alternative to `vosonSML` using the R command-line interface we have also developed an R Shiny app called [VOSON Dash](https://vosonlab.github.io/VOSONDash/). It provides a user friendly GUI for the collection of data using `vosonSML` and has additional network visualization and analysis features.

For more detailed information about functions and their parameters, please refer to the [Reference](https://vosonlab.github.io/vosonSML/reference/index.html) page.

## Special thanks

This package would not be possible without key packages by other authors in the R community, particularly: [data.table](https://github.com/Rdatatable/data.table), [dplyr](https://github.com/tidyverse/dplyr), [httr](https://github.com/r-lib/httr), [igraph](https://github.com/igraph/rigraph), [magrittr](https://CRAN.R-project.org/package=magrittr), [RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR), [rtweet](https://github.com/ropensci/rtweet) and [tidytext](https://github.com/juliasilge/tidytext).
