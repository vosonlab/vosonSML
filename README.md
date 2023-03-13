
# vosonSML - Social Media Lab<img src="https://vosonlab.github.io/vosonSML/images/logo.png" width="140px" align="right" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Monthly](https://cranlogs.r-pkg.org/badges/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Github_Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github)](https://github.com/vosonlab/vosonSML/releases)
[![Github_Dev](https://img.shields.io/static/v1?label=dev&message=v0.33.0&logo=github)](https://github.com/vosonlab/vosonSML)
[![Last_Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?&logo=github)](https://github.com/vosonlab/vosonSML/commits/master)
[![Build_Status](https://github.com/vosonlab/vosonSML/workflows/R-CMD-check/badge.svg)](https://github.com/vosonlab/vosonSML/actions)

The `vosonSML` R package is a suite of easy to use functions for
collecting and generating different types of networks from social media
data. The package supports the collection of data from `twitter`,
`youtube` and `reddit`, as well as `hyperlinks` from web sites. Networks
in the form of node and edge lists can be generated from collected data,
supplemented with additional metadata, and used to create graphs for
Social Network Analysis.

## Installation Options

Install the most recent CRAN release:

``` r
install.packages("vosonSML")
```

Install the most recent release tag via GitHub:

``` r
install.packages(
  "https://github.com/vosonlab/vosonSML/releases/download/v0.32.7/vosonSML-0.32.7.tar.gz",
  repo = NULL, type = "source")
```

Install the latest development version:

``` r
# library(remotes)
remotes::install_github("vosonlab/vosonSML")
```

## Getting started

The following usage examples will provide a quick start to using
`vosonSML` functions. Additionally there is an [Introduction to
vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html)
vignette that is a practical and explanatory guide to collecting data
and creating networks.

### General Usage

The process of authentication, data collection and creating networks in
`vosonSML` is expressed with the three functions: *Authenticate*,
*Collect* and *Create*. The following are some examples of their usage
for supported social media:

[Twitter](#twitter-usage) \| [YouTube](#youtube-usage) \|
[Reddit](#reddit-usage) \| [Hyperlink](#hyperlink-usage) \|
[Supplemental Functions](#supplemental-functions)

### General Options

- `verbose`: most `vosonSML` functions accept a verbosity parameter that
  is now set to `FALSE` by default. When `FALSE` functions will run
  silently unless there is an error. If set to `TRUE` then progress and
  summary information for the function will be printed to the console.

The following environment options can also be used:

- `voson.msg`: If set to `FALSE` then the verbose output of functions
  will be printed using the base `cat()` function instead of the
  `message()` function. Set by entering `options(voson.msg = FALSE)`,
  and clear by assigning a value of `NULL`.
- `voson.data`: If set to an existing directory path the `writeToFile`
  output files will be written to that directory instead of the working
  directory. Can be set using `options(voson.data = "~/vsml-data")` for
  example, and cleared by assigning a value of `NULL`. Directory paths
  can be relative to the working directory or full paths.

### Authentication

Authentication objects generally *only need to be created once* unless
your credentials change. Save `twitter` and `youtube` authentication
objects to file after creation and then load them in future sessions.

*Please note in the examples provided that the “\~” notation in paths
are short-hand for the system to use the users home directory, and the
“.” at the start of file names classifies it as a hidden file on some
OS. You can name and save objects however you wish.*

``` r
# youtube data api key
auth_yt <- Authenticate("youtube", apiKey = "xxxxxxxxxx")

# save the object after Authenticate
saveRDS(auth_yt, file = "~/.auth_yt")

# load a previously saved authentication object for use in Collect
auth_yt <- readRDS("~/.auth_yt")
```

### Twitter Usage

Please note that `vosonSML` only accesses the Twitter `v1.1` API via
[rtweet](https://github.com/ropensci/rtweet) and does not support the
newer `v2` API at this time. Please refer to the VOSON Lab
[voson.tcn](https://github.com/vosonlab/voson.tcn) package if you are
interested in using the `v2` API to collect and analyse Twitter
conversation networks. `voson.tcn` has features to collect tweets and
conversation threads by url or id, and similarly to `vosonSML` produces
`activity` and `actor` networks with additional metadata.

`The Twitter features of this version of vosonSML requires rtweet v1.0 or later.`

``` r
packageVersion("rtweet")
## [1] '1.1.0'
```

#### Authenticate with the Twitter API

`Authenticate` is used to create an object that contains a Twitter token
for accessing the Twitter API. This can and should be re-used by saving
it once to file after calling `Authenticate` and then by loading it
again during future sessions.

``` r
library(vosonSML)

# twitter authentication creates an access token as part of the auth object
auth_tw_bearer <- Authenticate("twitter", bearerToken = "xxxxxxxxxxxx")

# save the object to file after authenticate
saveRDS(auth_tw_bearer, file = "~/.auth_tw_bearer")
```

``` r
# load a previously saved auth object for use in collect
auth_tw_bearer <- readRDS("~/.auth_tw_bearer")
```

#### Collect tweets using a search or from user timelines

`Collect` can be used to perform a twitter search with a search term or
collect tweets from timelines using user names. The following example
collects 100 `recent` tweets for the hashtag `#auspol` and creates a
dataframe with the collected tweet data.

``` r
# set output data directory
options(voson.data = "./vsml-data")

# collect 100 recent tweets for the hashtag #auspol
collect_tw <- auth_tw_bearer |>
  Collect(searchTerm = "#auspol",
          searchType = "recent",
          numTweets = 100,
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE)
## Collecting tweets for search query...
## Search term: #auspol
## Requested 100 tweets of 44900 in this search rate limit.
## Rate limit reset: 2023-03-13 15:10:53
## 
## tweet        | status_id           | created            
## --------------------------------------------------------
## Latest Obs   | 1635295414751727618 | 2023-03-13 15:03:03
## Earliest Obs | 1635286747411275779 | 2023-03-13 14:28:37
## Collected 100 tweets.
## RDS file written: ./vsml-data/2023-03-13_150313-TwitterData.rds
## Done.
```

The next example collects the 100 most recent tweets from the
`@vosonlab` and `@ANU_SOC` user timelines. Note that this method
requires the `endpoint = "timeline"` parameter.

``` r
# collect 100 timeline tweets for each specified user
collect_tw_tl <- auth_tw_bearer |>
  Collect(endpoint = "timeline",
          users = c("vosonlab", "ANU_SOCY"),
          numTweets = 100,
          writeToFile = TRUE,
          verbose = TRUE)
## Collecting timeline tweets for users...
## Requested 200 tweets of 149800 in this search rate limit.
## Rate limit reset: 2023-03-13 15:10:56
## 
## tweet        | status_id           | created            
## --------------------------------------------------------
## Latest Obs   | 1628912713949925376 | 2023-02-24 00:20:29
## Earliest Obs | 1434727454787051521 | 2021-09-06 03:57:35
## Collected 200 tweets.
## RDS file written: ./vsml-data/2023-03-13_150317-TwitterData.rds
## Done.
```

The output for these methods also lists the earliest and most recent
tweet as well as the number of tweets collected.

##### Importing tweets from rtweet

Because `vosonSML` uses the `rtweet` package to access and collect
tweets data, `rtweet` data is also able to be easily imported from
dataframe or file and then transformed into a `Collect` object for
further use.

``` r
tw_auth_rt <- readRDS("~/.rtweet_oauth1a")

tweets <- rtweet::search_tweets("#auspol", n = 20, token = tw_auth_rt)
data_tw <- ImportRtweet(tweets)

names(data_tw)
## [1] "tweets" "users"
class(data_tw)
## [1] "datasource" "twitter"    "list"
```

#### Create twitter activity, actor, semantic and 2-mode network graphs

The twitter `Create` function accepts the data from `Collect` and a type
parameter of `activity`, `actor`, `semantic` or `twomode` that specifies
the type of network to create from the collected data. `Create` produces
two dataframes, one for network `nodes` and one for node relations or
`edges` in the network. These can then undergo further processing as per
the [supplemental functions](#supplemental-functions) section or be
passed to the `Graph` function that creates an `igraph` object.

##### Activity network

Nodes are tweets and edges are the relationship to other tweets such as
`reply`, `retweet` or `quote` tweets.

``` r
net_activity <- collect_tw |> Create("activity", verbose = TRUE)
## Generating twitter activity network...
## -------------------------
## collected tweets | 100
## tweet            | 14
## retweet          | 80
## reply            | 4 
## quote            | 2 
## nodes            | 158
## edges            | 100
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-03-14_020318-TwitterActivity.graphml
## Done.

g_activity
## IGRAPH 3000ffa DN-- 158 100 -- 
## + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name
## | (v/c), created_at (v/c), user_id (e/c), screen_name (e/c), created_at
## | (e/c), edge_type (e/c)
## + edges from 3000ffa (vertex names):
##  [1] 1635295414751727618->1634849125903769601
##  [2] 1635295397911621633->1635295214989623302
##  [3] 1635295363912568834->1635229552657190912
##  [4] 1635295305494310918->1635287222441349124
##  [5] 1635295287630766081->1635229552657190912
##  [6] 1635295283298078721->1635295283298078721
## + ... omitted several edges
```

##### Actor network

Nodes are twitter users and edges are the relationship to other users in
the network such as `reply`, `mention`, `retweet` and `quote` tweets.
Mentions can be excluded by setting the parameter `inclMentions` to
`FALSE`.

``` r
net_actor <- collect_tw |>
  Create("actor", inclMentions = TRUE, verbose = TRUE)
## Generating twitter actor network...
## -------------------------
## collected tweets | 100
## tweet mention    | 6 
## tweet            | 14
## retweet          | 80
## reply mention    | 4 
## reply            | 4 
## quote mention    | 2 
## quote            | 2 
## nodes            | 128
## edges            | 112
## -------------------------
## Done.
```

``` r
g_actor <- net_actor |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-03-14_020318-TwitterActor.graphml
## Done.

g_actor
## IGRAPH 306a4b4 DN-- 128 112 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), status_id (e/c),
## | created_at (e/c), edge_type (e/c)
## + edges from 306a4b4 (vertex names):
##  [1] 15322814           ->322866759          
##  [2] 47353721           ->47353721           
##  [3] 1125425563076710401->1238370241169616897
##  [4] 862498438650380288 ->1573800344         
##  [5] 1470184751017250819->1238370241169616897
##  [6] 481997386          ->481997386          
##  [7] 47353721           ->1173213967         
## + ... omitted several edges
```

##### Semantic network

Nodes are concepts represented as common `words` and `hashtags`. Edges
represent the occurence of a particular word and a particular hashtag in
the same tweet. The semantic network is `undirected`.

``` r
# install additional required packages
# install.packages("stopwords")

# create a semantic network excluding the hashtag #auspol, include only the
# top 10% most frequent words and 20% most frequent hashtags as nodes
net_semantic <- collect_tw |>
  Create(
    "semantic",
    removeTermsOrHashtags = c("#auspol"),
    termFreq = 10,
    hashtagFreq = 20,
    verbose = TRUE
  )
## Generating twitter semantic network...
## Removing terms and hashtags: #auspol
## -------------------------
## retweets                 | 80
## tokens                   | 546
## removed specified        | 20
## removed users            | 14
## hashtag count            | 26
## hashtags unique          | 19
## term count               | 240
## terms unique             | 204
## top 20% hashtags n (>=3) | 3 
## top 10% terms n (>=2)    | 28
## nodes                    | 11
## edges                    | 11
## -------------------------
## Done.
```

``` r
g_semantic <- net_semantic |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-03-14_020319-TwitterSemantic.graphml
## Done.

g_semantic
## IGRAPH 30a4a33 UN-B 11 11 -- 
## + attr: type (g/c), name (v/c), type (v/c), n (v/n), from.type (e/c),
## | to.type (e/c), status_id (e/c)
## + edges from 30a4a33 (vertex names):
##  [1] aukus --#aukus     things--#aukus     #aukus--defence    #aukus--australia 
##  [5] #aukus--submarines #aukus--war        #aukus--coast      #aukus--cusp      
##  [9] #aukus--australia  #aukus--deal       #aukus--government
```

##### 2-mode network

Nodes are twitter `users` or `hashtags`. Edges represent the use of a
`hashtag` or the reference to another `user` in a tweet. The `weighted`
parameter will add a simple frequency weight column for `edges`.

``` r
net_2mode <- collect_tw |>
  Create("twomode", 
         removeTermsOrHashtags = c("#auspol"),
         weighted = TRUE,
         verbose = TRUE)
## Generating twitter 2-mode network...
## Removing terms and hashtags: #auspol
## -------------------------
## collected tweets  | 100
## removed specified | 20
## users             | 14
## hashtags          | 26
## nodes             | 44
## edges             | 40
## -------------------------
## Done.
```

``` r
g_2mode <- net_2mode |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-03-14_020319-Twitter2mode.graphml
## Done.

mask(g_2mode)
## IGRAPH 30cf1ff DNWB 44 40 -- 
## + attr: type (g/c), name (v/c), type (v/c), user_id (v/c), screen_name
## | (v/c), status_id (e/c), created_at (e/c), is_retweet (e/l), is_quote
## | (e/l), is_reply (e/l), weight (e/n)
## + edges from 30cf1ff (vertex names):
##  [1] @sxxxxxxxxoc ->#australia          @sxxxxxxxxoc ->#australianpolitics
##  [3] @txxxrr      ->@gxxxxxxds          @txxxrr      ->@pxxxbl            
##  [5] @txxxrr      ->@sxxxxxxxxna        @txxxrr      ->@axxxxxxxxxxxnv    
##  [7] @txxxrr      ->@sxxtu              @txxxrr      ->#freeassange       
##  [9] @1xxxxxxxxxu9->#protectjulian      @1xxxxxxxxxu9->#unity4j           
## [11] @1xxxxxxxxxu9->#freeassange        @1xxxxxxxxxu9->#ecuador           
## + ... omitted several edges
```

### YouTube Usage

#### Authenticate and Collect comments from youtube videos

YouTube uses an API key rather than an OAuth token and is simply set by
calling `Authenticate` with the key as a parameter.

``` r
# youtube authentication sets the api key
auth_yt <- Authenticate("youtube", apiKey = "xxxxxxxxxxxxxx")
```

Once the key is set then `Collect` can be used to collect the comments
from specified youtube videos. The following example collects a maximum
of 100 top-level comments and all replies from each of the 2 specified
video ID’s. It produces a dataframe with the combined comment data.

``` r
video_url <- c("https://www.youtube.com/watch?v=AQzZNIyjyWM",
               "https://www.youtube.com/watch?v=lY0YLDZhT88&t=3152s")

collect_yt <- auth_yt |>
  Collect(videoIDs = video_url,
          maxComments = 100,
          verbose = TRUE)
## Collecting comment threads for YouTube videos...
## Video 1 of 2
## ---------------------------------------------------------------
## ** Creating dataframe from threads of AQzZNIyjyWM.
## ** Collecting replies for 1 threads with replies. Please be patient.
## Comment replies 1 
## ** Collected replies: 1
## ** Total video comments: 11
## (Video API unit cost: 5)
## ---------------------------------------------------------------
## Video 2 of 2
## ---------------------------------------------------------------
## ** Creating dataframe from threads of lY0YLDZhT88.
## ** Collecting replies for 1 threads with replies. Please be patient.
## Comment replies 6 
## ** Collected replies: 6
## ** Total video comments: 14
## (Video API unit cost: 5)
## ---------------------------------------------------------------
## ** Total comments collected for all videos 25.
## (Estimated API unit cost: 10)
## Done.
```

#### Create youtube activity and actor network graphs

The youtube `Create` function accepts the data from `Collect` and a
network type parameter of `activity` or `actor`.

##### Activity network

Nodes are video comments and edges represent whether they were directed
to the video as a top-level comment or to another comment as a reply
comment.

``` r
net_activity <- collect_yt |> Create("activity", verbose = TRUE)
## Generating youtube activity network...
## -------------------------
## collected YouTube comments | 25
## top-level comments         | 18
## reply comments             | 7
## videos                     | 2
## nodes                      | 27
## edges                      | 25
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph()

g_activity
## IGRAPH 3174547 DN-- 27 25 -- 
## + attr: type (g/c), name (v/c), video_id (v/c), published_at (v/c),
## | updated_at (v/c), author_id (v/c), screen_name (v/c), node_type
## | (v/c), edge_type (e/c)
## + edges from 3174547 (vertex names):
## [1] Ugw13lb0nCf4o4IKFb54AaABAg->VIDEOID:AQzZNIyjyWM
## [2] UgyJBlqZ64YnltQTOTt4AaABAg->VIDEOID:AQzZNIyjyWM
## [3] Ugysomx_apk24Pqrs1h4AaABAg->VIDEOID:AQzZNIyjyWM
## [4] UgxTjkzuvY2BOKUThT14AaABAg->VIDEOID:AQzZNIyjyWM
## [5] Ugx7yyBFwvDBe8hGexB4AaABAg->VIDEOID:AQzZNIyjyWM
## [6] UgxDjVTbpt6BCRw4Lqx4AaABAg->VIDEOID:AQzZNIyjyWM
## + ... omitted several edges
```

##### Actor network

Nodes are users who have posted comments and the video publishers, edges
represent comments directed at other users.

``` r
net_actor <- collect_yt |> Create("actor", verbose = TRUE)
## Generating YouTube actor network...
## Done.
```

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 3183404 DN-- 24 27 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
## | video_id (e/c), comment_id (e/c), edge_type (e/c)
## + edges from 3183404 (vertex names):
##  [1] UCb9ElH9tzEkG9OxDIiSYgdg->VIDEOID:AQzZNIyjyWM
##  [2] UC0DwaB_wHNzUh-LA9sWXKYQ->VIDEOID:AQzZNIyjyWM
##  [3] UCNHA8SkizJKauefYt1FHmjQ->VIDEOID:AQzZNIyjyWM
##  [4] UCmFYrmqK7zO51STyk1jBSTw->VIDEOID:AQzZNIyjyWM
##  [5] UC4Wa_1O2w4Wf8MhrIdYFZCQ->VIDEOID:AQzZNIyjyWM
##  [6] UCGwMcYKT2hmT3MEy4Bgfpiw->VIDEOID:AQzZNIyjyWM
##  [7] UCW_9UuD91Ult0wwyn2Mnb_w->VIDEOID:AQzZNIyjyWM
## + ... omitted several edges
```

### Reddit Usage

The reddit API end-point used by `vosonSML` does not require
authentication but an `Authenticate` object is still used to set up the
collection and creation operations as part of a reddit workflow.

#### Collect a thread listing from subreddit

By using the `endpoint = "listing"` parameter and a vector of subreddit
names, a list of comment threads and their metadata can be collected.
The number of list results returned per subreddit can be coarsely
specified within 25 items, by using the `max` parameter.

``` r
# specify subreddit names
subreddits <- c("datascience")

# collect a listing of the 25 top threads of all time
collect_rd_listing <- Authenticate("reddit") |>
  Collect(endpoint = "listing", subreddits = subreddits,
          sort = "top", top = "all", max = 25,
          writeToFile = TRUE, verbose = TRUE)
## Collecting thread listing for subreddits...
## Waiting between 3 and 5 seconds per request.
## Request subreddit listing: datascience (max items: 25).
## subreddit_id | subreddit   | count
## ----------------------------------
## t5_2sptq     | datascience | 25   
## Collected metadata for 25 threads in listings.
## RDS file written: ./vsml-data/2023-03-13_150322-RedditListing.rds
## Done.
```

#### Collect reddit threads

The reddit `Collect` function can then be used to collect comments from
reddit threads specified by URL’s.

``` r
# specify reddit threads to collect by url
thread_urls <- c(
  "https://www.reddit.com/r/datascience/comments/wcd8x5/",
  "https://www.reddit.com/r/datascience/comments/wcni2g/"
)

# or use permalinks from a previously collected listing
thread_urls <- collect_rd_listing$permalink |> head(n = 3)

# collect 2 comment threads with comments sorted by controversiality
collect_rd <- Authenticate("reddit") |>
  Collect(threadUrls = thread_urls,
          sort = "controversial", writeToFile = TRUE, verbose = TRUE)
## Collecting comment threads for reddit urls...
## Waiting between 3 and 5 seconds per thread request.
## Request thread: r/datascience (k8nyf8)
## Request thread: r/datascience (oeg6nl)
## Request thread: r/datascience (hohvgq)
## HTML decoding comments.
## thread_id | title                                         | subreddit   | count
## -------------------------------------------------------------------------------
## hohvgq    | Shout Out to All the Mediocre Data Scienti... | datascience | 272  
## k8nyf8    | data siens                                    | datascience | 77   
## oeg6nl    | The pain and excitement                       | datascience | 179
## Collected 528 total comments.
## RDS file written: ./vsml-data/2023-03-13_150335-RedditData.rds
## Done.
```

Please note that because of the API end-point used that `Collect` is
limited to the first 500 comments per thread. It is therefore suited to
collecting only smaller threads in their entirety.

#### Create reddit activity and actor networks

##### Activity network

Nodes are original thread posts and comments, edges are replies directed
to the original post and to comments made by others.

``` r
# create an activity network
net_activity <- collect_rd |> Create("activity", verbose = TRUE)
## Generating reddit activity network...
## -------------------------
## collected reddit comments | 528
## subreddits                | 1 
## threads                   | 3 
## comments                  | 528
## nodes                     | 531
## edges                     | 528
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph()

g_activity
## IGRAPH 3aad52c DN-- 531 528 -- 
## + attr: type (g/c), name (v/c), thread_id (v/c), comm_id (v/c),
## | datetime (v/c), ts (v/n), subreddit (v/c), user (v/c), node_type
## | (v/c), edge_type (e/c)
## + edges from 3aad52c (vertex names):
##  [1] k8nyf8.1        ->k8nyf8.0       k8nyf8.1_1      ->k8nyf8.1      
##  [3] k8nyf8.1_1_1    ->k8nyf8.1_1     k8nyf8.2        ->k8nyf8.0      
##  [5] k8nyf8.3        ->k8nyf8.0       k8nyf8.4        ->k8nyf8.0      
##  [7] k8nyf8.5        ->k8nyf8.0       k8nyf8.5_1      ->k8nyf8.5      
##  [9] k8nyf8.5_1_1    ->k8nyf8.5_1     k8nyf8.5_1_2    ->k8nyf8.5_1    
## [11] k8nyf8.5_1_2_1  ->k8nyf8.5_1_2   k8nyf8.5_1_2_1_1->k8nyf8.5_1_2_1
## + ... omitted several edges
```

##### Actor network

Nodes are reddit users who have commented on threads and edges represent
replies to other users.

``` r
# create an actor network
net_actor <- collect_rd |> Create("actor", verbose = TRUE)
## Generating reddit actor network...
## -------------------------
## collected reddit comments | 528
## subreddits                | 1 
## threads                   | 3 
## comments                  | 273
## nodes                     | 321
## edges                     | 531
## -------------------------
## Done.
```

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 3ac01ea DN-- 321 531 -- 
## + attr: type (g/c), name (v/c), user (v/c), subreddit (e/c), thread_id
## | (e/c), comment_id (e/n), comm_id (e/c)
## + edges from 3ac01ea (vertex names):
##  [1] 1 ->6  2 ->1  1 ->2  3 ->6  4 ->6  5 ->6  6 ->6  7 ->6  6 ->7  8 ->7 
## [11] 7 ->8  8 ->7  9 ->6  10->9  11->9  12->6  13->12 14->12 15->12 12->15
## [21] 16->12 17->6  6 ->17 18->17 19->6  20->19 21->19 22->21 23->22 24->23
## [31] 6 ->24 25->6  24->6  6 ->24 24->6  22->23 26->21 27->6  6 ->27 6 ->6 
## [41] 6 ->6  6 ->6  6 ->6  6 ->6  28->6  29->27 30->6  6 ->30 31->6  32->6 
## [51] 33->6  34->6  6 ->6  35->6  36->35 37->6  6 ->37 37->6  38->6  39->6 
## [61] 40->37 41->6  42->6  43->42 44->6  45->6  43->45 46->6  43->6  47->6 
## + ... omitted several edges
```

### Hyperlink Usage

#### Authenticate and Collect from web sites

The `vosonSML` hyperlink collection functionality does not require
authentication as it is not using any web API’s, however an
`Authenticate` object is still used to set up the collection and
creation operations as part of the `vosonSML` workflow.

The hyperlink `Collect` function accepts a dataframe of seed web pages,
as well as corresponding `type` and `max_depth` parameters for each
page.

*Please note that this implementalion of hyperlink collection and
networks is still in an experimental stage.*

``` r
# specify seed web pages and parameters for hyperlink collection
seed_pages <-
  data.frame(page = c("http://vosonlab.net",
                      "https://www.oii.ox.ac.uk",
                      "https://sonic.northwestern.edu"),
             type = c("ext", "ext", "ext"),
             max_depth = c(2, 2, 2))

collect_web <- Authenticate("web") |>
  Collect(pages = seed_pages, verbose = TRUE)

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
net_activity <- collect_web |> Create("activity")

# generate a hyperlink actor network
net_actor <- collect_web |> Create("actor")
```

### <a name="supplemental-functions"></a>Supplemental Functions

#### Merge collected data together

The `Merge` and `MergeFiles` functions allow two or more `Collect`
objects to be merged together provided they are of the same datasource
type e.g `twitter`, `youtube`.

``` r
# collect data
collect_tw_auspol <- auth_tw_bearer |>
  Collect(searchTerm = "#auspol", writeToFile = TRUE)
  
collect_tw_springst <- auth_tw_bearer |>
  Collect(searchTerm = "#springst", writeToFile = TRUE)

# merge collect objects
data_tw <- Merge(
  collect_tw_auspol, collect_tw_springst, writeToFile = TRUE, verbose = TRUE
)

# merge files from a data directory
data_tw <- MergeFiles(
  "vsml-tw-data", pattern = "*TwitterData.rds", writeToFile = TRUE, verbose = TRUE
)
```

#### AddText adds collected text data to networks as node or edge attributes

The `AddText` function can be used following the creation of all
networks for `twitter`, `youtube` and `reddit`. It will add an attribute
starting with `vosonTxt_` to nodes of `activity` networks and to edges
of `actor` networks. It requires a collected `datasource` from which to
extract text data.

An additional parameter `hashtags` is available for `twitter` networks
that will add tweet hashtags as an attribute.

``` r
# create activity network
net_activity <- collect_tw |> Create("activity")

# activity network with text data added as node attribute
net_activity <- net_activity |>
  AddText(collect_tw, hashtags = TRUE, verbose = TRUE)
## Adding text data to network...Done.
```

``` r
g_activity <- net_activity |> Graph()

g_activity
## IGRAPH 3b235c3 DN-- 158 100 -- 
## + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name
## | (v/c), created_at (v/c), t.is_reply (v/l), t.is_quote (v/l),
## | t.is_retweet (v/l), t.full_text (v/c), t.hashtags (v/x),
## | t.quoted.status_id (v/c), t.quoted.full_text (v/c), t.quoted.hashtags
## | (v/x), t.retweeted.status_id (v/c), t.retweeted.full_text (v/c),
## | t.retweeted.hashtags (v/x), vosonTxt_tweet (v/c), vosonTxt_hashtags
## | (v/c), user_id (e/c), screen_name (e/c), created_at (e/c), edge_type
## | (e/c)
## + edges from 3b235c3 (vertex names):
## [1] 1635295414751727618->1634849125903769601
## + ... omitted several edges
```

`AddText` will also redirect some edges in a youtube `actor` network by
finding user references at the beginning of reply comments text using
the `repliesFromText` parameter. In the following example an edge would
be redirected from `UserC` to `UserB` by text reference as opposed to
`UserA` who made the top-level comment both users are replying to.

``` r
# video comments
# UserA: Great tutorial.
# |- UserB: I agree, but it could have had more examples.
# |- UserC: @UserB I thought it probably had too many.
```

Redirect edge between user nodes `C -> A` to `C -> B`.

``` r
# create activity network
net_actor <- collect_yt |> Create("actor")

# detects replies to users in text
net_actor <- net_actor |>
  AddText(collect_yt,
          repliesFromText = TRUE,
          verbose = TRUE)
## Adding text data to network...Done.
```

#### AddUserData requests and adds user profile data to networks

`AddUserData` adds user profile information from the `users` dataframe
to as many users in a twitter `actor` and `2mode` network as possible.
If the profile information is not available for referenced users in the
collect data then the user id and name will be added to the
`missing_users` dataframe. If the profile metadata is not available in
the collect data and the `lookupUsers` parameter is set then additional
twitter API requests will be made to retrieve the missing information.

``` r
# add additional twitter user profile info
net_actor <- collect_tw |> Create("actor")

net_actor_meta <- net_actor |> AddUserData(collect_tw, verbose = TRUE)
## Adding user data to network...Done.

names(net_actor_meta)
## [1] "edges"         "nodes"         "missing_users"
nrow(net_actor_meta$missing_users)
## [1] 13

# add additional twitter user profile info
net_actor_lookupmeta <- net_actor |>
  AddUserData(collect_tw,
              lookupUsers = TRUE,
              twitterAuth = auth_tw_bearer,
              verbose = TRUE)
## Adding user data to network...Done.

names(net_actor_lookupmeta)
## [1] "edges"         "nodes"         "missing_users" "lookup_users"
```

For reference the `AddUserData` function will also add a new dataframe
to the `actor_network` network list containing the retrieved user
metadata.

``` r
g_actor <- net_actor_meta |> Graph()

g_actor
## IGRAPH 3bfda43 DN-- 128 112 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), u.user_id (v/c),
## | u.name (v/c), u.screen_name (v/c), u.location (v/c), u.derived (v/x),
## | u.url (v/c), u.description (v/c), u.protected (v/l), u.verified
## | (v/l), u.followers_count (v/n), u.friends_count (v/n), u.listed_count
## | (v/n), u.favourites_count (v/n), u.statuses_count (v/n), u.created_at
## | (v/c), u.profile_banner_url (v/c), u.default_profile (v/l),
## | u.default_profile_image (v/l), u.withheld_in_countries (v/x),
## | u.withheld_scope (v/l), u.utc_offset (v/l), u.time_zone (v/l),
## | u.geo_enabled (v/l), u.lang (v/l), u.has_extended_profile (v/l),
## | status_id (e/c), created_at (e/c), edge_type (e/c)
## + edges from 3bfda43 (vertex names):
```

#### AddVideoData requests and adds video data to networks

`AddVideoData` adds video information as node attributes in youtube
`actor` networks and replaces the video ID nodes with a user (channel
owner or publisher). The `actorSubOnly` parameter can be used to only
perform the ID substitution.

``` r
# replaces VIDEOID:xxxxxx references in actor network with their publishers
# user id (channel ID) and adds additional collected youtube video info to actor
# network graph as node attributes
net_actor <- collect_yt |>
  Create("actor") |> 
  AddVideoData(auth_yt, actorSubOnly = FALSE)

names(net_actor)
## [1] "nodes"  "edges"  "videos"
nrow(net_actor$videos)
## [1] 2
```

`AddVideoData` function will also add a new dataframe to the
`actor_network` network list containing the retrieved video information
called `videos`.

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 3c27ecc DN-- 23 27 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
## | video_id (e/c), comment_id (e/c), edge_type (e/c), video_title (e/c),
## | video_description (e/c), video_published_at (e/c)
## + edges from 3c27ecc (vertex names):
## [1] UCb9ElH9tzEkG9OxDIiSYgdg->UCeiiqmVK07qhY-wvg3IZiZQ
## [2] UC0DwaB_wHNzUh-LA9sWXKYQ->UCeiiqmVK07qhY-wvg3IZiZQ
## [3] UCNHA8SkizJKauefYt1FHmjQ->UCeiiqmVK07qhY-wvg3IZiZQ
## [4] UCmFYrmqK7zO51STyk1jBSTw->UCeiiqmVK07qhY-wvg3IZiZQ
## [5] UC4Wa_1O2w4Wf8MhrIdYFZCQ->UCeiiqmVK07qhY-wvg3IZiZQ
## [6] UCGwMcYKT2hmT3MEy4Bgfpiw->UCeiiqmVK07qhY-wvg3IZiZQ
## + ... omitted several edges
```

## Where to next?

Continue working with the network graphs using the `igraph` package and
check out some examples of plots in the [Introduction to
vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html)
vignette. The `graphml` files produced by `vosonSML` are also easily
imported into software such as [Gephi](https://gephi.org/) for further
visualization and exploration of networks.

As an alternative to `vosonSML` using the R command-line interface we
have also developed an R Shiny app called [VOSON
Dash](https://vosonlab.github.io/VOSONDash/). It provides a user
friendly GUI for the collection of data using `vosonSML` and has
additional network visualization and analysis features.

For more detailed information about functions and their parameters,
please refer to the
[Reference](https://vosonlab.github.io/vosonSML/reference/index.html)
page.

## Special thanks

This package would not be possible without key packages by other authors
in the R community, particularly:
[data.table](https://github.com/Rdatatable/data.table),
[dplyr](https://github.com/tidyverse/dplyr),
[httr](https://github.com/r-lib/httr),
[igraph](https://github.com/igraph/rigraph),
[RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR),
[rtweet](https://github.com/ropensci/rtweet) and
[tidytext](https://github.com/juliasilge/tidytext).

## Code of Conduct

Please note that the VOSON Lab projects are released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
