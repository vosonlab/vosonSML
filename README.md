
# vosonSML - Social Media Lab<img src="https://vosonlab.github.io/vosonSML/images/logo.png" width="140px" align="right" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Monthly](https://cranlogs.r-pkg.org/badges/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Github_Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github)](https://github.com/vosonlab/vosonSML/releases)
[![Github_Dev](https://img.shields.io/static/v1?label=dev&message=v0.32.5&logo=github)](https://github.com/vosonlab/vosonSML)
[![Last_Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?&logo=github)](https://github.com/vosonlab/vosonSML/commits/master)
[![Build_Status](https://github.com/vosonlab/vosonSML/workflows/R-CMD-check/badge.svg)](https://github.com/vosonlab/vosonSML/actions)

The `vosonSML` R package is a suite of easy to use functions for
collecting and generating different types of networks from social media
data. The package supports the collection of data from `twitter`,
`youtube` and `reddit`, as well as `hyperlinks` from web sites. Networks
in the form of node and edge lists can be generated from collected data,
supplemented with additional metadata and used to create graphs for
Social Network Analysis.

## Installation Options

Install the most recent CRAN release:

``` r
install.packages("vosonSML")
```

Install the most recent release tag via GitHub:

``` r
install.packages(
  "https://github.com/vosonlab/vosonSML/releases/download/v0.30.6/vosonSML-0.30.6.tar.gz",
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

### Authentication

Authentication objects generally *only need to be created once* unless
your credentials change. Save `twitter` and `youtube` authentication
objects to file after creation and then load them in future sessions.

*Please note in the examples provided that the “\~” notation in paths
are short-hand for the system to use the users home directory, and the
“.” at the start of file names classifies it as a hidden file on some
OS. You can name and save objects however you wish.*

``` r
# save the object after Authenticate
saveRDS(auth_yt, file = "~/.auth_yt")

# load a previously saved authentication object for use in Collect
auth_yt <- readRDS("~/.auth_yt")
```

### Twitter Usage

Please note that `vosonSML` only uses the `Twitter v1.0a API` through
[rtweet](https://github.com/ropensci/rtweet) and does not support the
`v2 API` at this stage. The VOSON Lab
[voson.tcn](https://github.com/vosonlab/voson.tcn) package for Twitter
Conversation Networks does use the `v2 API` but is limited to collecting
conversation threads and tweets by URL or ID.

#### Authenticate with the Twitter API

`Authenticate` is used to create an object that contains a Twitter OAuth
1.0a user token for accessing the twitter API. This can and should be
re-used by saving it once to file after calling `Authenticate` and then
by loading it again during future sessions.

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
# collect 100 recent tweets for the hashtag #auspol
collect_tw <- auth_tw_bearer |>
  Collect(searchTerm = "#auspol",
          searchType = "recent",
          numTweets = 100,
          includeRetweets = TRUE,
          retryOnRateLimit = TRUE,
          writeToFile = FALSE,
          verbose = TRUE)
## Collecting tweets for search query...
## Search term: #auspol
## Requested 100 tweets of 44900 in this search rate limit.
## Rate limit reset: 2022-08-01 12:42:22
## 
## tweet        | status_id           | created            
## --------------------------------------------------------
## Latest Obs   | 1554084885895454721 | 2022-08-01 12:41:24
## Earliest Obs | 1554083670797201410 | 2022-08-01 12:36:35
## Collected 100 tweets.
## Done.
```

The next example collects the 100 most recents tweets from the
`@vosonlab` twitter account. Note that this method requires the
`endpoint = "timeline"` parameter.

``` r
# collect 100 timeline tweets for @vosonlab
collect_tw_tl <- auth_tw_bearer |>
  Collect(endpoint = "timeline",
          users = "vosonlab",
          numTweets = 40,
          verbose = TRUE)
## Collecting timeline tweets for users...
## Requested 40 tweets of 149900 in this search rate limit.
## Rate limit reset: 2022-08-01 12:42:24
## 
## tweet        | status_id           | created            
## --------------------------------------------------------
## Latest Obs   | 1548485369062395904 | 2022-07-17 01:50:56
## Earliest Obs | 1524850528089698304 | 2022-05-12 20:34:30
## Collected 40 tweets.
## Done.
```

The output for these methods also lists the 2 earliest and 2 latest
tweets collected as well as the number of tweets collected.

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
## tweet            | 3 
## retweet          | 88
## reply            | 4 
## quote            | 5 
## nodes            | 174
## edges            | 100
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: 2022-08-01_224128-TwitterActivity.graphml
## Done.

g_activity
## IGRAPH 4368c38 DN-- 174 100 -- 
## + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name
## | (v/c), created_at (v/c), user_id (e/c), screen_name (e/c), created_at
## | (e/c), edge_type (e/c)
## + edges from 4368c38 (vertex names):
##  [1] 1554084885895454721->1554062973840543744
##  [2] 1554084872549199872->1554015506327375873
##  [3] 1554084864491606016->1554083220937125888
##  [4] 1554084856597864448->1554077132904710146
##  [5] 1554084835248857088->1553657721102090241
##  [6] 1554084823659995136->1553610501845749760
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
## tweet mention    | 1 
## tweet            | 3 
## retweet          | 88
## reply mention    | 3 
## reply            | 4 
## quote mention    | 1 
## quote            | 5 
## nodes            | 145
## edges            | 105
## -------------------------
## Done.
```

``` r
g_actor <- net_actor |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: 2022-08-01_224129-TwitterActor.graphml
## Done.

g_actor
## IGRAPH 439d472 DN-- 145 105 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), status_id (e/c),
## | created_at (e/c), edge_type (e/c)
## + edges from 439d472 (vertex names):
##  [1] 1462718884184616966->2262542771         
##  [2] 2671719728         ->198296897          
##  [3] 1077703279218282497->4168028659         
##  [4] 1251559552681558016->1416957225105494024
##  [5] 1103970815656132608->1535567278661459969
##  [6] 85233737           ->1132832803         
##  [7] 1520365271592038409->2615581908         
## + ... omitted several edges
```

##### Semantic network

Nodes are concepts represented as common `words` and `hashtags`. Edges
represent the occurence of a particular word and a particular hashtag in
the same tweet. The semantic network is `undirected`.

``` r
# install additional required packages
# install.packages(c("tidytext", "stopwords"))

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
## retweets                 | 88
## tokens                   | 339
## removed specified        | 12
## removed users            | 9 
## hashtag count            | 52
## hashtags unique          | 48
## term count               | 107
## terms unique             | 98
## top 20% hashtags n (>=1) | 48
## top 10% terms n (>=2)    | 9 
## nodes                    | 50
## edges                    | 120
## -------------------------
## Done.
```

``` r
g_semantic <- net_semantic |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: 2022-08-01_224130-TwitterSemantic.graphml
## Done.

g_semantic
## IGRAPH 44389d6 UN-B 50 120 -- 
## + attr: type (g/c), name (v/c), type (v/c), n (v/n), from.type (e/c),
## | to.type (e/c), status_id (e/c)
## + edges from 44389d6 (vertex names):
##  [1] commies    --#lnp              commies    --#auspol2022      
##  [3] commies    --#politics         commies    --#ausmedia        
##  [5] #parliament--rough             #parliament--boys             
##  [7] rough      --#wearamask        rough      --#getvaccinated   
##  [9] rough      --#keepyourdistance rough      --#sanitize        
## [11] rough      --#covid19          rough      --#covidhasnotgone 
## [13] rough      --#covid            boys       --#wearamask       
## + ... omitted several edges
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
## removed specified | 12
## users             | 9 
## hashtags          | 52
## nodes             | 69
## edges             | 61
## -------------------------
## Done.
```

``` r
g_2mode <- net_2mode |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: 2022-08-01_224130-Twitter2mode.graphml
## Done.

mask(g_2mode)
## IGRAPH 4455ba9 DNW- 69 61 -- 
## + attr: type (g/c), name (v/c), user_id (v/c), status_id (e/c),
## | created_at (e/c), is_retweet (e/l), is_quote (e/l), is_reply (e/l),
## | weight (e/n)
## + edges from 4455ba9 (vertex names):
##  [1] @axxxxxxxxxxxxet->#monkeypox        @exxxxxxnl      ->@cxxxxxxna       
##  [3] @exxxxxxnl      ->#lnp              @exxxxxxnl      ->#auspol2022      
##  [5] @exxxxxxnl      ->#politics         @exxxxxxnl      ->#ausmedia        
##  [7] @ixxxxxxxxxxxro ->#parliament       @ixxxxxxxxxxxro ->#wearamask       
##  [9] @ixxxxxxxxxxxro ->#getvaccinated    @ixxxxxxxxxxxro ->#keepyourdistance
## [11] @ixxxxxxxxxxxro ->#sanitize         @ixxxxxxxxxxxro ->#covid19         
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
## 
## ** Total comments collected for all videos 11.
## (Estimated API unit cost: 5)
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
## collected YouTube comments | 11
## top-level comments         | 10
## reply comments             | 1
## videos                     | 1
## nodes                      | 12
## edges                      | 11
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph()

g_activity
## IGRAPH 44d8981 DN-- 12 11 -- 
## + attr: type (g/c), name (v/c), video_id (v/c), published_at (v/c),
## | updated_at (v/c), author_id (v/c), screen_name (v/c), node_type
## | (v/c), edge_type (e/c)
## + edges from 44d8981 (vertex names):
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
## IGRAPH 44e286d DN-- 11 12 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
## | video_id (e/c), comment_id (e/c), edge_type (e/c)
## + edges from 44e286d (vertex names):
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

#### Authenticate and Collect from reddit threads

The reddit API end-point used by `vosonSML` does not require
authentication but an `Authenticate` object is still used to set up the
collection and creation operations as part of a reddit workflow. The
reddit `Collect` function can then be used to collect comments from
reddit threads specified by URL’s.

``` r
# specify reddit threads to collect by url
thread_url <-
  c("https://www.reddit.com/r/datascience/comments/wcd8x5/",
    "https://www.reddit.com/r/datascience/comments/wcni2g/")

# authentication does not require credentials
collect_rd <- Authenticate("reddit") |>
  Collect(threadUrls = thread_url, verbose = TRUE)
## Collecting comment threads for reddit urls...
## Waiting between 3 and 10 seconds per thread request.
## Request thread: r/datascience (wcd8x5)
## Request thread: r/datascience (wcni2g)
## HTML decoding comments.
## thread_id | title                                   | subreddit   | count
## -------------------------------------------------------------------------
## wcd8x5    | what is the name of the job I do?       | datascience | 63   
## wcni2g    | Ops research analyst vs data scientist. | datascience | 2
## Collected 65 total comments.
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
## collected reddit comments | 65
## threads                   | 2
## comments                  | 65
## nodes                     | 67
## edges                     | 65
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph()

g_activity
## IGRAPH 4a068ec DN-- 67 65 -- 
## + attr: type (g/c), name (v/c), thread_id (v/c), comm_id (v/c),
## | datetime (v/c), ts (v/n), subreddit (v/c), user (v/c), node_type
## | (v/c), edge_type (e/c)
## + edges from 4a068ec (vertex names):
##  [1] wcd8x5.1          ->wcd8x5.0         wcd8x5.2          ->wcd8x5.0        
##  [3] wcd8x5.2_1        ->wcd8x5.2         wcd8x5.2_2        ->wcd8x5.2        
##  [5] wcd8x5.2_2_1      ->wcd8x5.2_2       wcd8x5.2_2_1_1    ->wcd8x5.2_2_1    
##  [7] wcd8x5.2_2_1_1_1  ->wcd8x5.2_2_1_1   wcd8x5.2_2_1_1_1_1->wcd8x5.2_2_1_1_1
##  [9] wcd8x5.2_2_1_1_2  ->wcd8x5.2_2_1_1   wcd8x5.2_2_1_1_2_1->wcd8x5.2_2_1_1_2
## [11] wcd8x5.3          ->wcd8x5.0         wcd8x5.3_1        ->wcd8x5.3        
## + ... omitted several edges
```

##### Actor network

Nodes are reddit users who have commented on threads and edges represent
replies to other users.

``` r
# create an actor network
net_actor <- collect_rd |> Create("actor", verbose = TRUE)
## Generating reddit actor network...
## Done.
```

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 4a10b01 DN-- 35 67 -- 
## + attr: type (g/c), name (v/c), user (v/c), subreddit (e/c), thread_id
## | (e/c), comment_id (e/n), comm_id (e/c)
## + edges from 4a10b01 (vertex names):
##  [1] 1 ->7  2 ->7  3 ->2  4 ->2  2 ->4  4 ->2  5 ->4  4 ->5  1 ->4  4 ->1 
## [11] 6 ->7  7 ->6  8 ->7  9 ->8  7 ->9  9 ->7  7 ->8  10->7  11->10 7 ->11
## [21] 12->7  7 ->12 13->11 14->13 9 ->13 13->9  9 ->13 15->7  7 ->15 16->7 
## [31] 7 ->16 17->7  18->17 17->18 19->17 17->19 19->17 17->19 20->7  7 ->20
## [41] 21->7  22->7  23->7  24->7  7 ->24 18->7  7 ->18 25->7  26->7  18->7 
## [51] 9 ->7  7 ->9  9 ->7  7 ->9  9 ->7  27->7  28->7  29->7  7 ->29 29->7 
## [61] 30->7  31->7  32->7  33->35 34->33 7 ->7  35->35
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

#### AddText adds collected text data to networks as node or edge attributes

The `AddText` function can be used following the creation of all
networks for `twitter`, `youtube` and `reddit`. It will add an attribute
starting with `vosonTxt_` to nodes of `activity` networks and to edges
of `actor` networks. It requires a collected `datasource` from which to
extract text data.

``` r
# create activity network
net_activity <- collect_tw |> Create("activity")

# activity network with text data added as node attribute
net_activity <- net_activity |> AddText(collect_tw, verbose = TRUE)
## Adding text data to network...Done.
```

``` r
g_activity <- net_activity |> Graph()

g_activity
## IGRAPH 4a5188b DN-- 174 100 -- 
## + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name
## | (v/c), created_at (v/c), t.is_reply (v/l), t.is_quote (v/l),
## | t.is_retweet (v/l), t.full_text (v/c), t.quoted.full_text (v/c),
## | t.retweeted.full_text (v/c), vosonTxt_tweet (v/c), user_id (e/c),
## | screen_name (e/c), created_at (e/c), edge_type (e/c)
## + edges from 4a5188b (vertex names):
## [1] 1554084885895454721->1554062973840543744
## [2] 1554084872549199872->1554015506327375873
## [3] 1554084864491606016->1554083220937125888
## [4] 1554084856597864448->1554077132904710146
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
`missing_users` dataframe and if required can be collected in a seperate
operation.

``` r
# add additional twitter user profile info
net_actor <- collect_tw |>
  Create("actor") |>
  AddUserData(collect_tw, verbose = TRUE)
## Adding user data to network...Done.

names(net_actor)
## [1] "edges"         "nodes"         "missing_users"
nrow(net_actor$missing_users)
## [1] 85
```

For reference the `AddUserData` function will also add a new dataframe
to the `actor_network` network list containing the retrieved user
profiles called `users`.

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 4a9ab56 DN-- 145 105 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), u.id_str (v/c),
## | u.display_name (v/c), u.screen_name (v/c), u.location (v/c),
## | u.description (v/c), u.url (v/c), u.protected (v/l),
## | u.followers_count (v/n), u.friends_count (v/n), u.listed_count (v/n),
## | u.created_at (v/c), u.favourites_count (v/n), u.verified (v/l),
## | u.statuses_count (v/n), u.profile_banner_url (v/c), u.default_profile
## | (v/l), u.default_profile_image (v/l), u.withheld_in_countries (v/x),
## | u.derived (v/c), u.withheld_scope (v/l), u.utc_offset (v/l),
## | u.time_zone (v/l), u.geo_enabled (v/l), u.lang (v/l),
## | u.has_extended_profile (v/l), status_id (e/c), created_at (e/c),
## | edge_type (e/c)
## + edges from 4a9ab56 (vertex names):
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
## [1] 1
```

`AddVideoData` function will also add a new dataframe to the
`actor_network` network list containing the retrieved video information
called `videos`.

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 4abd149 DN-- 11 12 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
## | video_id (e/c), comment_id (e/c), edge_type (e/c), video_title (e/c),
## | video_description (e/c), video_published_at (e/c)
## + edges from 4abd149 (vertex names):
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
