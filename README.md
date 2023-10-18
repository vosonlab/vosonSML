# vosonSML - Social Media Lab<img src="https://vosonlab.github.io/vosonSML/images/logo.png" width="140px" align="right"/>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Monthly](https://cranlogs.r-pkg.org/badges/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![Github_Release](https://img.shields.io/github/release-pre/vosonlab/vosonSML.svg?logo=github)](https://github.com/vosonlab/vosonSML/releases)
[![Github_Dev](https://img.shields.io/static/v1?label=dev&message=v0.34&logo=github)](https://github.com/vosonlab/vosonSML)
[![Last_Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?&logo=github)](https://github.com/vosonlab/vosonSML/commits/master)
[![Build_Status](https://github.com/vosonlab/vosonSML/workflows/R-CMD-check/badge.svg)](https://github.com/vosonlab/vosonSML/actions)

The `vosonSML` R package is a suite of easy to use functions for collecting
and generating different types of networks from social media data. The package
supports the collection of data from the `mastodon`, `youtube`, `reddit`,
`twitter` platforms and `hyperlinks` from web sites. Networks in the form of
node and edge lists can be generated from collected data, supplemented with
additional metadata, and used to create graphs for Social Network Analysis.

## Installation Options

Install the most recent CRAN release:

``` r
install.packages("vosonSML")
```

Install the most recent release tag via GitHub:

``` r
install.packages(
  "https://github.com/vosonlab/vosonSML/releases/download/v0.34/vosonSML-0.34.tar.gz",
  repo = NULL, type = "source")
```

Install the latest development version:

``` r
# library(remotes)
remotes::install_github("vosonlab/vosonSML")
```

## Getting started

The following usage examples will provide a quick start to using `vosonSML`
functions. Additionally there is an [Introduction to
vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html)
vignette that is a practical and explanatory guide to collecting data and
creating networks.

### General Usage

The process of authentication, data collection and creating networks in
`vosonSML` is expressed with the three functions: *Authenticate*, *Collect*
and *Create*. The following are some examples of their usage for supported
social media:

[Mastodon](#mastodon-usage) \| [YouTube](#youtube-usage) \|
[Reddit](#reddit-usage) \| [Hyperlink](#hyperlink-usage) \|
[Twitter](#twitter-usage) \| [Supplemental Functions](#supplemental-functions)

### General Options

-   `verbose`: most `vosonSML` functions accept a verbosity parameter that is
    now set to `FALSE` by default. When `FALSE` functions will run silently
    unless there is an error. If set to `TRUE` then progress and summary
    information for the function will be printed to the console.
-   `writeToFile`: `vosonSML` `Collect` functions accept a write to file
    parameter. When set `TRUE` the collected data will be saved to a file in
    either the working directory or if set the `voson.data` directory. The
    file will be saved as a RDS file with a datetime generated name in the
    following format: `YYYY-MM-DD_HHMMSS-XXXXXXData.rds`. A log file of same
    name but with a `.txt` extension will also be written containing
    collection parameters and results.
    `writeToFile` can also be used with the `Graph` function.

The following environment options can also be used:

-   `voson.data`: If set to an existing directory path the `writeToFile`
    output files will be written to that directory instead of the working
    directory. Can be set using `options(voson.data = "~/vsml-data")` for
    example, and cleared by assigning a value of `NULL`. Directory paths can
    be relative to the working directory or full paths.
-   `voson.msg`: If set to `FALSE` then the verbose output of functions will
    be printed using the base `cat()` function instead of the `message()`
    function. Set by entering `options(voson.msg = FALSE)`, and clear by
    assigning a value of `NULL`.

### Authentication

Authentication objects generally *only need to be created once* unless your
credentials change. It is recommended to save your `mastodon`, `youtube` and
`twitter` authentication objects to file after creation and then load them in
future sessions.

*Please note in the examples provided that the "\~" notation in paths are
short-hand for the system to use the users home directory, and the "." at the
start of file names signifies it as a hidden file on some OS. You can name and
save objects however you wish.*

``` r
# youtube data api key
auth_yt <- Authenticate("youtube", apiKey = "xxxxxxxxxx")

# save the object after Authenticate
saveRDS(auth_yt, file = "~/.auth_yt")

# load a previously saved authentication object for use in Collect
auth_yt <- readRDS("~/.auth_yt")
```

### Mastodon Usage {#mastodon-usage}

This implementation of `mastodon` collection uses the `rtoot` package and is
most suited to collecting public threads or posts.

#### Collect threads or search for posts from server timelines

`Collect` can be used to collect threads by setting the parameter
`endpoint = thread` and providing the URL's for the starting post
of each thread to be collected. A mastodon server does not need to be
specified, as the function will collect the thread posts from the server
referenced in each URL.

The following example collects and combines all of the posts from the
3 threads provided. The result is a named list of two dataframes,
one containing `posts` and one with the metadata for referenced
`users` in the collection.

``` r
library(vosonSML)
options(voson.data = "./mast-data")

mast_auth <- Authenticate("mastodon")

# collect thread posts belonging to the supplied mastodon
# threads, the url of the first post in each thread should
# be used
mast_data <- mast_auth |>
  Collect(
    endpoint = "thread",
    threadUrls = c(
      "https://mastodon.social/@arstechnica/111257471648143532",
      "https://mastodon.social/@arstechnica/111257425856847171",
      "https://mastodon.social/@arstechnica/111257193332540480"
    ),
    writeToFile = TRUE,
    verbose = TRUE
  )

# Collecting post threads for mastodon urls...
#
# id                 | created
# --------------------------------------------
# 111257471648143532 | 2023-10-18 18:38:11.509
# 111257731042879692 | 2023-10-18 19:44:08
# Collected 36 posts.
# RDS file written: ./mast-data/2023-10-18_201254-MastodonData.rds
# Done.
```

#### Collect posts or search and collect posts from server timelines

`Collect` with the parameter `endpoint = search` can be used to collect
a number of the most recent posts, or the most recent posts containing
a hashtag from server timelines. This function requires a server to be
specified using the `instance` parameter.

The following example collect the most recent 100 posts made to the
`mastodon.social` server local timeline. The `local = TRUE` parameter
restricts posts to only those made by server users.

``` r
mast_data <- mast_auth |>
  Collect(
    endpoint = "search",
    instance = "mastodon.social",
    local = TRUE,
    numPosts = 100,
    writeToFile = FALSE,
    verbose = TRUE
  )

# Collecting timeline posts...
# Requested 100 posts
#
# id                 | created
# --------------------------------------------
# 111257854695457456 | 2023-10-18 20:15:36.349
# 111257844442617952 | 2023-10-18 20:12:59.92
# Collected 120 posts.
# Done.
```

The next example collects the most recent 100 posts from the
`mastodon.social` server global timeline containing the hashtag `#rstats`.
The global timeline includes posts made by users from `mastodon.social` as
well as posts made by users on its affiliated servers. The global
timeline is specified by setting `local = FALSE`.

``` r
mast_data <- mast_auth |>
  Collect(
    endpoint = "search",
    instance = "mastodon.social",
    local = FALSE,
    hashtag = "rstats",
    numPosts = 100,
    writeToFile = TRUE,
    verbose = TRUE
  )

# Collecting timeline posts...
# Hashtag: rstats
# Requested 100 posts
#
# id                 | created
# --------------------------------------------
# 111257687564078619 | 2023-10-18 19:33:04
# 111245846866572681 | 2023-10-16 17:21:51.425
# Collected 120 posts.
# RDS file written: ./mast-data/2023-10-18_201339-MastodonData.rds
# Done.
```

#### Create mastodon activity and actor network graphs

The mastodon `Create` function accepts the data from `Collect` and a type
parameter of `activity` or `actor` that specifies the type of network to
create from the collected data. `Create` produces two dataframes, one for
network `nodes` and one for node relations or `edges`. These can then be
passed to the `Graph` function to produce an `igraph` object.

##### Activity network

Nodes are `posts` and edges are the relationship to other posts. The only
relationship type supported at this time is `reply` edge.

``` r
net_activity <- mast_data |>
  Create("activity", verbose = TRUE) |>
  AddText(mast_data) |>
  Graph()

# Generating mastodon activity network...
# Done.

# IGRAPH 7b1ed0b DN-- 128 13 --
# + attr: type (g/c), name (v/c), created_at (v/n), visibility (v/c),
# | account.id (v/c), account.username (v/c), account.acct (v/c),
# | account.displayname (v/c), tag (v/x), tag_url (v/x),
# | reblogs_count (v/n), favourites_count (v/n), replies_count (v/n),
# | url (v/c), node_type (v/c), absent (v/l), vosonTxt_post (v/c),
# | created_at (e/n), edge_type (e/c)
# + edges from 7b1ed0b (vertex names):
#  [1] 111257542118730043->111257406204910121
#  [2] 111257399598738707->111257301774432049
#  [3] 111256348652983199->111256322377193800
# + ... omitted several edges
```

##### Actor network

Nodes are authors of collected posts and edges are their relationship to
other authors. The only relationship types supported at this time
are `reply` and `mention` edges.

``` r
net_actor <- mast_data |>
  Create("actor", inclMentions = TRUE, verbose = TRUE) |>
  AddText(mast_data) |>
  Graph()

# Generating mastodon actor network...
# Done.

# IGRAPH 7c9ffa7 DN-- 90 19 --
# + attr: type (g/c), name (v/c), username (v/c), acct (v/c),
# | display_name (v/c), locked (v/l), bot (v/l), discoverable (v/l),
# | group (v/l), created_at (v/n), note (v/c), url (v/c), avatar (v/c),
# | avatar_static (v/c), header (v/c), header_static (v/c),
# | followers_count (v/n), following_count (v/n), statuses_count (v/n),
# | last_status_at (v/n), fields (v/x), emojis (v/x), note.text (v/c),
# | node_type (v/c), absent (v/l), post.id (e/c), created_at (e/n),
# | edge_type (e/c), vosonTxt_post (e/c)
# + edges from 7c9ffa7 (vertex names):
# [1] 110627608153263800->109298362307469046
# [2] 109300072474546954->109259949279312293
# [3] 109297446967960427->109297446967960427
# + ... omitted several edges
```

### YouTube Usage {#youtube-usage}

#### Authenticate and Collect comments from youtube videos

YouTube uses an API key rather than an OAuth token and is simply set by
calling `Authenticate` with the key as a parameter.

``` r
# youtube authentication sets the api key
auth_yt <- Authenticate("youtube", apiKey = "xxxxxxxxxxxxxx")
```

Once the key is set then `Collect` can be used to collect the comments from
specified youtube videos. The following example collects a maximum of 100
top-level comments and all replies from each of the 2 specified video ID's. It
produces a dataframe with the combined comment data.

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

The youtube `Create` function accepts the data from `Collect` and a network
type parameter of `activity` or `actor`.

##### Activity network

Nodes are video comments and edges represent whether they were directed to the
video as a top-level comment or to another comment as a reply comment.

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
## IGRAPH 5a9fb56 DN-- 27 25 -- 
## + attr: type (g/c), name (v/c), video_id (v/c), published_at (v/c),
## | updated_at (v/c), author_id (v/c), screen_name (v/c), node_type
## | (v/c), edge_type (e/c)
## + edges from 5a9fb56 (vertex names):
## [1] Ugw13lb0nCf4o4IKFb54AaABAg->VIDEOID:AQzZNIyjyWM
## [2] UgyJBlqZ64YnltQTOTt4AaABAg->VIDEOID:AQzZNIyjyWM
## [3] Ugysomx_apk24Pqrs1h4AaABAg->VIDEOID:AQzZNIyjyWM
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
## IGRAPH 5aad4c4 DN-- 24 27 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
## | video_id (e/c), comment_id (e/c), edge_type (e/c)
## + edges from 5aad4c4 (vertex names):
##  [1] UCb9ElH9tzEkG9OxDIiSYgdg->VIDEOID:AQzZNIyjyWM
##  [2] UC0DwaB_wHNzUh-LA9sWXKYQ->VIDEOID:AQzZNIyjyWM
##  [3] UCNHA8SkizJKauefYt1FHmjQ->VIDEOID:AQzZNIyjyWM
## + ... omitted several edges
```

### Reddit Usage {#reddit-usage}

The reddit API end-point used by `vosonSML` does not require authentication
but an `Authenticate` object is still used to set up the collection and
creation operations as part of a reddit workflow.

#### Collect a thread listing from subreddit

By using the `endpoint = "listing"` parameter and a vector of subreddit names,
a list of comment threads and their metadata can be collected. The number of
list results returned per subreddit can be coarsely specified within 25 items,
by using the `max` parameter.

``` r
# specify subreddit names
subreddits <- c("datascience")

# collect a listing of the 25 top threads by upvote of all time
collect_rd_listing <- Authenticate("reddit") |>
  Collect(endpoint = "listing", subreddits = subreddits,
          sort = "top", period = "all", max = 25,
          writeToFile = TRUE, verbose = TRUE)
## Collecting thread listing for subreddits...
## Waiting between 3 and 5 seconds per request.
## Request subreddit listing: datascience (max items: 25).
## subreddit_id | subreddit   | count
## ----------------------------------
## t5_2sptq     | datascience | 25   
## Collected metadata for 25 threads in listings.
## RDS file written: ./vsml-data/2023-04-02_073117-RedditListing.rds
## Done.
```

#### Collect reddit threads

The reddit `Collect` function can then be used to collect comments from reddit
threads specified by URL's.

``` r
# specify reddit threads to collect by url
thread_urls <- c(
  "https://www.reddit.com/r/datascience/comments/wcd8x5/",
  "https://www.reddit.com/r/datascience/comments/wcni2g/"
)

# or use permalinks from a previously collected listing
thread_urls <- collect_rd_listing$permalink |> head(n = 3)

# collect comment threads with their comments sorted by best comments first
collect_rd <- Authenticate("reddit") |>
  Collect(threadUrls = thread_urls,
          sort = "best", writeToFile = TRUE, verbose = TRUE)
## Collecting comment threads for reddit urls...
## Waiting between 3 and 5 seconds per thread request.
## Request thread: r/datascience (k8nyf8) - sort: best
## Request thread: r/datascience (oeg6nl) - sort: best
## Request thread: r/datascience (hohvgq) - sort: best
## HTML decoding comments.
## thread_id | title                                         | subreddit   | count
## -------------------------------------------------------------------------------
## hohvgq    | Shout Out to All the Mediocre Data Scienti... | datascience | 272  
## k8nyf8    | data siens                                    | datascience | 77   
## oeg6nl    | The pain and excitement                       | datascience | 179
## Collected 528 total comments.
## RDS file written: ./vsml-data/2023-04-02_073130-RedditData.rds
## Done.
```

*Please note that because of the API end-point used that `Collect` is limited
to the first 500 comments per thread (plus 500 for each `continue thread`
encountered). It is therefore suited to collecting only smaller threads in
their entirety.*

#### Create reddit activity and actor networks

##### Activity network

Nodes are original thread posts and comments, edges are replies directed to
the original post and to comments made by others.

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
## IGRAPH 62e8305 DN-- 531 528 -- 
## + attr: type (g/c), name (v/c), thread_id (v/c), comm_id (v/c),
## | datetime (v/c), ts (v/n), subreddit (v/c), user (v/c), node_type
## | (v/c), edge_type (e/c)
## + edges from 62e8305 (vertex names):
##  [1] k8nyf8.1      ->k8nyf8.0     k8nyf8.1_1    ->k8nyf8.1    
##  [3] k8nyf8.1_2    ->k8nyf8.1     k8nyf8.1_2_1  ->k8nyf8.1_2  
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
## IGRAPH 62fa45c DN-- 321 531 -- 
## + attr: type (g/c), name (v/c), user (v/c), subreddit (e/c), thread_id
## | (e/c), comment_id (e/n), comm_id (e/c)
## + edges from 62fa45c (vertex names):
##  [1] 1 ->1  2 ->1  3 ->1  1 ->3  3 ->1  4 ->1  5 ->1  6 ->3  7 ->1  8 ->7 
## [11] 9 ->1  1 ->9  10->1  11->1  1 ->11 1 ->1  1 ->1  1 ->1  1 ->1  1 ->1 
## + ... omitted several edges
```

### Hyperlink Usage {#hyperlink-usage}

#### Authenticate and Collect from web sites

The `vosonSML` hyperlink collection functionality does not require
authentication as it is not using any web API's, however an `Authenticate`
object is still used to set up the collection and creation operations as part
of the `vosonSML` workflow.

The hyperlink `Collect` function accepts a dataframe of seed web pages, as
well as corresponding `type` and `max_depth` parameters for each page.

*Please note that this implementalion of hyperlink collection and networks is
still in an experimental stage.*

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

### Twitter Usage {#twitter-usage}

------------------------------------------------------------------------------

> **Note**
>
> At this time we are unable to maintain the Twitter collection
> features of `vosonSML` and advise that the following functions may no longer
> work with recent updates to the Twitter API. If problems are encountered we
> suggest trying to collect data with the `rtweet` package and using the
> `vosonSML` function [ImportRtweet](#twitter-import) to import data.

The Twitter features of this version of vosonSML require rtweet v1.1 or later.

``` r
packageVersion("rtweet")
## [1] '1.1.0'
```

#### Authenticate with the Twitter API

`Authenticate` is used to create an object that contains a Twitter token for
accessing the Twitter API. This can and should be re-used by saving it once to
file after calling `Authenticate` and then by loading it again during future
sessions.

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
collect tweets from timelines using user names. The following example collects
100 `recent` tweets for the hashtag `#auspol` and creates a dataframe with the
collected tweet data.

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
## Rate limit reset: 2023-04-02 07:43:58
## 
## tweet        | status_id           | created            
## --------------------------------------------------------
## Latest Obs   | 1642429383460925441 | 2023-04-02 07:30:54
## Earliest Obs | 1642428234058039297 | 2023-04-02 07:26:20
## Collected 100 tweets.
## RDS file written: ./vsml-data/2023-04-02_073109-TwitterData.rds
## Done.
```

The next example collects the 100 most recent tweets from the `@vosonlab` and
`@ANU_SOC` user timelines. Note that this method requires the
`endpoint = "timeline"` parameter.

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
## Rate limit reset: 2023-04-02 07:44:01
## 
## tweet        | status_id           | created            
## --------------------------------------------------------
## Latest Obs   | 1641303253823680512 | 2023-03-30 04:56:04
## Earliest Obs | 1437995199636983808 | 2021-09-15 04:22:26
## Collected 200 tweets.
## RDS file written: ./vsml-data/2023-04-02_073113-TwitterData.rds
## Done.
```

The output for these methods also lists the earliest and most recent tweet as
well as the number of tweets collected.

##### Importing tweets from rtweet {#twitter-import}

Because `vosonSML` uses the `rtweet` package to access and collect tweets
data, `rtweet` data is also able to be easily imported from dataframe or file
and then transformed into a `Collect` object for further use.

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
parameter of `activity`, `actor`, `semantic` or `twomode` that specifies the
type of network to create from the collected data. `Create` produces two
dataframes, one for network `nodes` and one for node relations or `edges` in
the network. These can then undergo further processing as per the
[supplemental functions](#supplemental-functions) section or be passed to the
`Graph` function that creates an `igraph` object.

##### Activity network

Nodes are tweets and edges are the relationship to other tweets such as
`reply`, `retweet` or `quote` tweets.

``` r
net_activity <- collect_tw |> Create("activity", verbose = TRUE)
## Generating twitter activity network...
## -------------------------
## collected tweets | 100
## tweet            | 2 
## retweet          | 92
## reply            | 4 
## quote            | 2 
## nodes            | 167
## edges            | 100
## -------------------------
## Done.
```

``` r
g_activity <- net_activity |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-04-02_173114-TwitterActivity.graphml
## Done.

g_activity
## IGRAPH 5943f8d DN-- 167 100 -- 
## + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name
## | (v/c), created_at (v/c), user_id (e/c), screen_name (e/c), created_at
## | (e/c), edge_type (e/c)
## + edges from 5943f8d (vertex names):
##  [1] 1642429383460925441->1642343694660677632
##  [2] 1642429382257147904->1641392034991726592
## + ... omitted several edges
```

##### Actor network

Nodes are twitter users and edges are the relationship to other users in the
network such as `reply`, `mention`, `retweet` and `quote` tweets. Mentions can
be excluded by setting the parameter `inclMentions` to `FALSE`.

``` r
net_actor <- collect_tw |>
  Create("actor", inclMentions = TRUE, verbose = TRUE)
## Generating twitter actor network...
## -------------------------
## collected tweets | 100
## tweet            | 2 
## retweet          | 92
## reply mention    | 1 
## reply            | 4 
## quote mention    | 3 
## quote            | 2 
## nodes            | 125
## edges            | 104
## -------------------------
## Done.
```

``` r
g_actor <- net_actor |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-04-02_173115-TwitterActor.graphml
## Done.

g_actor
## IGRAPH 59a5dc7 DN-- 125 104 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), status_id (e/c),
## | created_at (e/c), edge_type (e/c)
## + edges from 59a5dc7 (vertex names):
##  [1] 2849412290         ->2882834947
##  [2] 141563430          ->4168028659
## + ... omitted several edges
```

##### Semantic network

Nodes are concepts represented as common `words` and `hashtags`. Edges
represent the occurence of a particular word and a particular hashtag in the
same tweet. The semantic network is `undirected`.

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
## retweets                 | 92
## tokens                   | 231
## removed specified        | 8 
## removed users            | 7 
## hashtag count            | 9 
## hashtags unique          | 9 
## term count               | 89
## terms unique             | 85
## top 20% hashtags n (>=1) | 9 
## top 10% terms n (>=1)    | 85
## nodes                    | 41
## edges                    | 91
## -------------------------
## Done.
```

``` r
g_semantic <- net_semantic |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-04-02_173115-TwitterSemantic.graphml
## Done.

g_semantic
## IGRAPH 59de049 UN-B 41 91 -- 
## + attr: type (g/c), name (v/c), type (v/c), n (v/n), from.type (e/c),
## | to.type (e/c), status_id (e/c)
## + edges from 59de049 (vertex names):
##  [1] immoral     --#assisteddy... immoral     --#vad
##  [3] immoral     --#endoflif...   immoral     --#humanrights
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
## removed specified | 8 
## users             | 7 
## hashtags          | 9 
## nodes             | 22
## edges             | 16
## -------------------------
## Done.
```

``` r
g_2mode <- net_2mode |> Graph(writeToFile = TRUE, verbose = TRUE)
## Creating igraph network graph...
## GRAPHML file written: ./vsml-data/2023-04-02_173115-Twitter2mode.graphml
## Done.

mask(g_2mode)
## IGRAPH 59f5de1 DNWB 22 16 -- 
## + attr: type (g/c), name (v/c), type (v/c), user_id (v/c), screen_name
## | (v/c), status_id (e/c), created_at (e/c), is_retweet (e/l), is_quote
## | (e/l), is_reply (e/l), weight (e/n)
## + edges from 59f5de1 (vertex names):
##  [1] @uxxxxxxxxxlg  ->#assisteddy...   @uxxxxxxxxxlg  ->#vad            
##  [3] @uxxxxxxxxxlg  ->#endoflif...     @uxxxxxxxxxlg  ->#humanrights    
##  [5] @axxxxdo       ->#turnbull        @axxxxdo       ->#murdoch        
##  [7] @axxxxdo       ->#media           @cxxxxxxxxbc   ->@cxxxxxxxxxnl
## + ... omitted several edges
```

### <a name="supplemental-functions"></a>Supplemental Functions {#supplemental-functions}

#### Merge collected data together

The `Merge` and `MergeFiles` functions allow two or more `Collect` objects to
be merged together provided they are of the same datasource type e.g
`twitter`, `youtube`.

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

The `AddText` function can be used following the creation of all networks for
`mastodon`, `youtube`, `reddit` and `twitter`. It will add an attribute
starting with `vosonTxt_` to nodes of `activity` networks and to edges of
`actor` networks. It requires a collected `datasource` from which to extract
text data.

An additional parameter `hashtags` is available for `twitter` networks that
will add tweet hashtags as an attribute.

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
## IGRAPH 635ad2d DN-- 167 100 -- 
## + attr: type (g/c), name (v/c), author_id (v/c), author_screen_name
## | (v/c), created_at (v/c), t.is_reply (v/l), t.is_quote (v/l),
## | t.is_retweet (v/l), t.full_text (v/c), t.hashtags (v/x),
## | t.quoted.status_id (v/c), t.quoted.full_text (v/c), t.quoted.hashtags
## | (v/x), t.retweeted.status_id (v/c), t.retweeted.full_text (v/c),
## | t.retweeted.hashtags (v/x), vosonTxt_tweet (v/c), vosonTxt_hashtags
## | (v/c), user_id (e/c), screen_name (e/c), created_at (e/c), edge_type
## | (e/c)
## + edges from 635ad2d (vertex names):
## [1] 1642429383460925441->1642343694660677632
## + ... omitted several edges
```

`AddText` will also redirect some edges in a youtube `actor` network by
finding user references at the beginning of reply comments text using the
`repliesFromText` parameter. In the following example an edge would be
redirected from `UserC` to `UserB` by text reference as opposed to `UserA` who
made the top-level comment both users are replying to.

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

`AddUserData` adds user profile information from the `users` dataframe to as
many users in a twitter `actor` and `2mode` network as possible. If the
profile information is not available for referenced users in the collect data
then the user id and name will be added to the `missing_users` dataframe. If
the profile metadata is not available in the collect data and the
`lookupUsers` parameter is set then additional twitter API requests will be
made to retrieve the missing information.

``` r
# add additional twitter user profile info
net_actor <- collect_tw |> Create("actor")

net_actor_meta <- net_actor |> AddUserData(collect_tw, verbose = TRUE)
## Adding user data to network...Done.

names(net_actor_meta)
## [1] "edges"         "nodes"         "missing_users"
nrow(net_actor_meta$missing_users)
## [1] 7

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

For reference the `AddUserData` function will also add a new dataframe to the
`actor_network` network list containing the retrieved user metadata.

``` r
g_actor <- net_actor_meta |> Graph()

g_actor
## IGRAPH 642546e DN-- 125 104 -- 
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
## + edges from 642546e (vertex names):
```

#### AddVideoData requests and adds video data to networks

`AddVideoData` adds video information as node attributes in youtube `actor`
networks and replaces the video ID nodes with a user (channel owner or
publisher). The `actorSubOnly` parameter can be used to only perform the ID
substitution.

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

`AddVideoData` function will also add a new dataframe to the `actor_network`
network list containing the retrieved video information called `videos`.

``` r
g_actor <- net_actor |> Graph()

g_actor
## IGRAPH 644cb17 DN-- 23 27 -- 
## + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
## | video_id (e/c), comment_id (e/c), edge_type (e/c), video_title (e/c),
## | video_description (e/c), video_published_at (e/c)
## + edges from 644cb17 (vertex names):
## [1] UCb9ElH9tzEkG9OxDIiSYgdg->UCeiiqmVK07qhY-wvg3IZiZQ
## [2] UC0DwaB_wHNzUh-LA9sWXKYQ->UCeiiqmVK07qhY-wvg3IZiZQ
## + ... omitted several edges
```

## Where to next?

Continue working with the network graphs using the `igraph` package and check
out some examples of plots in the [Introduction to
vosonSML](https://vosonlab.github.io/vosonSML/articles/Intro-to-vosonSML.html)
vignette. The `graphml` files produced by `vosonSML` are also easily imported
into software such as [Gephi](https://gephi.org/) for further visualization
and exploration of networks.

As an alternative to `vosonSML` using the R command-line interface we have
also developed an R Shiny app called [VOSON
Dash](https://vosonlab.github.io/VOSONDash/). It provides a user friendly GUI
for the collection of data using `vosonSML` and has additional network
visualization and analysis features.

For more detailed information about functions and their parameters, please
refer to the
[Reference](https://vosonlab.github.io/vosonSML/reference/index.html) page.

## Special thanks

This package would not be possible without key packages by other authors in
the R community, particularly:
[data.table](https://github.com/Rdatatable/data.table),
[dplyr](https://github.com/tidyverse/dplyr),
[httr](https://github.com/r-lib/httr),
[igraph](https://github.com/igraph/rigraph),
[RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR), rtoot,
[rtweet](https://github.com/ropensci/rtweet) and
[tidytext](https://github.com/juliasilge/tidytext).

## Code of Conduct

Please note that the VOSON Lab projects are released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
