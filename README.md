# vosonSML - Social Media Lab<img src="https://vosonlab.github.io/vosonSML/images/logo.png" width="140px" align="right"/>

[![Github_Dev](https://img.shields.io/static/v1?label=dev&message=v0.35&logo=github)](https://github.com/vosonlab/vosonSML)
[![Last_Commit](https://img.shields.io/github/last-commit/vosonlab/vosonSML.svg?&logo=github)](https://github.com/vosonlab/vosonSML/commits/master)
[![Build_Status](https://github.com/vosonlab/vosonSML/workflows/R-CMD-check/badge.svg)](https://github.com/vosonlab/vosonSML/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Monthly](https://cranlogs.r-pkg.org/badges/vosonSML)](https://CRAN.R-project.org/package=vosonSML)
[![CRAN_Total](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML)](https://CRAN.R-project.org/package=vosonSML)

The `vosonSML` R package is a suite of easy to use functions for
collecting and generating different types of networks from social media
data. The package supports the collection of data from the `mastodon`,
`reddit`, `youtube` platforms and `hyperlinks` from web sites. Networks
in the form of node and edge lists can be generated from collected data,
supplemented with additional metadata, and used to create graphs for
Social Network Analysis.

## Installation Options

Installing the github version is recommended at this time as CRAN
releases may occur less frequently.

1.  Install the latest github version:

``` r
# library(remotes)
remotes::install_github("vosonlab/vosonSML")
```

2.  Install the CRAN release version:

``` r
install.packages("vosonSML")
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

[Mastodon](#mastodon-usage) \| [Reddit](#reddit-usage) \|
[YouTube](#youtube-usage) \| [Hyperlink](#hyperlink-usage) \|
[Supplemental Functions](#supplemental-functions)

### General Options

-   `verbose`: most `vosonSML` functions accept a verbosity parameter
    that is now set to `TRUE` by default. When `FALSE` functions will
    run silently unless there is a warning or error. If set to `TRUE`
    then progress and summary information for the function will be
    printed to the console.
-   `writeToFile`: `vosonSML` functions accept a write to file
    parameter. When set `TRUE` the collected data will be saved to a
    file in either the working directory or a directory set by the
    `voson.data` option. The file will be saved as a RDS file with a
    datetime generated name in the following format:
    `YYYY-MM-DD_HHMMSS-XXXXXX` as a `rds` or `graphml` file.

The following environment options can also be used:

-   `voson.data`: If set to an existing directory path the `writeToFile`
    output files will be written to that directory instead of the
    working directory. Can be set using
    `options(voson.data = "~/vsml-data")` for example, and is cleared by
    assigning a value of `NULL`. Directory paths can be relative to the
    working directory e.g. `./data` or full paths.
-   `voson.cat`: If set to `TRUE` then the verbose output of functions
    will be printed using the base `cat()` function instead of the
    `message()` function. Set by entering `options(voson.cat = TRUE)`,
    and clear by assigning a value of `NULL`.

### Authentication

Authentication objects generally *only need to be created once* unless
your credentials change. It is recommended to save your `mastodon` and
`youtube` authentication objects to file after creation and then load
them in future sessions.

*Please note in the examples provided that the "\~" notation in paths
are short-hand for the system to use the users home directory, and the
"." at the start of file names signifies it as a hidden file on some OS.
You can name and save objects however you wish.*

``` r
# youtube data api key
youtube_auth <- Authenticate("youtube", apiKey = "xxxxxxxxxx")

# save the object after Authenticate
saveRDS(youtube_auth, file = "~/.auth_yt")

# load a previously saved authentication object for use in Collect
youtube_auth <- readRDS("~/.auth_yt")
```

### <a name="mastodon-usage"/>Mastodon Usage {#mastodon-usage}

This implementation of `mastodon` collection uses the `rtoot` package
and is most suited to collecting public posts.

#### Collect threads or search for posts from server timelines

`Collect` can be used to collect threads by setting the parameter
`endpoint = thread` and providing the URL's for the starting post of
each thread to be collected. A mastodon server does not need to be
specified, as the function will collect the thread posts from the server
referenced in each URL.

The following example collects and combines all of the posts from the 3
threads provided. The result is a named list of two dataframes, one
containing `posts` and one with the metadata for referenced `users` in
the collection.

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
    writeToFile = TRUE
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
a number of the most recent posts, or the most recent posts containing a
hashtag from server timelines. This function requires a server to be
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
    numPosts = 100
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
`mastodon.social` server global timeline containing the hashtag
`#rstats`. The global timeline includes posts made by users from
`mastodon.social` as well as posts made by users on its affiliated
servers. The global timeline is specified by setting `local = FALSE`.

``` r
mast_data <- mast_auth |>
  Collect(
    endpoint = "search",
    instance = "mastodon.social",
    local = FALSE,
    hashtag = "rstats",
    numPosts = 100,
    writeToFile = TRUE
  )

# Collecting timeline posts...
# Hashtag: rstats
# Requested 100 posts
# 
# id                 | created            
# ----------------------------------------
# 111851761879684588 | 2024-01-31 17:33:57
# 111839343172130565 | 2024-01-29 12:55:38
# Collected 120 posts.
# RDS file written: 2024-01-31_190125-MastodonData.rds
# Done.
```

#### Create mastodon activity and actor network graphs

The mastodon `Create` function accepts the data from `Collect` and a
type parameter of `activity` or `actor` that specifies the type of
network to create from the collected data. `Create` produces two
dataframes, one for network `nodes` and one for node relations or
`edges`. These can then be passed to the `Graph` function to produce an
`igraph` object.

##### Activity network

Nodes are `posts` and edges are the relationship to other posts. The
only relationship type supported at this time is `reply` edge.

``` r
net_activity <- mast_data |>
  Create("activity") |>
  AddText(mast_data) |>
  Graph()

# Generating mastodon activity network...
# Done.

# IGRAPH 7cc21ba DN-- 128 12 -- 
# + attr: type (g/c), name (v/c), post.created_at (v/n),
# | post.visibility (v/c), account.id (v/c), account.username
# | (v/c), account.acct (v/c), account.displayname (v/c),
# | user.avatar (v/c), post.tags (v/x), post.tags.urls (v/x),
# | post.reblogs_count (v/n), post.favourites_count (v/n),
# | post.replies_count (v/n), post.url (v/c), node_type (v/c),
# | absent (v/l), vosonTxt_post (v/c), created_at (e/n), edge_type
# | (e/c)
# + edges from 7cc21ba (vertex names):
# [1] 111851737032132167->111846251799585000
# + ... omitted several edges
```

##### Tag network

A variation on the mastodon `activity` network is the subtype `tag`. A
tag network is a netork of tags (hashtags) found in posts, and their
coocurrence with other tags within same posts used to create relations.

``` r
net_tag <- mast_data |>
   Create("activity", subtype = "tag") |>
   Graph()
   
# Generating mastodon activity network...
# Done.

# IGRAPH 23e6e20 DN-- 94 624 -- 
# + attr: type (g/c), name (v/c), post.id (e/c), edge_type (e/c)
# + edges from 23e6e20 (vertex names):
#  [1] peerreviewed   ->apackageaday    peerreviewed   ->oss            
#  [3] peerreviewed   ->rstats          apackageaday   ->peerreviewed   
#  [5] apackageaday   ->oss             apackageaday   ->rstats         
#  [7] oss            ->peerreviewed    oss            ->apackageaday   
#  [9] oss            ->rstats          rstats         ->peerreviewed   
# [11] rstats         ->apackageaday    rstats         ->oss            
# [13] rstats         ->reproducibility reproducibility->rstats         
# [15] rshiny         ->rstats          rstats         ->rshiny         
# + ... omitted several edges
```

##### Actor network

Nodes are authors of collected posts and edges are their relationship to
other authors. The only relationship types supported at this time are
`reply` and `mention` edges.

``` r
net_actor <- mast_data |>
  Create("actor", inclMentions = TRUE) |>
  AddText(mast_data) |>
  Graph()

# Generating mastodon actor network...
# Done.

# IGRAPH c46e984 DN-B 82 12 -- 
# + attr: type (g/c), name (v/c), user.acct (v/c), user.username
# | (v/c), user.displayname (v/c), user.url (v/c), user.avatar
# | (v/c), type (v/c), absent (v/l), post.id (e/c),
# | post.created_at (e/n), edge_type (e/c), vosonTxt_post (e/c)
# + edges from c46e984 (vertex names):
# [1] 109610301164555149->109610301164555149
# + ... omitted several edges
```

##### Server network

A variation on the mastodon `actor` network is the subtype `server`. A
server network simply groups the users into single actors as represented
by their servers, and similarly combines their relations at the server
level.

``` r
net_server <- mast_data |>
   Create("actor", subtype = "server") |>
   Graph()

# Generating mastodon actor network...
# Done.

# IGRAPH 845c991 DN-- 23 10 -- 
# + attr: type (g/c), name (v/c), n (v/n), edge_type (e/c)
# + edges from 845c991 (vertex names):
#  [1] fosstodon.org  ->fosstodon.org   fosstodon.org  ->fosstodon.org  
#  [3] aus.social     ->aus.social      fosstodon.org  ->fosstodon.org  
#  [5] mastodon.social->mastodon.social fosstodon.org  ->fosstodon.org  
#  [7] fosstodon.org  ->fosstodon.org   fosstodon.org  ->fosstodon.org  
#  [9] mastodon.social->hachyderm.io    mstdn.social   ->mstdn.social
```

### <a name="youtube-usage"/>YouTube Usage {#youtube-usage}

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
video ID's. It produces a dataframe with the combined comment data.

``` r
video_url <- c("https://www.youtube.com/watch?v=AQzZNIyjyWM",
               "https://www.youtube.com/watch?v=lY0YLDZhT88&t=3152s")

collect_yt <- auth_yt |>
  Collect(videoIDs = video_url,
          maxComments = 100)
          
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
net_activity <- collect_yt |> Create("activity")

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
net_actor <- collect_yt |> Create("actor")

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

### <a name="reddit-usage"/>Reddit Usage {#reddit-usage}

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

The reddit `Collect` function can then be used to collect comments from
reddit threads specified by URL's.

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

*Please note that because of the API end-point used that `Collect` is
limited to the first 500 comments per thread (plus 500 for each
`continue thread` encountered). It is therefore suited to collecting
only smaller threads in their entirety.*

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

### <a name="hyperlink-usage"/>Hyperlink Usage {#hyperlink-usage}

#### Authenticate and Collect from web sites

The `vosonSML` hyperlink collection functionality does not require
authentication as it is not using any web API's, however an
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
   tibble::tribble(
     ~page, ~type, ~max_depth,
     "http://vosonlab.net", "ext", 2,
     "https://www.oii.ox.ac.uk", "ext", 2,
     "https://sonic.northwestern.edu", "ext", 2
   )

#  A tibble: 3 × 3
#   page                           type  max_depth
#   <chr>                          <chr>     <dbl>
# 1 http://vosonlab.net            ext           2
# 2 https://www.oii.ox.ac.uk       ext           2
# 3 https://sonic.northwestern.edu ext           2

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

### <a name="supplemental-functions"/>Supplemental Functions {#supplemental-functions}

#### Merge collected data together

The `Merge` and `MergeFiles` functions allow two or more `Collect`
objects to be merged together provided they are of the same datasource
type e.g `mastodon`.

``` r
get_hashtag_data <- function(tag) {
  Authenticate("mastodon") |>
    Collect(
      endpoint = "search",
      instance = "mastodon.social",
      local = FALSE,
      hashtag = tag,
      numPosts = 100,
      writeToFile = TRUE
    )
}

library(vosonSML)
options(voson.data = "./mast-data")

# collect data
mast_rstats <- get_hashtag_data("rstats")
mast_python <- get_hashtag_data("python")

# merge collect objects
data <- Merge(mast_rstats, mast_python, writeToFile = TRUE)

# Merging collect data...
# RDS file written: ./mast-data/2024-07-21_150353-MastodonDataMerge.rds
# Done.

# merge files from a data directory
data <- MergeFiles(
  "./mast-data", pattern = "*MastodonData.rds", writeToFile = TRUE
)

# Merging collect files...
# Matching files:
# - ./mast-data/2024-07-21_035919-MastodonData.rds
# - ./mast-data/2024-07-21_035925-MastodonData.rds
# Merging collect data...
# RDS file written: ./mast-data/2024-07-21_150544-DataMergeFile.rds
# Done.
```

#### AddText adds collected text data to networks as node or edge attributes

The `AddText` function can be used following the creation of all
networks for `mastodon`, `youtube` and `reddit`. It will add an
attribute starting with `vosonTxt_` to nodes of `activity` networks and
to edges of `actor` networks. It requires a collected `datasource` from
which to extract text data.

``` r
# create activity network
net_activity <- data |> Create("activity")

# activity network with text data added as node attribute
net_activity <- net_activity |> AddText(data)
  
## Adding text data to network...Done.
```

``` r
names(net_activity)
#  [1] "post.id"               "post.created_at"      
# ..             
# [17] "vosonTxt_post"
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
  AddText(collect_yt, repliesFromText = TRUE)

## Adding text data to network...Done.
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

This package would not be possible without a number of excellent
packages created by others in the R community, we would especially like
to thank the authors of the [dplyr](https://github.com/tidyverse/dplyr),
[httr2](https://github.com/r-lib/httr2),
[igraph](https://github.com/igraph/rigraph),
[RedditExtractoR](https://github.com/ivan-rivera/RedditExtractoR),
[rtoot](https://gesistsa.github.io/rtoot/)

## Code of Conduct

Please note that the VOSON Lab projects are released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
