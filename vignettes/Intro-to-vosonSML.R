## ----eval=FALSE---------------------------------------------------------------
# library(vosonSML)

## ----eval=FALSE---------------------------------------------------------------
# # create auth object with api key
# youtubeAuth <- Authenticate("youtube", apiKey = "xxxxxxxx")

## ----eval=FALSE---------------------------------------------------------------
# saveRDS(youtubeAuth, file = "youtube_auth")

## ----eval=FALSE---------------------------------------------------------------
# youtubeAuth <- readRDS("youtube_auth")

## ----eval=FALSE---------------------------------------------------------------
# videoIDs <- c(
#   "xxxxxx",
#   "https://www.youtube.com/watch?v=xxxxxxxx",
#   "https://youtu.be/xxxxxxxx")
# )

## ----eval=FALSE---------------------------------------------------------------
# videoID <- "https://www.youtube.com/watch?v=pJ_NyEYRkLQ"
# youtubeData <- youtubeAuth |>
#   Collect(videoID, maxComments = 500, writeToFile = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# > str(youtubeData)
# Classes ‘dataource’, ‘youtube’ and 'data.frame':	603 obs. of  12 variables:
#  $ Comment              : chr  "xxxxx"
#  $ AuthorDisplayName    : chr  "xx" "xx" "xx" "xx"
#  $ AuthorProfileImageUrl: chr  "https://xx" "https://xx" "https://xx"
#  $ AuthorChannelUrl     : chr  "http://xx" "http://xx" "http://xx" "http://xx"
#  $ AuthorChannelID      : chr  "xx" "xx" "xx" "xx"
#  $ ReplyCount           : chr  "0" "0" "0" "0"
#  $ LikeCount            : chr  "0" "0" "0" "0"
#  $ PublishedAt          : chr  "2020-01-10T02:23:43" "2020-01-09T20:56:23"
#                                "2020-01-09T20:44:00" "2020-01-09T19:31:32"
#  $ UpdatedAt            : chr  "2020-01-10T02:23:43" "2020-01-09T20:56:23"
#                                "2020-01-09T20:44:00" "2020-01-09T19:31:32"
#  $ CommentID            : chr  "xx" "xx" "xx" "xx"
#  $ ParentID             : chr  NA NA NA NA
#  $ VideoID              : chr  "pJ_NyLQ" "pJ_NyLQ" "pJ_NyLQ" "pJ_NyLQ"

## ----eval=FALSE---------------------------------------------------------------
# # read dataframe from file
# youtubeData <- readRDS("2020-09-26_095354-YoutubeData.rds")

## ----eval=FALSE---------------------------------------------------------------
# actorNetwork <- youtubeData |> Create("actor") |> AddText(youtubeData)
# actorGraph <- actorNetwork |> Graph(writeToFile = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# > actorNetwork
# $nodes
# # A tibble: 522 x 3
#    id                       screen_name                         node_type
#    <chr>                    <chr>                               <chr>
#  1 xxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxxx                      actor
#  2 xxxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxx                       actor
# [snip]
# # … with 512 more rows
# 
# $edges
# # A tibble: 604 x 6
#    from      to       video_id comment_id   edge_type vosonTxt_comment
#    <chr>     <chr>    <chr>    <chr>        <chr>     <chr>
#  1 xxxxxxxx… VIDEOID… pJ_NyEY… xxxxxxxxxxx… comment   "xxxxx"
#  2 xxxxxxxx… VIDEOID… pJ_NyEY… xxxxxxxxxxx… comment   "xxxxx"
# [snip]
# # … with 594 more rows
# 
# attr(,"class")
# [1] "list"       "network"    "actor"      "youtube"    "voson_text"

## ----eval=FALSE---------------------------------------------------------------
# actorGraph <- youtubeData |> Create("actor") |> AddText(youtubeData) |> Graph()

## ----eval=FALSE---------------------------------------------------------------
# > actorGraph
# IGRAPH 79e5456 DN-- 522 604 --
# + attr: type (g/c), name (v/c), screen_name (v/c), node_type (v/c),
# | label (v/c), video_id (e/c), comment_id (e/c), edge_type (e/c),
# | vosonTxt_comment (e/c)
# + edges from 79e5456 (vertex names):
# [1] xxxx->VIDEOID:pJ_NyEYRkLQ
# [2] xxxx->VIDEOID:pJ_NyEYRkLQ
# [snip]
# + ... omitted several edges

## ----eval=FALSE---------------------------------------------------------------
# > table(E(actorGraph)$edge_type)
#    comment reply-comment     self-loop
#        500           103             1

## ----eval=FALSE---------------------------------------------------------------
# # change color of nodes with type video to red and others grey
# V(actorGraph)$color <- ifelse(
#   V(actorGraph)$node_type == "video", "red", "grey"
# )
# 
# # open and write plot to a png file
# png("youtube_actor.png", width = 600, height = 600)
# plot(actorGraph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
# dev.off()

## ----eval=FALSE---------------------------------------------------------------
# # removed edges that are not of type reply-comment
# g2 <- delete.edges(
#   actorGraph, which(E(actorGraph)$edge_type != "reply-comment")
# )
# 
# # check number of isolates
# > length(which(degree(g2) == 0))
# [1] 417
# 
# # remove isolates
# g2 <- delete.vertices(g2, which(degree(g2) == 0))
# 
# # get node indexes for the tails of edges that have comments containing
# # words of interest change the indexed node colors to red and others grey
# V(g2)$color <- "grey"
# ind <- tail_of(
#   actorGraph,
#   grep("arson|backburn|climate change", tolower(E(g2)$vosonTxt_comment))
# )
# V(g2)$color[ind] <- "red"
# 
# # open and write plot to a png file
# png("youtube_actor_reply.png", width = 600, height = 600)
# plot(g2, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
# dev.off()

## ----eval=FALSE---------------------------------------------------------------
# actorNetwork_withVideoInfo <- actorNetwork |> AddVideoData(youtubeAuth)

## ----eval=FALSE---------------------------------------------------------------
# > actorNetwork_withVideoInfo
# $nodes
# # A tibble: 522 x 3
#    id                       screen_name                         node_type
#    <chr>                    <chr>                               <chr>
#  1 xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxx                       actor
#  2 xxxxxxxxxxxxxxxxxxxxxxxx xxxxxxxxxxxxx                       actor
# [snip]
# # … with 512 more rows
# 
# $edges
# # A tibble: 604 x 9
#    from  to    video_id comment_id edge_type vosonTxt_comment video_title
#    <chr> <chr> <chr>    <chr>      <chr>     <chr>            <chr>
#  1 xxxx… xxxx… pJ_NyEY… xxxxxxxxx… comment   xxxxxxxxxxxx … Australia …
#  2 xxxx… xxxx… pJ_NyEY… xxxxxxxxx… comment   "xxxx"         Australia …
# [snip]
# # … with 594 more rows, and 2 more variables: video_description <chr>,
# #   video_published_at <chr>
# 
# $videos
# # A tibble: 1 x 6
#  VideoID  VideoTitle  VideoDescription VideoPublishedAt ChannelID ChannelTitle
#  <chr>    <chr>       <chr>            <chr>            <chr>     <chr>
# 1 pJ_NyEY… Australia … "As Australia ba… 2020-01-05T12:3… UCknLrEd… DW News
# 
# attr(,"class")
# [1] "list"             "network"          "actor"            "youtube"
# [5] "voson_text"       "voson_video_data"

## ----eval=FALSE---------------------------------------------------------------
# activityNetwork <- youtubeData |> Create("activity") |> AddText(youtubeData)
# activityGraph <- activityNetwork |> Graph()

## ----eval=FALSE---------------------------------------------------------------
# > activityNetwork
# $edges
# # A tibble: 603 x 3
#    from                       to                  edge_type
#    <chr>                      <chr>               <chr>
#  1 xxxxxxxxxxxxxxxxxxxxxxxxxx VIDEOID:pJ_NyEYRkLQ comment
#  2 xxxxxxxxxxxxxxxxxxxxxxxxxx VIDEOID:pJ_NyEYRkLQ comment
# [snip]
# # … with 593 more rows
# 
# $nodes
# # A tibble: 604 x 8
#    id    video_id published_at updated_at author_id screen_name node_type
#    <chr> <chr>    <chr>        <chr>      <chr>     <chr>       <chr>
#  1 xxxx… pJ_NyEY… 2020-01-10T… 2020-01-1… xxxxxxxx… xxxxxxxxxx… comment
#  2 xxxx… pJ_NyEY… 2020-01-09T… 2020-01-0… xxxxxxxx… xxxxxxxxxx… comment
# [snip]
# # … with 594 more rows, and 1 more variable: vosonTxt_comment <chr>
# 
# attr(,"class")
# [1] "list"       "network"    "activity"   "youtube"    "voson_text"

## ----eval=FALSE---------------------------------------------------------------
# IGRAPH 02664d1 DN-- 604 603 --
# + attr: type (g/c), name (v/c), video_id (v/c), published_at (v/c),
# | updated_at (v/c), author_id (v/c), screen_name (v/c), node_type
# | (v/c), vosonTxt_comment (v/c), label (v/c), edge_type (e/c)
# + edges from 02664d1 (vertex names):
# [1] xxxx->VIDEOID:pJ_NyEYRkLQ
# [2] xxxx->VIDEOID:pJ_NyEYRkLQ
# [3] xxxx->VIDEOID:pJ_NyEYRkLQ
# [4] xxxx->VIDEOID:pJ_NyEYRkLQ
# [5] xxxx->VIDEOID:pJ_NyEYRkLQ
# [6] xxxx->VIDEOID:pJ_NyEYRkLQ
# + ... omitted several edges

## ----eval=FALSE---------------------------------------------------------------
# # set all video node colors to red and others to grey
# V(activityGraph)$color <- "grey"
# V(activityGraph)$color[which(V(activityGraph)$node_type == "video")] <- "red"
# 
# # get node indexes of comments that contain terms of interest
# # set their node colors to blue
# ind <- grep(
#   "arson|backburn|climate change", tolower(V(activityGraph)$vosonTxt_comment)
# )
# V(activityGraph)$color[ind] <- "blue"
# 
# # open and write plot to a png file
# png("youtube_activity.png", width = 600, height = 600)
# plot(activityGraph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
# dev.off()

## ----eval=FALSE---------------------------------------------------------------
# myThreadUrls <- c(
#   "https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/",
#   "https://www.reddit.com/r/xxxxxx/comments/xxxxxx/x_xxxx_xxxxxxxxx/"
# )

## ----eval=FALSE---------------------------------------------------------------
# myThreadUrls <- "https://www.reddit.com/r/worldnews/comments/elcb9b/australias_leaders_deny_link_between_climate/"
# redditData <- Authenticate("reddit") |>
#               Collect(threadUrls = myThreadUrls, writeToFile = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# > str(redditData)
# Classes ‘tbl_df’, ‘tbl’, ‘datasource’, ‘reddit’ and 'data.frame':	
#   767 obs. of  22 variables:
#  $ id              : int  1 2 3 4 5 6 7 8 9 10 ...
#  $ structure       : chr  "1" "4_1_1_1_1_1_1_1_1_1" "4_1_1_4_2_1_1_1_1_1" ...
#  $ post_date       : chr  "2020-01-07 14:34:58" "2020-01-07 14:34:58" ...
#  $ post_date_unix  : num  1.58e+09 1.58e+09 1.58e+09 1.58e+09 1.58e+09 ...
#  $ comm_id         : chr  "xxxx" "xxxx" "xxxx" "xxxx" ...
#  $ comm_date       : chr  "2020-01-07 19:11:10" "2020-01-07 21:04:05" ...
#  $ comm_date_unix  : num  1.58e+09 1.58e+09 1.58e+09 1.58e+09 1.58e+09 ...
#  $ num_comments    : int  4435 4435 4435 4435 4435 4435 4435 4435 4435 4435 ...
#  $ subreddit       : chr  "worldnews" "worldnews" "worldnews" "worldnews" ...
#  $ upvote_prop     : num  0.91 0.91 0.91 0.91 0.91 0.91 0.91 0.91 0.91 0.91 ...
#  $ post_score      : int  45714 45714 45714 45712 45714 45710 45720 45712 ..
#  $ author          : chr  "xxxx" "xxxx" "xxxx" "xxxx" ...
#  $ user            : chr  "xxxx" "xxxx" "xxxx" "xxxx" ...
#  $ comment_score   : int  1904 136 17 13 9 9 125 4 6 12 ...
#  $ controversiality: int  0 0 0 0 0 0 0 0 0 0 ...
#  $ comment         : chr  "xxxx...
#  $ title           : chr  "Australia’s leaders deny link between climate change and the country’s devastating bushfires" "Australia’s leaders deny link between climate change and the country’s devastating bushfires" "Australia’s leaders deny link between climate change and the country’s devastating bushfires" "Australia’s leaders deny link between climate change and the country’s devastating bushfires" ...
#  $ post_text       : chr  "" "" "" "" ...
#  $ link            : chr  "https://www.theglobeandmail.com/world/article-australias-leaders-unmoved-on-climate-action-after-devastating-2/" "https://www.theglobeandmail.com/world/article-australias-leaders-unmoved-on-climate-action-after-devastating-2/" "https://www.theglobeandmail.com/world/article-australias-leaders-unmoved-on-climate-action-after-devastating-2/" "https://www.theglobeandmail.com/world/article-australias-leaders-unmoved-on-climate-action-after-devastating-2/" ...
#  $ domain          : chr  "theglobeandmail.com" "theglobeandmail.com" "theglobeandmail.com" "theglobeandmail.com" ...
#  $ url             : chr  "https://www.reddit.com/r/worldnews/comments/elcb9b/australias_leaders_deny_link_between_climate/" "https://www.reddit.com/r/worldnews/comments/elcb9b/australias_leaders_deny_link_between_climate/" "https://www.reddit.com/r/worldnews/comments/elcb9b/australias_leaders_deny_link_between_climate/" "https://www.reddit.com/r/worldnews/comments/elcb9b/australias_leaders_deny_link_between_climate/" ...
#  $ thread_id       : chr  "elcb9b" "elcb9b" "elcb9b" "elcb9b" ...

## ----eval=FALSE---------------------------------------------------------------
# redditData <- readRDS("2020-09-26_095354-RedditData.rds")

## ----eval=FALSE---------------------------------------------------------------
# actorNetwork <- redditData |> Create("actor") |> AddText(redditData)
# actorGraph <- actorNetwork |> Graph(writeToFile = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# > actorNetwork
# $nodes
# # A tibble: 439 x 2
#       id user
#    <int> <chr>
#  1     1 xxxxxxxxxx
#  2     2 xxxxxxxxxxxxxx
# [snip]
# # … with 429 more rows
# 
# $edges
# # A tibble: 768 x 8
#     from    to subreddit thread_id comment_id comm_id vosonTxt_comment   title
#    <int> <int> <chr>     <chr>        <dbl> <chr>   <chr>                <chr>
#  1     1   439 worldnews elcb9b           1 xxxxxxx "xxxxxxxxxxxxxxxxxxx NA
#  2     2    73 worldnews elcb9b           2 xxxxxxx "xxxxxxxxxxxxxxxxxxx NA
# [snip]
# … with 758 more rows
# 
# attr(,"class")
# [1] "list"       "network"    "actor"      "reddit"     "voson_text"

## ----eval=FALSE---------------------------------------------------------------
# > actorGraph
# IGRAPH 5a5d5b9 DN-- 439 768 --
# + attr: type (g/c), name (v/c), user (v/c), label (v/c), subreddit
# | (e/c), thread_id (e/c), comment_id (e/n), comm_id (e/c),
# | vosonTxt_comment (e/c), title (e/c)
# + edges from 5a5d5b9 (vertex names):
#  [1] 1 ->439 2 ->73  3 ->113 4 ->120 5 ->120 6 ->17  7 ->194 8 ->20  9 ->20
# [10] 10->165 11->165 12->1   13->2   14->3   15->4   16->5   17->6   18->7
# [19] 19->8   20->9   21->10  22->11  23->12  2 ->13  24->3   7 ->18  25->23
# [28] 26->2   3 ->24  27->18  28->1   29->2   18->27  1 ->28  30->2   31->7
# [37] 25->1   32->2   33->31  34->1   2 ->32  35->7   25->34  36->2   7 ->35
# [46] 37->1   38->2   39->7   40->1   41->2   42->7   43->1   2 ->41  44->7
# + ... omitted several edges

## ----eval=FALSE---------------------------------------------------------------
# # set node color of original post to red based on presence of title edge
# # attribute set other node colors to grey
# V(actorGraph)$color <- "grey"
# V(actorGraph)$color[tail_of(
#   actorGraph, which(!is.na(E(actorGraph)$title))
# )] <- "red"
# 
# # get node indexes for the tails of edges that have comments containing
# # words of interest set their node colors to blue
# ind <- tail_of(
#   actorGraph,
#   grep("arson|starting fires",
#        tolower(E(actorGraph)$vosonTxt_comment))
# )
# V(actorGraph)$color[ind] <- "blue"
# 
# # open and write plot to a png file
# png("reddit_actor.png", width = 600, height = 600)
# plot(actorGraph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
# dev.off()

## ----eval=FALSE---------------------------------------------------------------
# activityNetwork <- redditData |> Create("activity") |> AddText(redditData)
# activityGraph <- activityNetwork |> Graph(writeToFile = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# > activityNetwork
# $nodes
# # A tibble: 768 x 10
#    id    thread_id comm_id datetime     ts subreddit user  node_type
#    <chr> <chr>     <chr>   <chr>     <dbl> <chr>     <chr> <chr>
#  1 elcb… elcb9b    xxxxxxx 2020-01… 1.58e9 worldnews xxxx… comment
#  2 elcb… elcb9b    xxxxxxx 2020-01… 1.58e9 worldnews xxxx… comment
# [snip]
# # … with 758 more rows, and 2 more variables: vosonTxt_comment <chr>,
# #   title <chr>
# 
# $edges
# # A tibble: 767 x 3
#    from                       to                       edge_type
#    <chr>                      <chr>                    <chr>
#  1 elcb9b.1                   elcb9b.0                 comment
#  2 elcb9b.4_1_1_1_1_1_1_1_1_1 elcb9b.4_1_1_1_1_1_1_1_1 comment
# [snip]
# # … with 757 more rows
# 
# attr(,"class")
# [1] "list"       "network"    "activity"   "reddit"     "voson_text"
# 

## ----eval=FALSE---------------------------------------------------------------
# > activityGraph
# IGRAPH 09e30ea DN-- 768 767 --
# + attr: type (g/c), name (v/c), thread_id (v/c), comm_id (v/c),
# | datetime (v/c), ts (v/n), subreddit (v/c), user (v/c), node_type
# | (v/c), vosonTxt_comment (v/c), title (v/c), label (v/c), edge_type
# | (e/c)
# + edges from 09e30ea (vertex names):
# [1] elcb9b.1                  ->elcb9b.0
# [2] elcb9b.4_1_1_1_1_1_1_1_1_1->elcb9b.4_1_1_1_1_1_1_1_1
# [3] elcb9b.4_1_1_4_2_1_1_1_1_1->elcb9b.4_1_1_4_2_1_1_1_1
# [4] elcb9b.4_1_1_4_3_1_1_1_3_1->elcb9b.4_1_1_4_3_1_1_1_3
# [5] elcb9b.4_1_1_4_3_1_1_1_3_2->elcb9b.4_1_1_4_3_1_1_1_3
# + ... omitted several edges

## ----eval=FALSE---------------------------------------------------------------
# # set original post node colors to red based on a node type of thread
# # set other node colors to grey
# V(activityGraph)$color <- "grey"
# V(activityGraph)$color[which(V(activityGraph)$node_type == "thread")] <- "red"
# 
# # get node indexes for nodes that have comment attributes containing words of interest
# # set their node colors to blue
# ind <- grep("arson|starting fires", tolower(V(activityGraph)$vosonTxt_comment))
# V(activityGraph)$color[ind] <- "blue"
# 
# # open and write plot to a png file
# png("reddit_activity.png", width = 600, height = 600)
# plot(activityGraph, vertex.label = "", vertex.size = 4, edge.arrow.size = 0.5)
# dev.off()

## ----eval=FALSE---------------------------------------------------------------
# V(g3)$vosonCA_tweetedBushfires <- V(g3)$tweetedBushfires
# write.graph(g3, "g3.graphml", format = "graphml")

