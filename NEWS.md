# vosonSML 0.29.5

## Minor Changes
- Reworked the `RedditExtractoR` collection and parsing methods.
- Removed the `tictoc` package from dependency imports to suggested packages.
- Added some checks for whether the `rtweet` package is installed.
- Removed the `RedditExtractoR` package from imports.
- HTML decoded tweet text during network creation to replace '&', '<', and '>' HTML codes.
- Added node type attribute to twomode networks.

# vosonSML 0.29.4

## Minor Changes
- Renamed `bimodal` networks to `twomode`.

# vosonSML 0.29.3

## Minor Changes
- Added output messages from supplemental functions such as `AddText()` and `Graph()`. Also improved
  consistency of output messages from `Collect` and `Create` functions.

## Bug Fixes
- Added a fix `reddit` gsub locale error https://github.com/vosonlab/vosonSML/issues/21.
- Changed `bimodal` network hashtags to lowercase as filter terms when entered are converted to
  lowercase.
- Fixed errors thrown when removing terms from `bimodal` and `semantic` networks.
- Removed a duplicate `GetVideoData()` function call in `AddVideoData`.
- Fixed data type errors in `AddText` functions related to strict typing by `dplyr::if_else` function.

# vosonSML 0.29.2

## Minor Changes
- A feature was added to the youtube actor `AddText` function to redirect edges towards actors based
  on the presence of a `screen name` or `@screen name` that may be found at the beginning of
  a reply comment. Typically reply comments are directed towards a top-level comment, this
  instead captures when reply comments are directed to other commenters in the thread.

# vosonSML 0.29.1

## Minor Changes
- Changed youtube `actor` network identifiers to be their unique `Channel ID` instead of their
  `screen names`.
- Created the `AddVideoData` function to add collected video data to the youtube `actor` network. The
  main purpose of this function is to replace video identifiers with the `Channel ID` of the video
  publisher (actor) instead. To get the `Channel ID` of video publishers an additional API lookup for
  the videos in the network is required. Additional columns such as video `Title`, `Description` and
  `Published` time are also added to the network `$edges` dataframe as well as returned in their own
  dataframe called `$videos`.

# vosonSML 0.29.0

## Major Changes
- Created the `AddText` function to add collected text data to networks. This feature applies
  to `activity` and `actor` networks and will typically add a node attribute to activity networks
  and an edge attribute to actor networks. For example, this function will add the column
  `vosonTxt_tweets` containing tweet text to `$nodes` if passed an activity network, and to
  `$edges` if passed an actor network.
- Generation of `igraph` graph objects and subsequent writing to file has been removed from the
  `Create` function and placed in a new function `Graph`. This change abstracts the graph creation
  and makes it optional, but also allows supplemental network steps such as `AddText` to be
  performed prior to creating the final igraph object.

## Minor Changes
- Removed `writeToFile` parameter from `Create` functions and added it to `Graph`.
- Removed `weightEdges`, `textData` and `cleanText` parameters from `Create.actor.reddit`.
  `cleanText` is now a parameter of `AddText.activity.reddit` and `AddText.actor.reddit`.
- Replaced `AddTwitterUserData` with `AddUserData` function that works similarly to `AddText`.
  This function currently only applies to twitter actor networks and will add, or download
  add if missing, user profile information to actors as node attributes.

# vosonSML 0.28.1

## Minor Changes
- Added `activity` network type for reddit. In the reddit activity network nodes are the
  thread posts and comments, edges represent where comments are directed in the threads.
- Added github dev version badge to README.

# vosonSML 0.28.0

## Major Changes
- Added new `activity` network type for twitter and youtube `Create` function. In this network
  nodes are the items collected such as tweets returned from a twitter search and comments
  posted to youtube videos. Edges represent the platform relationship between the tweets or
  comments.

# vosonSML 0.27.3

## Minor Changes
- Added a new twitter actor network edge type `self-loop`. This aims to facilitate the later addition
  of tweet text to the network graph for user tweets that have no ties to other users.

# vosonSML 0.27.2

## Minor Changes
- Added twitter interactive web authorization of an app as provided by `rtweet::create_token`.
  Method is used when only twitter app name and consumer keys are passed to `Authenticate.twitter`
  as parameters. e.g `Authenticate("twitter", appName = "An App", apiKey = "xxxxxxxxxxxx",
  apiSecret = "xxxxxxxxxxxx")`. A browser tab will open asking the user to authorize the app to
  their twitter account to complete authentication. This is using twitters
  `Application-user authentication: OAuth 1a (access token for user context)` method.
- It is suspected that Reddit is rate-limiting some generic R UA strings. So a User-Agent string is
  now set for underlaying R Collect functions (e.g `file`) via the `HTTPUserAgent` option. It is
  temporarily set to package name and current version number for Collect e.g
  `vosonSML v.0.27.2 (R Package)`.
- Removed hex sticker (and favicons for pkgdown site).

# vosonSML 0.27.1

## Bug Fixes
- Fixed a bug in `Create.semantic.twitter` in which a sum operation calculating edge
  weights would set `NA` values for all edges due to `NA` values present in the hashtag fields.
  This occurs when there are tweets with no hashtags in the twitter collection and is now
  checked.
- Some UTF encoding issues in `Create.semantic.twitter` were also fixed.

## Minor Changes
- Added '#' to hashtags and '@' to mentions in twitter semantic network to differentiate between
  hashtags, mentions and common terms.

# vosonSML 0.27.0

## Bug Fixes
- Fixed a bug in `Collect.twitter` in which any additional `twitter API` parameters
  e.g `lang` or `until` were not being passed properly to `rtweet::search_tweets`. This
  resulted in the additional parameters being ignored.

## Major Changes
- Removed the `SaveCredential` and `LoadCredential` functions, as well as the `useCachedToken`
  parameter for `Authenticate.twitter`. These were simply calling the `saveRDS` and `readRDS`
  functions and not performing any additional processing. Using `saveRDS` and `readRDS` directly
  to save and load an `Authenticate` credential object to file is simpler.
- Changed the way that the `cleanText` parameter works in `Create.actor.reddit` so that it is
  more permissive. Addresses encoding issues with apostrophes and pound symbols and removes
  unicode characters not permitted by the XML 1.0 standard as used in `graphml` files. This is
  best effort and does not resolve all `reddit` text encoding issues.

## Minor Changes
- Added `Collect.twitter` summary information that includes the earliest (min) and latest (max)
  tweet `status_id` collected with timestamp. The `status_id` values can be used to frame
  subsequent collections as `since_id` or `max_id` parameter values. If the `until` date
  parameter was used the timestamp can also be used as a quick confirmation.
- Added elapsed time output to the `Collect` method.

# vosonSML 0.26.3

## Bug Fixes
- Fixed bugs in `Create.actor.reddit` that were incorrectly creating edges between
  top-level commentors and thread authors from different threads. These bugs were only
  observable in when collecting multiple reddit threads.

## Minor Changes
- Improved output for `reddit` collection. Removed the progress bar and added a table
  of results summarising the number of comments collected for each thread.
- Added to `twitter` collection output the users `twitter API` reset time.

# vosonSML 0.26.2

## Bug Fixes
- Fixed a bug in `Create.actor.twitter` and `Create.bimodal.twitter` in which the vertices
  dataframe provided to the `graph_from_data_frame` function as a contained duplicate names
  raising an error.

## Major Changes
- Revised and updated `roxygen` documentation and examples for all package functions.
- Updated all `Authenticate`, `Collect` and `Create` S3 methods to implement function routing
  based on object class names.

## Minor Changes
- Created a `pkgdown` web site for github hosted package documentation.
- Created a new hex sticker logo.

# vosonSML 0.25.0

## Major Changes
- Replaced the `twitteR` twitter collection implementation with the `rtweet` package.
- A users `twitter` authentication token can now be cached in the `.twitter_oauth_token` file and
  used for subsequent `twitter API` requests without re-authentication. A new authentication
  token can be cached by deleting this file and using the re-using the parameter
  `useCachedToken = TRUE`.
