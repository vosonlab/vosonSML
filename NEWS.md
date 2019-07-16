# vosonSML 0.27.2

## Minor Changes
- Added twitter interactive web authorization of an app as provided by `rtweet::create_token`.
  Method is used when only twitter app name and consumer keys are passed to `Authenticate.twitter`
  as parameters. e.g `Authenticate("twitter", appName = "An App", apiKey = "xxxxxxxxxxxx", 
  apiSecret = "xxxxxxxxxxxx")`. A browser tab will open asking the user to authorize the app to
  their twitter account to complete authentication. This is using twitters 
  `Application-user authentication: OAuth 1a (access token for user context)` method.
- It is suspected that Reddit is rate-limiting some generic R UA strings. So a User-Agent string is
  now set for underlaying R Collect functions (e.g `file`) via the `HTTPUserAgent` option.  It is 
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
