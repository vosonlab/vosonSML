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
