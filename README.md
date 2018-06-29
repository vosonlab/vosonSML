# vosonSML [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/vosonSML)](https://CRAN.R-project.org/package=vosonSML) ![Downloads](https://cranlogs.r-pkg.org/badges/vosonSML) ![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/vosonSML) [![Rdoc](http://www.rdocumentation.org/badges/version/vosonSML)](http://www.rdocumentation.org/packages/vosonSML)

## What does this package do?

`vosonSML` is an R package that provides a suite of tools for collecting and constructing networks from social media data. It provides easy-to-use functions for collecting data across popular platforms (Instagram, Facebook, Twitter, and YouTube) and generating different types of networks for analysis.

`vosonSML` is the `SocialMediaLab` package, renamed. We decided that `SocialMediaLab` was a bit too generic and also we wanted to indicate the connection to the [Virtual Observatory for the Study of Online Networks Lab](http://vosonlab.net), where this package was conceived and created.

vosonSML was created by [Timothy Graham](http://uq.academia.edu/TimGraham) (who is also the maintainer of the package) and [Robert Ackland](https://researchers.anu.edu.au/researchers/ackland-rj).

Contributors:

* [Chung-hong Chan](https://github.com/chainsawriot)

The latest 'official' version of the package can also be found on [CRAN](https://cran.r-project.org/web/packages/vosonSML/index.html).

### Current known issues

If you are getting the error `Error in check_twitter_oauth( )`, please find a [solution here](https://github.com/geoffjentry/twitteR/issues/90).

If you are having trouble getting data from Facebook, it is probably due to a known issue with authentication for the `Rfacebook` package, which [has a solution](https://github.com/vosonlab/vosonSML/issues/28). 

Instagram API access is severely limited if you do not have an authorised app, which is significantly harder to obtain nowadays.

### Special thanks

This package would not be possible without key packages by other authors in the R community, particularly: [igraph](https://github.com/igraph/rigraph), [Rfacebook](https://github.com/pablobarbera/Rfacebook), [instaR](https://github.com/pablobarbera/instaR), [twitteR](https://github.com/geoffjentry/twitteR), [data.table](https://github.com/Rdatatable/data.table), [tm](https://cran.r-project.org/web/packages/tm/index.html), and [httr](https://github.com/hadley/httr).

## Getting started

For detailed information and examples, please refer to the [vosonSML documentation](https://github.com/vosonlab/vosonSML/blob/master/vosonSML.pdf).

The [vosonSML page on the VOSON website](http://vosonlab.net/vosonSML) also has several "how to" guides, including an "Absolute Beginners Guide to vosonSML" tutorial aimed at people with little or no programming experience.

## Using Magrittr's pipe interface

The process of authentication, data collection and creating social network can be expressed with the 3 verb functions: *Authenticate*, *Collect* and *Create*. The following are some of the examples from the package documentation expressed with the pipe interface.

```{r}
require(magrittr)
# Authenticate with youtube, Collect data from youtube and Create an actor network
Authenticate("youtube", apiKey= apiKey) %>% Collect(videoIDs = videoIDs) %>% Create("Actor")

# Authenticate with facebook, archive the API credential, Collect data about Starwars Page and Create a bimodal network
# You can use facebook, FaCebooK or Facebook in the datasource field
Authenticate("Facebook", appID = appID, appSecret = appSecret) %>% SaveCredential("FBCredential.RDS") %>% Collect(pageName="StarWars", rangeFrom="2015-05-01",rangeTo="2015-06-03") %>% Create("Bimodal")

# Authenticate with Twitter, Collect data about #auspol and Create a semantic network
Authenticate("twitter", apiKey=myapikey, apiSecret=myapisecret,accessToken=myaccesstoken, accessTokenSecret=myaccesstokensecret) %>% Collect(searchTerm="#auspol", numTweets=150) %>% Create("Semantic")

# Create Instagram Ego Network
myUsernames <- 
Authenticate("instagram", appID = myAppId, appSecret = myAppSecret) %>% Collect(ego = TRUE, username = c("adam_kinzinger","senatorreid")) %>% Create
```

## Example networks

The following networks were created in vosonSML and visualised using the [Gephi software](http://gephi.github.io/).

##### Facebook bimodal network (Star Wars page)

This network visualises two weeks of activity on the [Star Wars Facebook page](https://www.facebook.com/StarWarsAUNZ/?brand_redir=169299103121699). Nodes (vertices) represent either users or posts, and node ties (edges) represent either 'likes' or comments. Nodes are sized by out-degree and coloured by modularity cluster.

<img src="http://vosonlab.net/papers/ACSPRIWinter2015/Facebook_bimodal_network_socialmedialab_Star_Wars.png" alt="Facebook bimodal network created with vosonSML" width="600" height="600"/>

##### Instagram ego network

This network visualises the social network of one user (the 'ego' node) on Instagram. The degree of the network is "2", meaning that it shows *ego + alters* ("followers of ego") and *ego + alters* + *alters of alters of ego* "followers of followers of ego". 'Follows' data are also collected, so this network also shows "users who ego follows" and "users who followers of ego follow".

<img src="http://vosonlab.net/papers/ACSPRIWinter2015/Instagram_ego_network_socialmedialab_example.png" alt="Facebook bimodal network created with vosonSML" width="600" height="600"/>

