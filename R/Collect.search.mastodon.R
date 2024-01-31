#' @title Collect post data from mastodon search
#'
#' @description This function collects posts based on search terms and structures the data into a dataframe with
#'   the class names \code{"datasource"} and \code{"mastodon"}.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"mastodon"}.
#' @param endpoint API endpoint.
#' @param hashtag Character string. Specifies a mastodon query to search on e.g #hashtag. Set to \code{NULL} for unfiltered public posts. Default is \code{NULL}.
#' @param instance Character string. Server to collect posts from. Default is \code{NULL}.
#' @param local Logical. Search the local server or global timeline. \code{FALSE}.
#' @param numPosts Numeric. Specifies how many tweets to be collected. Default is \code{100}.
#' @param anonymous Logical. Collect public posts without authenticating. Default is \code{TRUE}.
#' @param retryOnRateLimit Logical. When the API rate-limit is reached should the collection wait and resume when it resets. Default is \code{TRUE}.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{FALSE}.
#' @inheritDotParams rtoot::get_timeline_hashtag -hashtag -local -max_id -since_id -limit -instance -token -anonymous -parse -retryonratelimit -verbose
#'
#' @return A tibble object with class names \code{"datasource"} and \code{"mastodon"}.
#'
#' @export
Collect.search.mastodon <-
  function(credential,
           endpoint,
           hashtag = NULL,
           instance = NULL,
           local = FALSE,
           numPosts = 100,
           anonymous = TRUE,
           retryOnRateLimit = TRUE,
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {

    prompt_and_stop("rtoot", "Collect.search.mastodon")
    
    msg("Collecting timeline posts...\n")

    auth_token <- credential$auth

    if (!is.null(auth_token$instance)) {
      instance <- auth_token$instance
    } else {
      instance <- check_chr(instance, param = "instance")
    }
    
    hashtag <- check_chr(hashtag, param = "hashtag", null.ok = TRUE)
    if (!is.null(hashtag) && hashtag != "") {
      msg(paste0("Hashtag: ", hashtag, "\n"))
    }

    msg(paste0("Requested ", numPosts, " posts\n"))

    search_params <- list()
    search_params[["token"]] <- auth_token
    search_params["instance"] <- instance
    
    if (!is.null(hashtag)) search_params["hashtag"] <- hashtag
    
    search_params["local"] <- local
    search_params["limit"] <- numPosts
    search_params["anonymous"] <- anonymous
    search_params["retryonratelimit"] <- retryOnRateLimit
    search_params["verbose"] <- verbose

    # additional mastodon api params
    dots <- substitute(...())
    search_params <- append(search_params, dots)

    if (is.null(hashtag)) {
      df_posts <- do.call(rtoot::get_timeline_public, search_params)
    } else {
      df_posts <- do.call(rtoot::get_timeline_hashtag, search_params)
    }
    
    if (nrow(df_posts)) {
      df_posts <- df_posts |> import_rtoot_()
      n_posts <- check_df_n(df_posts$posts)
    } else {
      n_posts <- 0
    }
    
    # summary
    if (n_posts > 0) {

      first_post <- df_posts$posts |> dplyr::slice_head(n = 1) |> dplyr::mutate(post = "Latest Obs")
      last_post <- df_posts$posts |> dplyr::slice_tail(n = 1) |> dplyr::mutate(post = "Earliest Obs")

      df_summary <- dplyr::bind_rows(first_post, last_post) |>
        dplyr::mutate(created = as.character(.data$created_at)) |>
        dplyr::select(.data$id, .data$created)

      msg("\n")
      msg(print_summary(df_summary))
    }
    msg(paste0("Collected ", n_posts, " posts.\n"))

    meta_log <- c(
      collect_log, "",
      ifelse(n_posts > 0, print_summary(df_summary), ""),
      "", paste0(format(Sys.time(), "%a %b %d %X %Y"))
    )
    
    if (writeToFile) write_output_file(df_posts, "rds", "MastodonData", verbose = verbose, log = meta_log)

    msg("Done.\n")

    df_posts
  }
