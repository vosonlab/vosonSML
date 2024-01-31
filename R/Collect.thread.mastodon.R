#' @title Collect posts data from mastodon threads
#'
#' @description Collects public posts for one or more specified mastodon conversation threads and structures
#'   the data into a dataframe with the class names \code{"datasource"} and \code{"mastodon"}.
#'
#' @param credential A \code{credential} object generated from \code{Authenticate} with class name \code{"mastodon"}.
#' @param endpoint API endpoint.
#' @param threadUrls Character vector. Mastodon thread post urls to collect data from.
#' @param writeToFile Logical. Write collected data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information about the data collection. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return A \code{tibble} object with class names \code{"datasource"} and \code{"mastodon"}.
#'
#' @examples
#' \dontrun{
#' # post urls to collect threads from
#' threadUrls <- c("https://mastodon.social/@xxxxxx/xxxxxxxxx")
#'
#' mastodonData <- Authenticate("mastodon") |>
#'   Collect(threadUrls = threadUrls, writeToFile = TRUE)
#' }
#'
#' @export
Collect.thread.mastodon <-
  function(credential,
           endpoint,
           threadUrls,
           writeToFile = FALSE,
           verbose = FALSE,
           ...) {

    prompt_and_stop("rtoot", "Collect.thread.mastodon")
    
    msg("Collecting post threads for mastodon urls...\n")

    if (missing(threadUrls)) {
      stop("Please provide a vector of one or more mastodon thread urls.", call. = FALSE)
    }

    invisible(check_chr(threadUrls, param = "threadUrls"))
    
    data <- purrr::map(threadUrls, ~ get_thread(.x, ...), .progress = ifelse(verbose, "collecting threads", FALSE))
    threads_df <- do.call(import_rtoot_, unlist(data, recursive = FALSE, use.names = FALSE))
    
    n_posts <- check_df_n(threads_df$posts)
    
    # summary
    if (n_posts > 0) {
      
      first_post <- threads_df$posts |> dplyr::slice_head(n = 1) |> dplyr::mutate(post = "Latest Obs")
      last_post <- threads_df$posts |> dplyr::slice_tail(n = 1) |> dplyr::mutate(post = "Earliest Obs")
      
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
    
    if (writeToFile) write_output_file(threads_df, "rds", "MastodonData", verbose = verbose, log = meta_log)
    
    msg("Done.\n")
    
    threads_df
  }

# collect a mastodon post by url and its thread
get_thread <- function(url, ...) {
  post_server <- httr::parse_url(url)$hostname
  post_id <- basename(httr::parse_url(url)$path)
  
  # get post and its thread
  post <- rtoot::get_status(post_id, instance = post_server, ...)
  thread <- rtoot::get_context(post_id, instance = post_server, ...)
  
  list(post = post, thread = thread)
}
