#' @title Add columns of user information as node attributes to network dataframes
#'
#' @description Network is supplemented with additional downloaded social media user information applied as node
#' attributes.
#'
#' @note Only supports twitter actor networks at this time. Refer to \code{\link{AddUserData.actor.twitter}}.
#'
#' @param net A named list of dataframes \code{nodes} and \code{edges} generated by \code{Create}.
#' @param data A dataframe generated by \code{Collect}.
#' @param ... Additional parameters passed to function.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}
#' including columns for additional user data.
#'
#' @aliases AddUserData
#' @name AddUserData
#' @export
AddUserData <- function(net, data, ...) {
  # searches the class list of net for matching method
  UseMethod("AddUserData", net)
}

#' @noRd
#' @export
AddUserData.default <- function(net, ...) {
  stop("Unknown network type passed to AddUserData.", call. = FALSE)
}

#' @noRd
#' @method AddUserData actor
#' @export
AddUserData.actor <- function(net, ...) {
  UseMethod("AddUserData.actor", net)
}

#' @noRd
#' @export
AddUserData.actor.default <- function(net, ...) {
  stop("Unknown social media type passed to AddUserData.", call. = FALSE)
}

#' @title Supplement twitter actor network by adding user profile attributes to nodes
#'
#' @description Network is supplemented with additional downloaded user information applied as actor node attributes.
#'
#' @note Using the standard twitter API this function is limited to collecting profiles of 90000 users per 15 mins
#' before hitting the rate limit. It does not wait and retry upon hitting rate limit.
#'
#' @param net A named list of dataframes \code{nodes} and \code{edges} generated by \code{Create}.
#' @param data A dataframe generated by \code{Collect}.
#' @param lookupUsers Logical. Lookup user profile information using the twitter API for any users data missing from
#' the collect data set. For example fetches profile information for users that became nodes during network creation
#' because they were mentioned in a tweet but did not author any tweets themselves. Default is \code{FALSE}.
#' @param twitterAuth A twitter authentication object from \code{Authenticate}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @examples
#' \dontrun{
#' # add user info to a twitter actor network
#' actor_net <- twitter_data %>%
#'   Create("actor") %>%
#'   AddUserData(twitter_data,
#'               lookupUsers = TRUE,
#'               twitterAuth = twitter_auth)
#'
#' # network
#' names(actor_net)
#' # "nodes", "edges"
#' names(actor_net$nodes)
#' # "name", "screen_name", "display_name", "location"
#' # "description", "url", "protected", "followers_count"
#' # "friends_count", "listed_count", "statuses_count", "favourites_count"
#' # "account_created_at", "verified", "profile_url", "profile_expanded_url"
#' # "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url"
#' }
#'
#' @return Network as a named list of two dataframes containing \code{$nodes}, \code{$edges} and
#' \code{$users}. Nodes include columns for additional user data.
#'
#' @aliases AddUserData.actor.twitter
#' @name AddUserData.actor.twitter
#' @export
AddUserData.actor.twitter <-
  function(net,
           data,
           lookupUsers = FALSE,
           twitterAuth = NULL,
           verbose = TRUE,
           ...) {
    rlang::check_installed("rtweet", "for AddUserData.actor.twitter")
    stop_req_pkgs(c("rtweet"), "AddUserData.actor.twitter")

    cat("Adding user profile data to network...")
    if (verbose) {
      cat("\n")
    }

    class(data) <- rm_collect_cls(class(data))

    dfUsers <- net$nodes

    dfUsers %<>% dplyr::mutate_all(as.character) # changes all col types to character

    df_users_info <-
      rtweet::users_data(data) %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE)

    df_users_info %<>% dplyr::mutate_all(as.character) # changes all col types to character

    df_missing_users <-
      dplyr::anti_join(dfUsers, df_users_info, by = "user_id") %>%
      dplyr::distinct(.data$user_id, .keep_all = TRUE)

    df_missing_users_info <- NULL
    if (lookupUsers) {
      if (is.null(twitterAuth)) {
        stop(
          paste0(
            "Please supply twitter authentication object to look up missing users profile info or set ",
            "lookupUsers = FALSE.\n"
          ),
          call. = FALSE
        )
      } else {
        if (verbose) {
          cat(paste0(
            "Fetching user information for ",
            nrow(df_missing_users),
            " users.\n"
          ))
        }

        # 90000 users per 15 mins with unused rate limit
        df_lookup_data <-
          rtweet::lookup_users(df_missing_users$user_id,
                               parse = TRUE,
                               token = twitterAuth$auth)
        df_missing_users_info <- rtweet::users_data(df_lookup_data)
        if (verbose) {
          cat(paste0(
            "User information collected for ",
            nrow(df_missing_users_info),
            " users.\n"
          ))
        }

        net$users <- tibble::as_tibble(df_missing_users_info)

        if (nrow(df_missing_users) != nrow(df_missing_users_info)) {
          if (verbose) {
            cat(
              "Collected user records does not match the number requested. Adding incomplete records back in.\n"
            )
          }
          df_not_collected <-
            dplyr::anti_join(df_missing_users, df_missing_users_info, by = "user_id")
          df_missing_users_info <-
            dplyr::bind_rows(df_missing_users_info, df_not_collected)
        }
      }
    } else {
      if (verbose) {
        cat("No additional users information fetched.\n")
      }
    }

    if (!is.null(df_missing_users_info)) {
      df_users_info_all <- rbind(df_users_info, df_missing_users_info)
    } else {
      df_users_info_all <-
        dplyr::bind_rows(df_users_info, df_missing_users)
    }

    df_users_info_all %<>% dplyr::rename("display_name" = .data$name) # , "name" = .data$user_id

    # numeric value column names in rtweet collected data end with "count"
    df_users_info_all %<>% dplyr::mutate_at(dplyr::vars(dplyr::ends_with("count")),
                                            # funs(ifelse(is.na(.data$.), as.integer(0), as.integer(.data$.)))
                                            list(function(x)
                                              ifelse(is.na(x), as.integer(0), as.integer(x))))

    net$nodes <- df_users_info_all

    class(net) <- union(class(net), c("vosonuser"))
    cat("Done.\n")

    net
  }
