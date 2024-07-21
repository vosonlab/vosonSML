#' @title Create mastodon activity network
#'
#' @description Creates a mastodon activity network from collected posts. Nodes are posts and directed edges represent
#'   the relationship of posts to one another.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"mastodon"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param subtype Character string. Subtype of activity network to be created. Can be set to \code{"tag"}. Default is
#'   \code{NULL}.
#' @param ... Additional parameters passed to function. Not used in this method.
#' @param writeToFile Logical. Write data to file. Default is \code{FALSE}.
#' @param verbose Logical. Output additional information. Default is \code{TRUE}.
#' 
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a mastodon activity network
#' activity_net <- mastodon_data |> Create("activity")
#'
#' # create a mastodon tag relations network
#' activity_net <- mastodon_data |> Create("activity", "tag")
#' }
#'
#' @export
Create.activity.mastodon <-
  function(datasource,
           type,
           subtype = NULL,
           ...,
           writeToFile = FALSE,
           verbose = TRUE) {
    
    msg("Generating mastodon activity network...\n")
    
    if (check_df_n(datasource$posts) < 1) stop("Datasource invalid or empty.", call. = FALSE)
    
    check_chr(subtype, param = "subtype", accept = "tag", null.ok = TRUE)
    
    if (!"tags" %in% colnames(datasource$posts)) {
      if (is.null(subtype)) {
        datasource$posts$tags <- NA
      } else {
        stop("Datasource does not have a tags column.", call. = FALSE)
      }
    }
    
    # tag network
    if (!is.null(subtype)) {
      tags <- datasource$posts |> dplyr::select(post.id = "id", "tags")
      
      # replace null value lists before unnest
      tags <- tags |> dplyr::mutate(tags = replace(.data$tags, lengths(.data$tags) == 0, NA))
      tags <- tags |> tidyr::unnest(cols = "tags") |> dplyr::rename(tag = "name")
      
      edges <- tags |>
        get_tag_relations(id = "post.id") |>
        dplyr::rename(from = "tag.x", to = "tag.y") |>
        dplyr::relocate("from", "to") |>
        dplyr::mutate(edge_type = "colocated")
      
      nodes <-
        dplyr::left_join(
          edges |> dplyr::select("tag" = "from"),
          edges |> dplyr::select("tag" = "to"), by = "tag") |>
        dplyr::distinct()
      
      net <- list("nodes" = nodes, "edges" = edges)
      class(net) <- append(c("network", "activity", "tag", "mastodon"), class(net))
    } else {
      edges <- datasource$posts |>
        dplyr::select(from = "id", to = "in_reply_to_id", "created_at") |>
        dplyr::filter(!is.na(.data$to)) |>
        dplyr::mutate(edge_type = "reply")
      
      nodes <- datasource$posts |>
        dplyr::select(
          post.id = "id",
          post.created_at = "created_at",
          post.visibility = "visibility",
          "account",
          "tags",
          post.reblogs_count = "reblogs_count",
          post.favourites_count = "favourites_count",
          post.replies_count = "replies_count",
          post.url = "url") |>
        tidyr::hoist(
          "account",
          account.id = "id",
          account.username = "username",
          account.acct = "acct",
          account.displayname = "display_name",
          user.avatar = "avatar") |>
        tidyr::hoist(
          "tags",
          post.tags = list("name"),
          post.tags.urls = list("url")) |>
        dplyr::select(-"account") |>
        dplyr::mutate(node_type = "post")
      
      # add referenced posts absent from nodes
      nodes <- add_absent_nodes(nodes, edges)
      
      net <- list("nodes" = nodes, "edges" = edges)
      class(net) <- append(c("network", "activity", "mastodon"), class(net))
    }
    
    if (writeToFile) write_output_file(net, "rds", "MastodonActivityNet", verbose = verbose)

    msg("Done.\n")
    
    net
  }

# tags colocated within posts
get_tag_relations <- function(tags_table, id) {
  tags_table |>
    dplyr::select({{ id }}, "tag") |>
    dplyr::left_join(
      tags_table |> dplyr::select({{ id }}, "tag"), by = {{ id }}, relationship = "many-to-many"
    ) |>
    dplyr::group_by(.data[[id]]) |>
    dplyr::filter(.data$tag.x != .data$tag.y) |>
    dplyr::ungroup()
}
