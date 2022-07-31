#' @title Create web activity network
#'
#' @description Creates a web page activity network from pages. Nodes are web pages.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"web"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param lcase Logical. Convert urls and page names to lowercase.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a web activity network graph
#' net_activity <- data_collect |> Create("activity")
#'
#' # network
#' # net_activity$nodes
#' # net_activity$edges
#' }
#'
#' @export
Create.activity.web <-
  function(datasource, type, lcase = TRUE, verbose = TRUE, ...) {
    msg("Generating web activity network...\n")

    edges <- datasource |>
      dplyr::mutate(from = ifelse({{ lcase }}, tolower(.data$page), .data$page),
                    to = ifelse({{ lcase }}, tolower(.data$url), .data$url)) |>
      # dplyr::select(.data$from, .data$to) |>
      dplyr::mutate(link_id = as.character(dplyr::row_number()))

    nodes <- tibble::tibble(page = c(edges$from, edges$to)) |>
      dplyr::distinct(.data$page) |>
      dplyr::mutate(id = as.character(dplyr::row_number())) # |>
      # dplyr::relocate(.data$id)

    net <- list("nodes" = nodes, "edges" = edges)
    class(net) <- append(class(net), c("network", "activity", "web"))
    msg("Done.\n")

    net
  }
