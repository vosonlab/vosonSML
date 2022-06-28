#' @title Create web activity network
#'
#' @description Creates a web page activity network from pages. Nodes are web pages.
#'
#' @param datasource Collected social media data with \code{"datasource"} and \code{"web"} class names.
#' @param type Character string. Type of network to be created, set to \code{"activity"}.
#' @param verbose Logical. Output additional information about the network creation. Default is \code{TRUE}.
#' @param ... Additional parameters passed to function. Not used in this method.
#'
#' @return Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}.
#'
#' @examples
#' \dontrun{
#' # create a web activity network graph
#' activityNetwork <- webData |> Create("activity")
#'
#' # network
#' # activityNetwork$nodes
#' # activityNetwork$edges
#' }
#'
#' @export
Create.activity.web <-
  function(datasource, type, verbose = TRUE, ...) {
    cat("Generating web activity network...")
    if (verbose) {
      cat("\n")
    }

    data <- datasource |>
      dplyr::mutate(from = tolower(.data$page),
                    to = tolower(.data$url)) |>
      dplyr::group_by(.data$from, .data$to) |>
      dplyr::summarise(weight = sum(.data$n), .groups = "drop") |>
      dplyr::select(.data$from, .data$to, .data$weight)

    nodes <- c(data$from, data$to)
    nodes <- data.frame(id = unique(nodes))
    nodes <-
      nodes |> dplyr::arrange(.data$id) |> dplyr::mutate(link_id = dplyr::row_number())

    edges <-
      data |> dplyr::select(.data$from, .data$to, .data$weight)

    func_output <- list("nodes" = nodes,
                        "edges" = edges)

    class(func_output) <-
      append(class(func_output), c("network", "activity", "web"))
    cat("Done.\n")

    return(func_output)
  }
