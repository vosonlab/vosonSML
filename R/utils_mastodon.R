# add any referenced nodes in edge list that are absent from node list
add_absent_nodes <- function(nodes, edges) {
  # find any absent nodes
  x <- c(edges |> dplyr::pull(.data$from), edges |> dplyr::pull(.data$to)) |> unique()
  y <- nodes |> dplyr::pull(.data$id)
  absent_nodes <- tibble::tibble(id = setdiff(x, y), absent = TRUE)
  
  # add nodes that are absent
  nodes <- nodes |> dplyr::mutate(absent = FALSE) |> dplyr::bind_rows(absent_nodes)
  
  nodes
}
