# add any referenced nodes in edge list that are absent from node list
add_absent_nodes <- function(nodes, edges, id = "post.id") {
  # find any absent nodes
  x <- c(edges |> dplyr::pull("from"), edges |> dplyr::pull("to")) |> unique()
  y <- nodes |> dplyr::pull({{ id }})

  absent_nodes <- tibble::tibble("{id}" := setdiff(x, y), absent = TRUE) |>
    tidyr::drop_na({{ id }})
  
  # add nodes that are absent
  nodes <- nodes |> dplyr::mutate(absent = FALSE) |> dplyr::bind_rows(absent_nodes)
  
  nodes
}
