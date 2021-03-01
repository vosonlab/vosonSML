#' @title Collecting Social Media Data and Generating Networks for Analysis
#'
#' @description The goal of the \pkg{vosonSML} package is to provide a suite of easy-to-use tools for collecting data
#' from social media and generating different types of networks suited to Social Network Analysis (SNA). It offers
#' tools to create unimodal, multimodal, and semantic networks. Excellent packages such as \pkg{rtweet},
#' \pkg{RedditExtractoR}, \pkg{dplyr} and \pkg{igraph} were drawn on to provide an integrated work flow for creating
#' different types of networks out of social media data. Creating networks from online social media is often non-trivial
#' and time consuming. This package simplifies such tasks so users can focus on analysis.
#'
#' \pkg{vosonSML} uses a straightforward S3 class system. Data collected with this package produces \code{data.frame}
#' inheritable objects that are assigned the class \code{"datasource"}. Additionally, \code{datasource} objects are
#' attributed a class identifying the source of data, such as \code{"twitter"} or \code{"youtube"}. In this way
#' \code{datasource} objects are fast, easy to work with, and can be used as input to easily construct different kinds
#' of networks. For example, the function \code{Collect} can be used to collect twitter data, which is then passed to
#' the \code{Create} function resulting in a twitter network that is ready for analysis.
#'
#' @name vosonSML-package
#' @aliases vosonSML-package vosonSML
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom data.table "data.table" setkey ":=" fcase fifelse
#' @importFrom dplyr anti_join arrange bind_rows case_when coalesce contains count
#' distinct ends_with filter funs group_by if_else left_join mutate mutate_all mutate_at
#' n rename row_number rowwise select slice starts_with summarise ungroup vars
#' @importFrom igraph graph_from_data_frame set_graph_attr V "V<-" write_graph
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as_datetime "is.POSIXt"
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom methods new
#' @importFrom purrr flatten_chr map_dfr map_if transpose
#' @importFrom rlang ".data"
#' @importFrom stats "na.omit" setNames runif
#' @importFrom stringr str_detect str_extract str_match_all str_replace str_replace_all
#' @importFrom textutils HTMLdecode
#' @importFrom tibble as_tibble tibble
#' @importFrom utils "capture.output" "flush.console" "read.table" tail
NULL
