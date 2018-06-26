#' Collection and network analysis of social media data
#'
#' The goal of the vosonSML package is to provide a suite of easy-to-use
#' tools for collecting data from social media sources (Instagram, Facebook,
#' Twitter, and Youtube) and generating different types of networks suited to
#' Social Network Analysis (SNA) and text analytics. It offers tools to create
#' unimodal, multimodal, semantic, and dynamic networks. It draws on excellent
#' packages such as \pkg{twitteR}, \pkg{instaR}, \pkg{Rfacebook}, and
#' \pkg{igraph} in order to provide an integrated 'work flow' for collecting
#' different types of social media data and creating different types of
#' networks out of these data. Creating networks from social media data is
#' often non-trivial and time consuming. This package simplifies such tasks so
#' users can focus on analysis.
#'
#' vosonSML uses a straightforward S3 class system. Data collected with
#' this package produces \code{data.table} objects (extension of class
#' \code{data.frame}), which are assigned the class \code{dataSource}.
#' Additionally, \code{dataSource} objects are assigned a class identifying the
#' source of data, e.g. \code{facebook} or \code{youtube}. In this way,
#' \code{dataSource} objects are fast, easy to work with, and can be used as
#' input to easily construct different types of networks. For example, the
#' function \code{\link{Collect}} can be used to collect Twitter data, which is
#' then 'piped' to the \code{\link{Create}} function, resulting in a network
#' (an igraph object) that is ready for analysis.
#'
#' @name vosonSML-package
#' @aliases vosonSML-package vosonSML
#' @docType package
#' @author Timothy Graham & Robert Ackland, with contribution from Chung-hong Chan
#'
#' Maintainer: Timothy Graham <timothy.graham@@anu.edu.au>
#' @import tm
#' @import RCurl
#' @import bitops
#' @import rjson
#' @import data.table
#' @import httpuv
#' @import methods
#' @import httr
#' @importFrom Hmisc escapeRegex
#' @importFrom igraph delete.vertices graph.data.frame simplify write.graph V 'V<-'
#' @importFrom Rfacebook fbOAuth getPost getPage getUsers
#' @importFrom instaR getComments getLikes instaOAuth searchInstagram getUser getFollowers getFollows
#' @importFrom plyr ldply
#' @importFrom twitteR lookupUsers searchTwitter setup_twitter_oauth twListToDF
#' @importFrom stringr str_extract str_replace_all str_match_all
#' @importFrom stats 'na.omit'
#' @importFrom utils "flush.console" head "install.packages" "read.table" "write.csv" "read.csv"
NULL
