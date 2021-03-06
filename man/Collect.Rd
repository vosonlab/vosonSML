% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Collect.R
\name{Collect}
\alias{Collect}
\title{Collect data from social media for generating networks}
\usage{
Collect(credential, ...)
}
\arguments{
\item{credential}{A \code{credential} object generated from \code{Authenticate}.}

\item{...}{Optional parameters to pass to functions providied by supporting R packages that are used for social media
API collection.}
}
\description{
This function collects data from social media and structures it into a dataframe that can be used for
creating networks for further analysis. \code{Collect} is the second step of the \code{\link{Authenticate}},
\code{Collect}, and \code{\link{Create}} workflow.

Refer to \code{\link{Collect.twitter}}, \code{\link{Collect.youtube}} and
\code{\link{Collect.reddit}} for parameters and usage.
}
