% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddUserData.R, R/wrappers.R
\name{AddUserData}
\alias{AddUserData}
\alias{add_users}
\title{Add columns of user information as node attributes to network dataframes}
\usage{
AddUserData(net, data, ...)

add_users(net, data, ...)
}
\arguments{
\item{net}{A named list of dataframes \code{nodes} and \code{edges} generated by \code{Create}.}

\item{data}{A dataframe generated by \code{Collect}.}

\item{...}{Additional parameters passed to function.}
}
\value{
Network as a named list of three dataframes containing \code{$nodes}, \code{$edges}. Nodes include columns
for additional user profile data and metrics. Referenced users for which no data was found are returned in
\code{missing_users}.
}
\description{
Network is supplemented with additional downloaded social media user information applied as node
attributes.
}
\note{
Only supports twitter actor networks. Refer to \code{\link{AddUserData.actor.twitter}}.
}
