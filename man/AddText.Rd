% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddText.R
\name{AddText}
\alias{AddText}
\title{Add columns containing text data to network dataframes}
\usage{
AddText(net, data, ...)
}
\arguments{
\item{net}{A named list of dataframes \code{nodes} and \code{edges} generated by \code{Create}.}

\item{data}{A dataframe generated by \code{Collect}.}

\item{...}{Additional parameters passed to function.}
}
\value{
Network as a named list of two dataframes containing \code{$nodes} and \code{$edges}
including columns containing text data.
}
\description{
Network is supplemented with additional social media text data applied as node or edge attributes.
}
\note{
Supports all \code{activity} and \code{actor} networks. Refer to \code{\link{AddText.activity.reddit}} and
\code{\link{AddText.actor.reddit}} for additional reddit parameters. Refer to \code{\link{AddText.actor.youtube}}
for additional YouTube actor network parameters.
}
\examples{
\dontrun{
# add text to an activity network
activityNetwork <- collectData \%>\% Create("activity") \%>\% AddText(collectData)

# network
# activityNetwork$nodes
# activityNetwork$edges
}

}
