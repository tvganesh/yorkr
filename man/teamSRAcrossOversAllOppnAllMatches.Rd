% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamSRAcrossOversAllOppnAllMatches.R
\name{teamSRAcrossOversAllOppnAllMatches}
\alias{teamSRAcrossOversAllOppnAllMatches}
\title{Compute the strike rate by team against all team in powerplay, middle and death overs in all matches}
\usage{
teamSRAcrossOversAllOppnAllMatches(matches,t1,plot=1)
}
\arguments{
\item{matches}{The dataframe of the matches}

\item{t1}{The team of the matches}

\item{plot}{Plot=1 (static), Plot=2(interactive)}
}
\value{
none
}
\description{
This function  plots the SR by team against  all team in in powerplay, middle and death overs
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{

# Plot tne match worm plot
teamSRAcrossOversAllOppnAllMatches(matches,"Pakistan")
}
}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{getBatsmanDetails}}\cr
\code{\link{getBowlerWicketDetails}}\cr
\code{\link{batsmanDismissals}}\cr
\code{\link{getTeamBattingDetails}}\cr
}
\author{
Tinniam V Ganesh
}
