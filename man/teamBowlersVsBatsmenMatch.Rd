% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamBowlersVsBatsmenMatch.R
\name{teamBowlersVsBatsmenMatch}
\alias{teamBowlersVsBatsmenMatch}
\title{Team bowlers vs batsmen in a match}
\usage{
teamBowlersVsBatsmenMatch(match,theTeam,opposition, plot=1)
}
\arguments{
\item{match}{The data frame of the match. This can be obtained with the call for e.g
a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")}

\item{theTeam}{The team against which the performance is required}

\item{opposition}{The opposition team}

\item{plot}{plot=1 (static),plot=2(interactive),plot=3(table)}
}
\value{
None or dataframe
If plot=TRUE there is no return. If plot=TRUE then the dataframe is returned
}
\description{
This function computes performance of bowlers of a team against an opposition in a match
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
# Get the match between England and Pakistan
a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
teamBowlersVsBatsmenMatch(a,"Pakistan","England")
teamBowlersVsBatsmenMatch(a,"England","Pakistan")
m <- teamBowlersVsBatsmenMatch(a,"Pakistan","England")
}


}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
\code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
\code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
\code{\link{teamBowlersVsBatsmenAllOppnAllMatchesRept}}\cr
\code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
}
\author{
Tinniam V Ganesh
}
