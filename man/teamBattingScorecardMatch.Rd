% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamBattingScorecardMatch.R
\name{teamBattingScorecardMatch}
\alias{teamBattingScorecardMatch}
\title{Team batting scorecard of a team in a match}
\usage{
teamBattingScorecardMatch(match,theTeam)
}
\arguments{
\item{match}{The match for which the score card is required e.g.}

\item{theTeam}{Team for which scorecard required}
}
\value{
scorecard
A data frame with the batting scorecard
}
\description{
This function computes returns the batting scorecard (runs, fours, sixes, balls played) for the
team
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
teamBowlingScorecardMatch(a,'England')
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
}
\author{
Tinniam V Ganesh
}
