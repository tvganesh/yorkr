% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/teamBowlingWicketRunsMatch.R
\name{teamBowlingWicketRunsMatch}
\alias{teamBowlingWicketRunsMatch}
\title{Team bowling wickets runs conceded in match}
\usage{
teamBowlingWicketRunsMatch(match,theTeam,opposition,plot=1)
}
\arguments{
\item{match}{The match between the teams}

\item{theTeam}{Team for which bowling performance is required}

\item{opposition}{The opposition team}

\item{plot}{plot=1 (static),plot=2(interactive),plot=3(table)}
}
\value{
None or data fame
A data frame with the bowling performance in all matches against all oppositions
}
\description{
This function computes returns the wickets taken and runs conceded bowlers in a match between 2 teams.
The user can choose to plot or return a dataframe
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
#Get the match details
a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")

teamBowlingWicketRunsMatch(a,"England","Pakistan",plot=FALSE)
teamBowlingWicketRunsMatch(a,"Pakistan","England")
}

}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{teamBowlingWicketMatch}}\cr
\code{\link{teamBowlingWicketRunsMatch}}\cr
\code{\link{teamBowlersVsBatsmenMatch}}\cr
}
\author{
Tinniam V Ganesh
}
