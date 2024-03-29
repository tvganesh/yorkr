% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAllMatchesBetweenTeams.R
\name{getAllMatchesBetweenTeams}
\alias{getAllMatchesBetweenTeams}
\title{Get data on all matches  between 2 opposing teams}
\usage{
getAllMatchesBetweenTeams(team1,team2,dir=".",save=FALSE,odir = ".")
}
\arguments{
\item{team1}{One of the team in consideration e.g (India, Australia, England)}

\item{team2}{The other team for which matches are needed e.g( India, Sri Lanka, Pakistan)}

\item{dir}{The input directory which has the RData files of matches between teams}

\item{save}{Default=FALSE. This parameter indicates whether the combined data frame needs to be saved or not. It is recommended
to save this large dataframe as the creation of this data frame takes a several seconds depending
on the number of matches}

\item{odir}{The output directory}
}
\value{
matches
The combined data frame
}
\description{
This function gets all the data on matches between opposing teams for e.g India-Pakistan,
Englad-Australia, South Africa- Sri Lanka etc. It constructs a huge dataframe of all these
matches. This can be saved by the user which can be used in function in which analyses are
done for all matches between these teams. This is done by loading the saved .RData for
each match and performing an rbind of the data frames for each match
}
\note{
Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
}
\examples{
\dontrun{
# Get all matches for team India
getAllMatchesAllOpposition("India",dir="../data/",save=TRUE)
getAllMatchesAllOpposition("Australia",dir="./mysavedata/",save=TRUE)
}

}
\references{
\url{https://cricsheet.org/}\cr
\url{https://gigadom.in/}\cr
\url{https://github.com/tvganesh/yorkrData/}
}
\seealso{
\code{\link{bowlerMovingAverage}}\cr
\code{\link{bowlerWicketsVenue}}\cr
\code{\link{getAllMatchesAllOpposition}}\cr
}
\author{
Tinniam V Ganesh
}
