##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: matchWormGraph
# This function computes  and plots the match worm chart
#
###########################################################################################
#' @title
#' Plot the match worm graph
#'
#' @description
#' This function  plots the match worm graph between 2 teams in a match
#'
#' @usage
#' matchWormGraph(match,team1,team2)
#'
#' @param match
#' The dataframe of the match
#'
#' @param team1
#' The 1st team of the match
#'
#' @param team2
#' the 2nd team in the match
#'
#' @return none
#'
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' #Get the match details
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#' # Plot tne match worm plot
#' matchWormGraph(a,'England',"Pakistan")
#'
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}
#'
#' @export
#'
matchWormGraph <- function(match,team1,team2) {
    # Filter the performance of team1
    a <-filter(match,team==team1)
    b <- select(a,ball,totalRuns)
    c <-mutate(b,ball=gsub("1st\\.","",ball))
    # Compute cumulative sum vs balls bowled
    d <- mutate(c,total=cumsum(totalRuns))

    # Filter performance of team2
    a <-filter(match,team==team2)
    b1 <- select(a,ball,totalRuns)
    c1 <-mutate(b1,ball=gsub("2nd\\.","",ball))
    # Compute cumulative sum vs balls bowled
    d1 <- mutate(c1,total=cumsum(totalRuns))

    # Plot both lines
    plot(d$ball,d$total,col="blue",type="l",lwd=2,xlab="Overs",ylab='Runs',
         main="Worm chart of match")
    lines(d1$ball,d1$total,type="l",col="red",lwd=2)
    teams <-c(team1,team2)
    legend(x="topleft",legend=teams,
           col=c("blue","red"),bty="n",cex=0.8,lty=1,lwd=2)
}
