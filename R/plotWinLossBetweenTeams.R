##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Apr 2021
# Function: plotWinLossBetweenTeams
# This function computes and plots number of wins for each team
#
###########################################################################################
#' @title
#' Plot  wins for each team
#'
#' @description
#' This function computes and plots number of wins for each team in all their
#' encounters. The plot includes the number of  wins byteam1 each team and the matches
#' with no result
#'
#' @usage
#' plotWinLossBetweenTeams(team1,team2,dir=".")
#'
#' @param team1
#' The 1st team
#'
#' @param team2
#' The 2nd team
#'
#' @param dir
#' The source directory of teh RData files
#'
#' @return None
#'
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#'
#' plotWinLossBetweenTeams(team1="India",team2="Australia",dir=pathToFile)
#' batsmanDismissals(kohli,"Kohli")
#' }
#' @seealso
#' \code{\link{batsmanFoursSixes}}\cr
#' \code{\link{batsmanRunsVsDeliveries}}\cr
#' \code{\link{batsmanRunsVsStrikeRate}}\cr
#'
#'
#' @export
#'

plotWinLossBetweenTeams <- function(team1,team2,dir=".",plot=1){
    matches=NULL
    venue=winner=result=date=NULL
    # Create 2 filenames with both combinations of team1 and team2
    d1 <-paste(team1,"-",team2,"-allMatches.RData",sep="")
    fl1 <- paste(dir,"/",d1,sep="")
    load(fl1)
    a <- select(matches,date,venue,winner,result)
    b=distinct(a) #Get distinct rows

    winLoss <- summarise(group_by(b,winner),count=n())

    x <- winLoss$winner=="NA"
    winLoss$winner <- as.character(winLoss$winner)
    if(sum(x) !=0) {
        winLoss[x,]$winner <-"NoResult"
    }

    plot.title <- paste("Number of wins in",team1," vs ",team2, " matches")
    if(plot == 1){ #ggplot2
        ggplot(winLoss, aes(x=winner, y=count, fill=winner))+
            geom_bar(stat = "identity",position="dodge") +
            xlab("Winner") + ylab("Numer of Wins") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else if(plot == 2 || plot == 3){
        g <- ggplot(winLoss, aes(x=winner, y=count, fill=winner))+
            geom_bar(stat = "identity",position="dodge") +
            xlab("Winner") + ylab("Numer of Wins") +
            ggtitle(plot.title)
        ggplotly(g)
    }


}
