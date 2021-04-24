##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Apr 2021
# Function: plotWinLossTeamVsAllTeams
# This function computes and plots number of wins for each team
#
###########################################################################################
#' @title
#' Plot  wins for each team
#'
#' @description
#' This function computes and plots number of wins for a team against all
#' other teams. The plot includes the number of  wins by team each team and the matches
#' with no result
#'
#' @usage
#' plotWinLossTeamVsAllTeams(team1,dir=".")
#'
#' @param team1
#' The 1st team
#'
#'
#' @param dir
#' The source directory of the RData files
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

plotWinLossTeamVsAllTeams <- function(team1,dir="."){
    matches=NULL
    venue=winner=result=date=NULL
    # Create 2 filenames with both combinations of team1 and team2
    d1 <-paste("allMatchesAllOpposition-",team1,".RData",sep="")
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

    plot.title <- paste("Number of wins of",team1,"against all teams in all  matches")
    ggplot(winLoss, aes(x=winner, y=count, fill=winner))+
        geom_bar(stat = "identity",position="dodge") +
        xlab("Winner") + ylab("Numer of Wins") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),"")))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

}
