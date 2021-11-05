##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: teamRunsAcrossOversAllOppnAllMatches
# This function computes runs across overs in all matches against all opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the runs by team against all team in powerplay, middle and death overs
#'
#' @description
#' This function  plots the runs by team against all teams in in powerplay, middle and death overs
#'
#' @usage
#' teamRunsAcrossOversAllOppnAllMatches(matches,t1,plot=1)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The  team for which  the runs is required
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
#'
#' @return none
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
#' # Plot tne match worm plot
#' teamRunsAcrossOversAllOppnAllMatches(matches,'England')
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
teamRunsAcrossOversAllOppnAllMatches <- function(matches,t1,plot=1) {
    team=ball=totalRuns=total=NULL
    ggplotly=NULL

    # Filter the performance of team1
    a <-filter(matches,team==t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,team,totalRuns,date)
    a3 <- a2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns))
    a4 = a3 %>% summarise(meanRuns=mean(total))
    a4$type="1-Power Play"


    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,team,totalRuns,date)
    b3 <- b2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns))
    b4 = b3 %>% summarise(meanRuns=mean(total))
    b4$type="2-Middle Overs"

    #Death overs
    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,team,totalRuns,date)
    c3 <- c2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns))
    c4 = c3 %>% summarise(meanRuns=mean(total))
    c4$type="3-Death Overs"


    m=rbind(a4,b4,c4)
    plot.title= paste("Mean runs across 20 overs by ",t1, "in all matches against all teams", sep=" ")


    # Plot both lines
    if(plot ==1){ #ggplot2
        ggplot(m,aes(x=type, y=meanRuns, fill=team)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))




    }else { #ggplotly
        g <-         ggplot(m,aes(x=type, y=meanRuns, fill=team)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(plot.title)
        ggplotly(g)

    }


}
