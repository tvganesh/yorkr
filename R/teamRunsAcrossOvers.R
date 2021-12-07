##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 4 Nov 2021
# Function: teamRunsAcrossOvers
# This function the plots runs scored in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the runs in powerplay, middle and death overs
#'
#' @description
#' This function  plots the runs in in powerplay, middle and death overs
#'
#' @usage
#' teamRunsAcrossOvers(match,t1,t2,plot=1)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The 1st team of the match
#'
#' @param t2
#' the 2nd team in the match
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
#' teamRunsAcrossOvers(match,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
teamRunsAcrossOvers <- function(match,t1,t2,plot=1) {
    team=ball=totalRuns=total=type=str_extract=NULL
    ggplotly=NULL

    # Filter the performance of team1
    a <-filter(match,team==t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,team,totalRuns)
    a3 <- a2 %>% group_by(team) %>% summarise(total=sum(totalRuns))
    a3$type="1-Power Play"

    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,team,totalRuns)
    b3 <- b2 %>% group_by(team) %>% summarise(total=sum(totalRuns))
    b3$type="2-Middle Overs"

    #Death overs
    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,team,totalRuns)
    c3 <- c2 %>% group_by(team) %>% summarise(total=sum(totalRuns))
    c3$type="3-Death Overs"

    ####################
    # Filter the performance of team2
    a <-filter(match,team==t2)
    # Power play
    a11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a21 <- select(a11,team,totalRuns)
    a31 <- a21 %>% group_by(team) %>% summarise(total=sum(totalRuns))
    a31$type="1-Power Play"


    # Middle overs I
    b11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b21 <- select(b11,team,totalRuns)
    b31 <- b21 %>% group_by(team) %>% summarise(total=sum(totalRuns))
    b31$type="2-Middle Overs"

    #Death overs
    c11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1,20.0))
    c21 <- select(c11,team,totalRuns)
    c31 <- c21 %>% group_by(team) %>% summarise(total=sum(totalRuns))
    c31$type="3-Death Overs"

    m=rbind(a3,b3,c3,a31,b31,c31)

    # Plot both lines
    if(plot ==1){ #ggplot2
        plot.title= paste("Runs scored across 20 overs by ",t1, "and", t2, sep=" ")
        ggplot(m,aes(x=type, y=total, fill=team)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))

    }else { #ggplotly
        plot.title= paste("Runs scored across 20 overs by ",t1, "and", t2, sep=" ")
        g <- ggplot(m,aes(x=type, y=total, fill=team)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(plot.title)

        ggplotly(g)

    }


}
