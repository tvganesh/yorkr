##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 4 Nov 2021
# Function: teamERAcrossOvers
# This function the plots economy rate in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the ER in powerplay, middle and death overs
#'
#' @description
#' This function  plots the runs in in powerplay, middle and death overs
#'
#' @usage
#' teamERAcrossOvers(match,t1,t2,plot=1)
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
#' teamERAcrossOvers(match,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
teamERAcrossOvers <- function(match,t1,t2,plot=1) {
    team=ball=totalRuns=total=str_extract=type=ER=opposition=NULL
    ggplotly=NULL

    # Filter the performance of team1
    a <-filter(match,team==t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,team,totalRuns)
    a3 <- a2 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())

    a3$ER=ifelse(dim(a3)[1]==0, 0,a3$total/a3$count * 6)
    if(dim(a3)[1]!=0){
        a3$type="1-PowerPlay"
        a3$opposition=t2
    }


    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,team,totalRuns)
    b3 <- b2 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())

    b3$ER=ifelse(dim(b3)[1]==0, 0,b3$total/b3$count * 6)
    if(dim(b3)[1]!=0){
        b3$type="2-Middle overs"
        b3$opposition=t2
    }

    ##Death overs
    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,team,totalRuns)
    c3 <- c2 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())

    c3$ER=ifelse(dim(c3)[1]==0, 0,c3$total/c3$count * 6)
    if(dim(c3)[1]!=0){
        c3$type="3-Death overs"
        c3$opposition=t2
    }





    ####################
    # Filter the performance of team2
    a <-filter(match,team==t2)
    # Power play
    a11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a21 <- select(a11,team,totalRuns)
    a31 <- a21 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())

    a31$ER=ifelse(dim(a31)[1]==0, 0,a31$total/a31$count * 6)
    if(dim(a31)[1]!=0){
        a31$type="1-PowerPlay"
        a31$opposition=t1
    }


    # Middle overs I
    b11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b21 <- select(b11,team,totalRuns)
    b31 <- b21 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    b31$ER=ifelse(dim(b31)[1]==0, 0,b31$total/b31$count * 6)

    if(dim(b31)[1]!=0){
        b31$type="2-Middle overs"
        b31$opposition=t1
    }

    ##Death overs
    c11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c21 <- select(c11,team,totalRuns)
    c31 <- c21 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    c31$ER=ifelse(dim(c31)[1]==0, 0,c31$total/c31$count * 6)

    if(dim(c31)[1]!=0){
        c31$type="3-Death overs"
        c31$opposition=t1
    }

    m=rbind(a3,b3,c3,a31,b31,c31)
    plot.title= paste("Economy rate across 20 overs of ",t1, "and", t2, sep=" ")



    # Plot both lines
    if(plot ==1){ #ggplot2
        ggplot(m,aes(x=type, y=ER, fill=opposition)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))



    }else { #ggplotly
        g <- ggplot(m,aes(x=type, y=ER, fill=opposition)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(plot.title)

        ggplotly(g)

    }


}
