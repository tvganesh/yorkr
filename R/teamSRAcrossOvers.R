##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 4 Nov 2021
# Function: teamSRAcrossOvers
# This function the computes the Strike Rate  in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the Strike Rate  in powerplay, middle and death overs
#'
#' @description
#' This function  plots strike rate scored in powerplay, middle and death overs
#'
#' @usage
#' teamSRAcrossOvers(match,t1,t2,plot=1)
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
#' teamSRAcrossOvers(match,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
teamSRAcrossOvers <- function(match,t1,t2,plot=1) {
    team=ball=totalRuns=total=type=SR=str_extract=NULL
    ggplotly=NULL

    # Filter the performance of team1
    a <-filter(match,team ==t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,team,totalRuns)
    a3 <- a2 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    a3$SR=ifelse(dim(a3)[1]==0, 0,a3$total/a3$count *100)
    if(dim(a3)[1]!=0)
        a3$type="1-Power Play"

    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,team,totalRuns)
    b3 <- b2 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    b3$SR=ifelse(dim(b3)[1]==0, 0,b3$total/b3$count *100)
    if(dim(b3)[1]!=0)
        b3$type="2-Middle Overs"

    ##Death overs
    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,team,totalRuns)
    c3 <- c2 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    c3$SR=ifelse(dim(c3)[1]==0, 0,c3$total/c3$count *100)
    if(dim(c3)[1]!=0)
        c3$type="3-Death Overs"



    ####################
    # Filter the performance of team2
    a <-filter(match,team ==t2)
    # Power play
    a11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a21 <- select(a11,team,totalRuns)
    a31 <- a21 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    a31$SR=ifelse(dim(a31)[1]==0, 0,a31$total/a31$count *100)
    if(dim(a31)[1]!=0)
        a31$type="1-Power Play"

    # Middle overs I
    b11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b21 <- select(b11,team,totalRuns)
    b31 <- b21 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    b31$SR=ifelse(dim(b31)[1]==0, 0,b31$total/b31$count *100)
    if(dim(b31)[1]!=0)
        b31$type="2-Middle Overs"

    ##Death overs
    c11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c21 <- select(c11,team,totalRuns)
    c31 <- c21 %>% group_by(team) %>% summarise(total=sum(totalRuns),count=n())
    c31$SR=ifelse(dim(c31)[1]==0, 0,c31$total/c31$count *100)
    if(dim(c31)[1]!=0)
        c31$type="3-Death Overs"

    m=rbind(a3,b3,c3,a31,b31,c31)
    plot.title= paste("Strike rate across 20 overs of ",t1, "and", t2, sep=" ")

    # Plot both lines
    if(plot ==1){ #ggplot2
        ggplot(m,aes(x=type, y=SR, fill=team)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))


    }else { #ggplotly
        g <-    ggplot(data = m,mapping=aes(x=type, y=SR, fill=team)) +
            geom_bar(stat="identity", position = "dodge") +
            ggtitle(plot.title)


        ggplotly(g)

    }


}
