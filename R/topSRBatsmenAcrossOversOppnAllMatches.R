##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: topSRBatsmenAcrossOversOppnAllMatches.R
# This function computes the highest SR by batsmen in matches against opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the highest SR by batsmen against team in powerplay, middle and death overs
#'
#' @description
#' This function  computes the highest SR by batsmen by batsman against team in in powerplay, middle and death overs
#'
#' @usage
#' topSRBatsmenAcrossOversOppnAllMatches(matches,t1,plot=1)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The 1st team of the match
#'
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
#' topSRBatsmenAcrossOversOppnAllMatches(matches,'England')
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
topSRBatsmenAcrossOversOppnAllMatches <- function(matches,t1,plot=1) {
    team=ball=totalRuns=total=SRinPowerpPlay=SRinMiddleOvers=SRinDeathOvers=batsman=runs=str_extract=NULL
    ggplotly=NULL
    # Filter the performance of team1
    a <-filter(matches,team==t1)
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,ball,totalRuns,batsman,date)
    a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRinPowerpPlay=runs/count*100) %>% arrange(desc(SRinPowerpPlay)) %>%
          select(batsman,SRinPowerpPlay)

    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,ball,totalRuns,batsman,date)
    b3 <- b2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRinMiddleOvers=runs/count*100) %>% arrange(desc(SRinMiddleOvers)) %>%
        select(batsman,SRinMiddleOvers)

    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,ball,totalRuns,batsman,date)
    c3 <- c2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRinDeathOvers=runs/count*100) %>% arrange(desc(SRinDeathOvers)) %>%
        select(batsman,SRinDeathOvers)

    val=min(dim(a3)[1],dim(b3)[1],dim(c3)[1])
    m=cbind(a3[1:val,],b3[1:val,],c3[1:val,])
    m
}
