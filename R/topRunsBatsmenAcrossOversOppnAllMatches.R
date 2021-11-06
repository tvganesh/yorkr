##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: topRunsBatsmenAcrossOversOppnAllMatches.R
# This function computes the top runs scorers in matches against opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the most runs scored by batsmen against team in powerplay, middle and death overs
#'
#' @description
#' This function  computes the most runs by batsman against team in in powerplay, middle and death overs
#'
#' @usage
#' topRunsBatsmenAcrossOversOppnAllMatches(matches,t1,plot=1)
#'
#' @param matches
#' The dataframe of the matches
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
#' topRunsBatsmenAcrossOversOppnAllMatches(matches,'England')
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
topRunsBatsmenAcrossOversOppnAllMatches <- function(matches,t1,plot=1) {
    team=ball=totalRuns=total=runsPowerPlay=runsMiddleOvers=runsDeathOvers=matches=str_extract=NULL
    ggplotly=batsman=NULL
    # Filter the performance of team1
    matches= matches %>% filter(date >= "2019-01-01" & date <= "2021-11-01")
    a <-filter(matches,team==t1)
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,ball,totalRuns,batsman)
    a3 <- a2 %>% group_by(batsman) %>% summarise(runsPowerPlay= sum(totalRuns)) %>% arrange(desc(runsPowerPlay))


    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,ball,totalRuns,batsman)
    b3 <- b2 %>% group_by(batsman) %>% summarise(runsMiddleOvers= sum(totalRuns)) %>% arrange(desc(runsMiddleOvers))

    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,ball,totalRuns,batsman)
    c3 <- c2 %>% group_by(batsman) %>% summarise(runsDeathOvers= sum(totalRuns)) %>% arrange(desc(runsDeathOvers))

    val=min(dim(a3)[1],dim(b3)[1],dim(c3)[1])
    m=cbind(a3[1:val,],b3[1:val,],c3[1:val,])
    m
}
