#######################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: topERBowlerAcrossOversOppnAllMatches.R
# This function computes the best ER by bowlers in matches against opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the best ER by bowlers against team in powerplay, middle and death overs
#'
#' @description
#' This function  computes the best ER by bowlers against team in in powerplay, middle and death overs
#'
#' @usage
#' topERBowlerAcrossOversOppnAllMatches.R(matches,t1,plot=1)
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
#' topERBowlerAcrossOversOppnAllMatches.R(matches,'England')
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
topERBowlerAcrossOversOppnAllMatches <- function(matches,t1,plot=1) {
  team=ball=totalRuns=total=NULL
  ggplotly=NULL

  # Filter the performance of team1
  a <-filter(matches,team!=t1)
  # Power play
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,team,bowler,date,totalRuns)
  a3 <- a2 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERPowerPlay=total/count *6)
  a4 <- a3 %>% select(bowler,ERPowerPlay) %>% arrange(ERPowerPlay)

  # Middle overs
  b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b2 <- select(b1,team,bowler,date,totalRuns)
  b3 <- b2 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERMiddleOvers=total/count *6)
  b4 <- b3 %>% select(bowler,ERMiddleOvers) %>% arrange(ERMiddleOvers)

  #Death overs
  c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c2 <- select(c1,team,bowler,date,totalRuns)
  c3 <- c2 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERDeathOvers=total/count *6)
  c4 <- c3 %>% select(bowler,ERDeathOvers) %>% arrange(ERDeathOvers)

  val=min(dim(a4)[1],dim(b4)[1],dim(c4)[1])
  m=cbind(a4[1:val,],b4[1:val,],c4[1:val,])
  m

}

