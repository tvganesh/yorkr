#######################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: topERBowlerAcrossOversAllOppnAllMatches.R
# This function computes the best ER by bowlers in all matches against all opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the best ER by bowlers against all team in powerplay, middle and death overs
#'
#' @description
#' This function  computes the best ER by bowlers against akk team in in powerplay, middle and death overs
#'
#' @usage
#' topERBowlerAcrossOversAllOppnAllMatches(matches,t1)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The 1st team of the match
#'
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

#' topERBowlerAcrossOversAllOppnAllMatches(matches,'England')
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
topERBowlerAcrossOversAllOppnAllMatches <- function(matches,t1) {
  team=ball=totalRuns=total=NULL
  ggplotly=NULL

  # Filter the performance of team1
  a <-filter(matches,team!=t1)
  # Power play
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,team,bowler,date,totalRuns)
  a3 <- a2 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERPowerPlay=total/count *6) %>% filter(count > quantile(count,prob=0.25))
  a4 <- a3 %>% select(bowler,ERPowerPlay) %>% arrange(ERPowerPlay)

  # Middle overs
  b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b2 <- select(b1,team,bowler,date,totalRuns)
  b3 <- b2 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERMiddleOvers=total/count *6) %>% filter(count > quantile(count,prob=0.25))
  b4 <- b3 %>% select(bowler,ERMiddleOvers) %>% arrange(ERMiddleOvers)

  #Death overs
  c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c2 <- select(c1,team,bowler,date,totalRuns)
  c3 <- c2 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERDeathOvers=total/count *6) %>% filter(count > quantile(count,prob=0.25))
  c4 <- c3 %>% select(bowler,ERDeathOvers) %>% arrange(ERDeathOvers)

  val=min(dim(a4)[1],dim(b4)[1],dim(c4)[1])
  m=cbind(a4[1:val,],b4[1:val,],c4[1:val,])
  m

}

