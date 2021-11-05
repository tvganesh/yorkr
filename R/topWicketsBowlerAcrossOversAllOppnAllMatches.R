########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: topWicketsBowlerAcrossOversAllOppnAllMatches
# This function best wicket takes in all matches against all opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the most wickets by bowlers against all team in powerplay, middle and death overs
#'
#' @description
#' This function  computes the highest wickets by bowlers against all team in in powerplay, middle and death overs
#'
#' @usage
#' topWicketsBowlerAcrossOversAllOppnAllMatches(matches,t1)
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
#' # Plot tne match worm plot
#' topWicketsBowlerAcrossOversAllOppnAllMatches(matches,'England')
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
topWicketsBowlerAcrossOversAllOppnAllMatches <- function(matches,t1) {
  team=ball=totalRuns=total=NULL
  ggplotly=NULL

  # Filter the performance of team1
  a <-filter(matches,team!=t1)
  # Power play
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsPowerPlay=sum(wickets)) %>%
             arrange(desc(wicketsPowerPlay))

  # Middle overs
  b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b2 <- select(b1,date,bowler,wicketPlayerOut)
  b3 <- b2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  b4 <- b3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsMiddleOvers=sum(wickets)) %>%
    arrange(desc(wicketsMiddleOvers))

  #Death overs
  c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c2 <- select(c1,date,bowler,wicketPlayerOut)
  c3 <- c2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  c4 <- c3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsDeathOvers=sum(wickets)) %>%
    arrange(desc(wicketsDeathOvers))

  val=min(dim(a4)[1],dim(b4)[1],dim(c4)[1])
  m=cbind(a4[1:val,],b4[1:val,],c4[1:val,])

  m

}
