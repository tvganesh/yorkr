##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: teamWicketERDeathOversPlotOppnAllMatches
# This function computes the  wickets vs ER of team in death overs against  opposition in all matches
#
###########################################################################################
#' @title
#' Team wickets vs ER in death overs against opposition all matches
#'
#' @description
#' This function computes wickets vs ER in death overs against  oppositions in all matches
#'
#' @usage
#' teamWicketERDeathOversPlotOppnAllMatches(matches,t1, plot=1)
#'
#' @param matches
#' The matches of the team against all oppositions and all matches
#'
#' @param t1
#' Team for which bowling performance is required
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
#'
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' teamWicketERDeathOversPlotOppnAllMatches(matches, t1, plot=1)
#'
#' @seealso
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'
teamWicketERDeathOversPlotOppnAllMatches <- function(matches,t1,t2,plot=1) {
  team=ball=totalRuns=total=wickets=wicketsPowerPlay=wicketsDeathOvers=wicketsDeathOvers=bowler=str_extract=NULL
  ggplotly=wicketPlayerOut=NULL

  # Filter the performance of team1
  a <-filter(matches,team!=t1)
  # Middle overs
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsDeathOvers=sum(wickets))

  a21 <- select(a1,team,bowler,date,totalRuns)
  a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERDeathOvers=total/count *6)
  a41 <- a31 %>% select(bowler,ERDeathOvers)
  a42=inner_join(a4,a41,by="bowler")


  x_lower <- quantile(a42$wicketsDeathOvers,p=0.66)
  y_lower <- quantile(a42$ERDeathOvers,p=0.33)
  plot.title <- paste("Wickets-ER Plot of", t1, "against ", t2, " in death overs all matches")
  if(plot == 1){ #ggplot2
    a42 %>%
      mutate(quadrant = case_when(wicketsDeathOvers > x_lower & ERDeathOvers > y_lower   ~ "Q1",
                                  wicketsDeathOvers <= x_lower & ERDeathOvers > y_lower  ~ "Q2",
                                  wicketsDeathOvers <= x_lower & ERDeathOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsDeathOvers,ERDeathOvers,color=quadrant)) +
      geom_text(aes(wicketsDeathOvers,ERDeathOvers,label=bowler,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  a42 %>%
      mutate(quadrant = case_when(wicketsDeathOvers > x_lower & ERDeathOvers > y_lower   ~ "Q1",
                                  wicketsDeathOvers <= x_lower & ERDeathOvers > y_lower  ~ "Q2",
                                  wicketsDeathOvers <= x_lower & ERDeathOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsDeathOvers,ERDeathOvers,color=quadrant)) +
      geom_text(aes(wicketsDeathOvers,ERDeathOvers,label=bowler,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}


