##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: teamWicketERMiddleOversPlotAllOppnAllMatches
# This function computes the  wickets vs ER of team in middle overs against all opposition in all matches
#
###########################################################################################
#' @title
#' Team wickets vs ER in middle overs against  all opposition all matches
#'
#' @description
#' This function computes wickets vs ER in middle overs against all oppositions in all matches
#'
#' @usage
#' teamWicketERMiddleOversPlotAllOppnAllMatches(matches,t1, plot=1)
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
#' teamWicketERMiddleOversPlotAllOppnAllMatches(matches, t1, plot=1)
#' }
#' @seealso
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'
teamWicketERMiddleOversPlotAllOppnAllMatches <- function(matches,t1, plot=1) {
  team=ball=totalRuns=total=wickets=wicketsMiddleOvers=wicketsDeathOvers=bowler=str_extract=NULL
  ggplotly=wicketPlayerOut=str_extract=quantile=quadrant=ERMiddleOvers=NULL

  # Filter the performance of team1
  a <-filter(matches,team!=t1)
  # Middle overs
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsMiddleOvers=sum(wickets))

  a21 <- select(a1,team,bowler,date,totalRuns)
  a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERMiddleOvers=total/count *6)
  a41 <- a31 %>% select(bowler,ERMiddleOvers)
  a42=inner_join(a4,a41,by="bowler")


  x_lower <- quantile(a42$wicketsMiddleOvers,p=0.66,na.rm = TRUE)
  y_lower <- quantile(a42$ERMiddleOvers,p=0.33,na.rm = TRUE)

  plot.title <- paste("Wickets-ER Plot of", t1, "in Middle overs against all opposition  all matches")
  if(plot == 1){ #ggplot2
    a42 %>%
      mutate(quadrant = case_when(wicketsMiddleOvers > x_lower & ERMiddleOvers > y_lower   ~ "Q1",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers > y_lower  ~ "Q2",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsMiddleOvers,ERMiddleOvers,color=quadrant)) +
      geom_text(aes(wicketsMiddleOvers,ERMiddleOvers,label=bowler,color=quadrant)) + geom_point() +
      xlab("Wickets - Middle overs") + ylab("Economy rate - Middle overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  a42 %>%
      mutate(quadrant = case_when(wicketsMiddleOvers > x_lower & ERMiddleOvers > y_lower   ~ "Q1",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers > y_lower  ~ "Q2",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsMiddleOvers,ERMiddleOvers,color=quadrant)) +
      geom_text(aes(wicketsMiddleOvers,ERMiddleOvers,label=bowler,color=quadrant)) + geom_point() +
      xlab("Wickets - Middle overs") + ylab("Economy rate - Middle overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}


