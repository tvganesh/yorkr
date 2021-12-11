##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Nov 2021
# Function: teamRunsSRPowerPlayPlotAllOppnAllMatches
# This function plot the runs vs SR for  the team batsman during powerplay against all opposition in
# in all matches
#
#
###########################################################################################
#' @title
#' Team batting plots runs vs SR in powerplay for team against all oppositions in all matches
#'
#' @description
#' This function computes and plots runs vs SR  in power play of a team in all matches against all
#' oppositions.
#'
#' @usage
#' teamRunsSRPowerPlayPlotAllOppnAllMatches(matches,t1,plot=1)
#'
#' @param matches
#' All matches of the team in all matches with all oppositions
#'
#' @param t1
#' The team for which the the batting partnerships are sought
#'
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
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
#' # Top batsman is displayed in descending order of runs
#' teamRunsSRPowerPlayPlotAllOppnAllMatches(matches,t1,plot=1)
#'
#' }
#'
#' @seealso
#' \code{\link{teamBatsmenVsBowlersAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBowlingWicketRunsAllOppnAllMatches}}
#'
#' @export
#'
teamRunsSRPowerPlayPlotAllOppnAllMatches <- function(matches,t1,plot=1) {
  team=ball=totalRuns=total=str_extract=batsman=runs=quantile=quadrant=SRPowerPlay=NULL
  ggplotly=NULL
  # Filter the performance of team1
  a <-filter(matches,team==t1)
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,ball,totalRuns,batsman,date)
  a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRPowerPlay=runs/count*100)

  x_lower <- quantile(a3$runs,p=0.66)
  y_lower <- quantile(a3$SRPowerPlay,p=0.66)

  plot.title <- paste(t1, "best batsmen in powerplay in all matches all teams")
  if(plot == 1){ #ggplot2
    a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRPowerPlay > y_lower   ~ "Q1",
                                  runs <= x_lower & SRPowerPlay > y_lower  ~ "Q2",
                                  runs <= x_lower & SRPowerPlay <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRPowerPlay,color=quadrant)) +
      geom_text(aes(runs,SRPowerPlay,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRPowerPlay > y_lower   ~ "Q1",
                                  runs <= x_lower & SRPowerPlay > y_lower  ~ "Q2",
                                  runs <= x_lower & SRPowerPlay <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRPowerPlay,color=quadrant)) +
      geom_text(aes(runs,SRPowerPlay,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }


}
