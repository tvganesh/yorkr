##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: overallWicketsERMiddleOversPlotT20M
# This function plots Wickets vs ER in power play of Intl.  T20  batsmen
#
#
###########################################################################################
#' @title
#' Plot the Wickets vs ER in middle overs  of Intl. T20 batsmen
#'
#' @description
#' Wickets vs ER in middle overs  of Intl. T20 batsmen
#'
#' @usage
#' overallWicketsERMiddleOversPlotT20M(dir=".",minMatches, dateRange)
#'
#' @param teamNames
#' The team names
#'
#' @param odir
#' The output directory
#'
#' @param minMatches
#' Minimum matches played
#'
#' @param dateRange
#' Date interval to consider
#'
#'
#' @return The ranked T20 batsmen
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
#' overallWicketsERPowerPlayPlotT20M(dir=".",minmatches, dateRange)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallWicketsERMiddleOversPlotT20M <- function(dir=".",minMatches, dateRange) {
  team=ball=totalRuns=total=wickets=wicketsPowerPlay=wicketsMiddleOvers=wicketsDeathOvers=bowler=str_extract=NULL
  ggplotly=wicketPlayerOut=NULL

  fl <- paste(dir,"/T20MDataFrame.RData",sep="")
  load(fl)

  df=t20MDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])
  # Middle overs
  a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsMiddleOvers=sum(wickets))

  a21 <- select(a1,team,bowler,date,totalRuns)
  a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERMiddleOvers=total/count *6)
  a41 <- a31 %>% select(bowler,ERMiddleOvers)
  a42=inner_join(a4,a41,by="bowler")

  x_lower <- 1/2 * min(a42$wicketsMiddleOvers + max(a42$wicketsMiddleOvers))
  y_lower <- 1/2 * min(a42$ERMiddleOvers + max(a42$ERMiddleOvers))


  plot.title <- paste("Top T20 bowlers in Middle overs in Intl. T20 (men)")
  if(plot == 1){ #ggplot2
    a42 %>%
      mutate(quadrant = case_when(wicketsMiddleOvers > x_lower & ERMiddleOvers > y_lower   ~ "Q1",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers > y_lower  ~ "Q2",
                                  wicketsMiddleOvers <= x_lower & ERMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsMiddleOvers,ERMiddleOvers,color=quadrant)) +
      geom_text(aes(wicketsMiddleOvers,ERMiddleOvers,label=bowler,color=quadrant)) + geom_point() +
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
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}


