##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: overallWicketsERPowerPlayPlotT20
# This function plots Wickets vs ER in power play of Intl.  T20  batsmen
#
#
###########################################################################################
#' @title
#' Plot the Wickets vs ER in power play  of Intl. T20 batsmen
#'
#' @description
#' Wickets vs ER in power play  of Intl. T20 batsmen
#'
#' @usage
#' overallWicketsERPowerPlayPlotT20(dir=".", dateRange,type="IPL",plot=1)
#'
#' @param dir
#' The input directory
#'
#'
#' @param dateRange
#' Date interval to consider
#'
#' @param type
#' T20 league
#'
#' @param plot
#' plot=1 (static),plot=2(interactive), plot=3 (table)
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
#' overallWicketsERPowerPlayPlotT20(dir=".", dateRange,type="IPL",plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallWicketsERPowerPlayPlotT20 <- function(dir=".", dateRange,type="IPL",plot=1) {
  team=ball=totalRuns=total=wickets=wicketsPowerPlay=bowler=str_extract=NULL
  ggplotly=wicketPlayerOut=t20MDF=ERPowerPlay=quantile=quadrant=t20MDF=ERMiddleOvers=NULL

  fl <- paste(dir,"/",type,"-MatchesDataFrame.RData",sep="")
  load(fl)

  df=t20MDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])

  # Power play
  a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsPowerPlay=sum(wickets))

  a21 <- select(a1,team,bowler,date,totalRuns)
  a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERPowerPlay=total/count *6)
  a41 <- a31 %>% select(bowler,ERPowerPlay)
  a42=inner_join(a4,a41,by="bowler")

  x_lower <- 1/2 * min(a42$wicketsPowerPlay + max(a42$wicketsPowerPlay))
  y_lower <- 1/2 * min(a42$ERPowerPlay + max(a42$ERPowerPlay))


  plot.title <- paste("Top T20 bowlers in Power play in Intl. T20 (men)")
  if(plot == 1){ #ggplot2
    a42 %>%
      mutate(quadrant = case_when(wicketsPowerPlay > x_lower & ERPowerPlay > y_lower   ~ "Q1",
                                  wicketsPowerPlay <= x_lower & ERPowerPlay > y_lower  ~ "Q2",
                                  wicketsPowerPlay <= x_lower & ERPowerPlay <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsPowerPlay,ERPowerPlay,color=quadrant)) +
      geom_text(aes(wicketsPowerPlay,ERPowerPlay,label=bowler,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <- a42 %>%
      mutate(quadrant = case_when(wicketsPowerPlay > x_lower & ERPowerPlay > y_lower   ~ "Q1",
                                  wicketsPowerPlay <= x_lower & ERPowerPlay > y_lower  ~ "Q2",
                                  wicketsPowerPlay <= x_lower & ERPowerPlay <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsPowerPlay,ERPowerPlay,color=quadrant)) +
      geom_text(aes(wicketsPowerPlay,ERPowerPlay,label=bowler,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}
