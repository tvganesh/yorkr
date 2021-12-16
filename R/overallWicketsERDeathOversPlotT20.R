##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: overallWicketsERDeathOversPlotT20
# This function plots Wickets vs ER in Death overs of Intl.  T20  batsmen
#
#
###########################################################################################
#' @title
#' Plot the Wickets vs ER in death overs  of Intl. T20 batsmen
#'
#' @description
#' Wickets vs ER in death overs  of Intl. T20 batsmen
#'
#' @usage
#' overallWicketsERDeathOversPlotT20(dir=".", dateRange,type="IPL",plot=1)
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
#' overallWicketsERDeathOversPlotT20(dir=".", dateRange,type="IPL",plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallWicketsERDeathOversPlotT20 <- function(dir=".", dateRange,type="IPL",plot=1) {
  team=ball=totalRuns=total=wickets=wicketsPowerPlay=wicketsMiddleOvers=wicketsDeathOvers=bowler=str_extract=NULL
  ggplotly=wicketPlayerOut=quantile=quadrant=t20MDF=ERDeathOvers=NULL
  fl <- paste(dir,"/",type,"-MatchesDataFrame.RData",sep="")
  load(fl)

  # Filter by date range
  df=t20MDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])


  # Death overs
  a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsDeathOvers=sum(wickets))

  a21 <- select(a1,team,bowler,date,totalRuns)
  a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERDeathOvers=total/count *6)
  a41 <- a31 %>% select(bowler,ERDeathOvers)
  a42=inner_join(a4,a41,by="bowler")


  x_lower <- quantile(a42$wicketsDeathOvers,p=0.66,na.rm = TRUE)
  y_lower <- quantile(a42$ERDeathOvers,p=0.66,na.rm = TRUE)
  plot.title <- paste("Overall Wickets vs ER in Death overs in ",type,sep="")
  if(plot == 1){ #ggplot2
    a42 %>%
      mutate(quadrant = case_when(wicketsDeathOvers > x_lower & ERDeathOvers > y_lower   ~ "Q1",
                                  wicketsDeathOvers <= x_lower & ERDeathOvers > y_lower  ~ "Q2",
                                  wicketsDeathOvers <= x_lower & ERDeathOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(wicketsDeathOvers,ERDeathOvers,color=quadrant)) +
      geom_text(aes(wicketsDeathOvers,ERDeathOvers,label=bowler,color=quadrant)) + geom_point() +
      xlab("Wickets - Death overs") + ylab("Economy rate - Death overs") +
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
      xlab("Wickets - Death overs") + ylab("Economy rate - Death overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}
