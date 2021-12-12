##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Nov 2021
# Function: overallRunsSRMiddleOversPlotT20
# This function plots Runs vs SR in middle overs Intl.  T20  batsmen
#
#
###########################################################################################
#' @title
#' Plot the Runs vs SR in  middle overs  of Intl. T20 batsmen
#'
#' @description
#' Runs vs SR in  middle overs  of Intl. T20 batsmen
#'
#' @usage
#' overallRunsSRMiddleOversPlotT20(dir=".", dateRange,type="IPL",plot=1)
#'
#' @param dir
#' The input directory
#'
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
#' overallRunsSRMiddleOversPlotT20(dir=".", dateRange,type="IPL",plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallRunsSRMiddleOversPlotT20 <- function(dir=".",dateRange,type="IPL",plot=1) {
  team=ball=totalRuns=total=t20MDF=str_extract=batsman=quantile=SRMiddleOvers=quadrant=runs=NULL
  ggplotly=NULL

  fl <- paste(dir,"/",type,"-MatchesDataFrame.RData",sep="")
  load(fl)


  # Filter by date range
  df=t20MDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])


  a1 <- df %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  a2 <- select(a1,ball,totalRuns,batsman,date)
  a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRMiddleOvers=runs/count*100)

  x_lower <- 1/3*  min(a3$runs + max(a3$runs))
  y_lower <- 1/3 * min(a3$SRMiddleOvers + max(a3$SRMiddleOvers))

  plot.title <- paste("Top batsmen in Middle overs Intl. T20 (men)")
  if(plot == 1){ #ggplot2
    a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRMiddleOvers > y_lower   ~ "Q1",
                                  runs <= x_lower & SRMiddleOvers > y_lower  ~ "Q2",
                                  runs <= x_lower & SRMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRMiddleOvers,color=quadrant)) +
      geom_text(aes(runs,SRMiddleOvers,label=batsman,color=quadrant)) + geom_point() +
      xlab("Runs - Middle overs") + ylab("Strike rate - Middle overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRMiddleOvers > y_lower   ~ "Q1",
                                  runs <= x_lower & SRMiddleOvers > y_lower  ~ "Q2",
                                  runs <= x_lower & SRMiddleOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRMiddleOvers,color=quadrant)) +
      geom_text(aes(runs,SRMiddleOvers,label=batsman,color=quadrant)) + geom_point() +
      xlab("Runs - Middle overs") + ylab("Strike rate - Middle overs") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }


}
