##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: overallRunsSRPPowerplayPlotT20
# This function plots Runs vs SR in power play of T20 batsmen
#
#
###########################################################################################
#' @title
#' Plot the Runs vs SR in power play  of T20 batsmen
#'
#' @description
#' Runs vs SR in power play  of  T20 batsmen
#'
#' @usage
#' overallRunsSRPPowerplayPlotT20(dir=".", dateRange,type="IPL",plot=1)
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
#' overallRunsSRPPowerplayPlotT20(dir=".", dateRange,type="IPL",plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallRunsSRPPowerplayPlotT20 <- function(dir=".", dateRange,type="IPL",plot=1){
  team=ball=totalRuns=total=t20MDF=str_extract=batsman=quantile=SRPowerPlay=quadrant=runs=NULL
  ggplotly=NULL

  fl <- paste(dir,"/",type,"-MatchesDataFrame.RData",sep="")
  load(fl)

  # Filter by date range
  df=t20MDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])


  a1 <-  df%>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,ball,totalRuns,batsman,date)
  a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRPowerPlay=runs/count*100)


  x_lower <- quantile(a3$runs,p=0.66)
  y_lower <- quantile(a3$SRPowerPlay,p=0.66)

  plot.title <- paste("Overall Runs vs SR in Power play in ",type,sep="")
  if(plot == 1){ #ggplot2
    a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRPowerPlay > y_lower   ~ "Q1",
                                  runs <= x_lower & SRPowerPlay > y_lower  ~ "Q2",
                                  runs <= x_lower & SRPowerPlay <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRPowerPlay,color=quadrant)) +
      geom_text(aes(runs,SRPowerPlay,label=batsman,color=quadrant)) + geom_point() +
      xlab("Runs - Power play") + ylab("Strike rate - Power play") +
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
      xlab("Runs - Power play") + ylab("Strike rate - Power play") +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }
}
