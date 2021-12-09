##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Nov 2021
# Function: overallRunsSRPlotT20M
# This function plots Runs vs SR of Intl.  T20  batsmen
#
#
###########################################################################################
#' @title
#' Plot the Runs vs SR of Intl. T20 batsmen
#'
#' @description
#' This function creates a single datframe of all T20 batsmen and then ranks them
#'
#' @usage
#' overallRunsSRPlotT20M(dir=".",minMatches, dateRange,type="IPL",plot=1)
#'
#'
#' @param dir
#' The input directory
#'
#' @param minMatches
#' Minimum matches played
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
#' overallRunsSRPlotT20M(dir=".",minMatches, dateRange,type="IPL",plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallRunsSRPlotT20M <- function(dir=".",minMatches, dateRange,type="IPL",plot=1) {


  currDir= getwd()
  cat("T20batmandir=",currDir,"\n")
  battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=year=NULL

  setwd(dir)
  battingDF<-NULL
  battingDetails <- paste(type,"-BattingDetails.RData",sep="")
  print(battingDetails)
  load(battingDetails)
  print(dim(battingDF))

  print(dim(battingDF))
  print(names(battingDF))
  # Note: If the date Range is NULL setback to root directory
  tryCatch({

    df=battingDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])

  },
  warning=function(war)
  {
    print(paste("NULL values: ", war))
  },
  error=function(err)
  {
    # Change to root directory on error
    setwd(currDir)
    cat("Back to root",getwd(),"\n")
  })

  df1 <- select(df,batsman,runs,strikeRate)
  df1 <- distinct(df1)

  b=summarise(group_by(df1,batsman),matches=n(), meanRuns=mean(runs),meanSR=mean(strikeRate))
  print(dim(b))
  b[is.na(b)] <- 0

  c <- filter(b,matches >= minMatches)
  # Reset to currDir
  setwd(currDir)

  x_lower <- quantile(c$meanRuns,p=0.66)
  y_lower <- quantile(c$meanSR,p=0.66)

  print("!!!!!!!!!!!!!!!!!!!!!!!!!")
  print(plot)
  print("!!!!!!!!!!!!!!!!!!!!!!!!!")
  plot.title <- paste("Runs vs SR of batsmen in ",type)
  if(plot == 1){ #ggplot2
    c %>%
      mutate(quadrant = case_when(meanRuns > x_lower & meanSR > y_lower   ~ "Q1",
                                  meanRuns <= x_lower & meanSR > y_lower  ~ "Q2",
                                  meanRuns <= x_lower & meanSR <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(meanRuns,meanSR,color=quadrant)) +
      geom_text(aes(meanRuns,meanSR,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  c %>%
      mutate(quadrant = case_when(meanRuns > x_lower & meanSR > y_lower   ~ "Q1",
                                  meanRuns <= x_lower & meanSR > y_lower  ~ "Q2",
                                  meanRuns <= x_lower & meanSR <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(meanRuns,meanSR,color=quadrant)) +
      geom_text(aes(meanRuns,meanSR,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }

}
