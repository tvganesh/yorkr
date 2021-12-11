##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankT20Batsmen
# This function ranks the  T20  batsmen
#
#
###########################################################################################
#' @title
#' Ranks the T20 batsmen
#'
#' @description
#' This function creates a single datframe of all T20 batsmen and then ranks them
#' @usage
#' rankT20Batsmen(dir=".",minMatches, dateRange, runsvsSR,type)
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
#' @param runsvsSR
#'  Runs or Strike rate
#'
#' @param type
#' T20 format
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
#' rankT20Batsmen(dir=".",minMatches, dateRange, runsvsSR,type)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankT20Batsmen <- function(dir=".",minMatches, dateRange, runsvsSR,type) {

  cat("Entering rank Batsmen1 \n")
  currDir= getwd()
  cat("T20batman   dir=",currDir,"\n")
  battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=year=NULL

  cat("dir=",dir)
  setwd(dir)
  battingDF<-NULL
  battingDetails <- paste(type,"-BattingDetails.RData",sep="")
  print(battingDetails)
  load(battingDetails)
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

  if(runsvsSR == "Runs over Strike rate"){
    T20BatsmenRank <- arrange(c,desc(meanRuns),desc(meanSR))
  } else if (runsvsSR == "Strike rate over Runs"){
    T20BatsmenRank <- arrange(c,desc(meanSR),desc(meanRuns))
  }
  T20BatsmenRank

}
