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
#' rankT20Batsmen(teamNames,odir=".",minMatches, yearSelected, runsvsSR)
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
#' @param yearSelected
#' Selected year
#'
#' @param runsvsSR
#'  Runs or Strike rate
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
#' rankT20Batsmen(teamNames,odir=".",minMatches, yearSelected, runsvsSR)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankT20Batsmen <- function(teamNames,odir=".",minMatches, yearSelected, runsvsSR) {

    cat("Entering rank Batsmen1 \n")
    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=year=NULL
    teams = unlist(teamNames)
    #Change dir
    setwd(odir)
    battingDF<-NULL
    for(team in teams){
        battingDetails <- NULL
        val <- paste(team,"-BattingDetails.RData",sep="")
        print(val)
        tryCatch(load(val),
                 error = function(e) {
                     print("No data1")
                     setNext=TRUE
                 }


        )
        details <- battingDetails
        battingDF <- rbind(battingDF,details)

    }
    print(dim(battingDF))
    maxDate= max(battingDF$date)
    minDate= min(battingDF$date)
    maxYear = lubridate::year(maxDate)
    minYear = lubridate::year(minDate)

    cat("year err=",yearSelected," minMatches=", minMatches," runsVsSR=", runsvsSR,"\n")
    dateValue=as.Date(paste(yearSelected,"-01-01",sep=""))
    if (dateValue < minDate)
        dateValue=minDate
    df=battingDF %>% filter(date > as.Date(dateValue))

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
