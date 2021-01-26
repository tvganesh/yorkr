##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankT20Batsmen
# This function ranks the  batsmen
#
#
###########################################################################################
#' @title
#' Ranks the T20 batsmen
#'
#' @description
#' This function creates a single datframe of all T20 batsmen and then ranks them
#' @usage
#' rankT20Batsmen(dir='.',odir=".",minMatches=50)
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
#'
#' @param minMatches
#' Minimum matches
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
#' #
#' t20BatsmanRank <- rankT20Batsmen()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankT20Batsmen <- function(teamNames,odir=".",minMatches, yearSelected, runsvsSR) {

    cat("Entering rank Batsmen1 \n")
    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
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
    maxYear = year(maxDate)
    minYear = year(minDate)
    if(is.null(yearSelected) | length(yearSelected)==0){
        print("Here")
        return
    }
    cat("year err=",yearSelected," minMatches=", minMatches," runsVsSR=", runsvsSR,"\n")
    dateValue=as.Date(paste(yearSelected,"-01-01",sep=""))
    if (dateValue < minDate)
        dateValue=minDate
    df=battingDF %>% filter(date > as.Date(dateValue))

    df1 <- select(df,batsman,runs,strikeRate)

    b=summarise(group_by(df1,batsman),matches=n(), meanRuns=mean(runs),meanSR=mean(strikeRate))
    print(dim(b))
    b[is.na(b)] <- 0

    c <- filter(b,matches >= minMatches)
    # Reset to currDir
    setwd(currDir)

    if(runsvsSR == "Runs over SR"){
        T20BatsmenRank <- arrange(c,desc(meanRuns),desc(meanSR))
    } else if (runsvsSR == "SR over Runs"){
        T20BatsmenRank <- arrange(c,desc(meanSR),desc(meanRuns))
    }
    T20BatsmenRank

}
