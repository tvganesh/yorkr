##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: helper
# This function ranks the IPL batsmen
#
###########################################################################################
#' @title
#' Gets min and max date from dataframe
#'
#' @description
#' This function creates a single datframe of all IPL batsmen and then ranks them
#' @usage
#' helper(teamNames,odir=".")
#'
#' @param teamNames
#' The team names
#'
#' @param odir
#' The output directory
#'
#'
#' @return minDate,maxDate, battingDF
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
#' iplBatsmanRank <- rankIPLBatsmen()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
helper<- function(teamNames,odir=".") {


    cat("Dir helper=====",getwd(),"\n")
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
    cat("Dir helper A=====",getwd(),"\n")
    maxDate= max(battingDF$date)
    minDate= min(battingDF$date)
    maxYear = year(maxDate)
    minYear = year(minDate)

    df <- select(battingDF,batsman,runs,strikeRate)

    b=summarise(group_by(df,batsman),matches=n())
    minMatches = min(b$matches)
    maxMatches = max(b$matches)
    setwd(currDir)

    #a=battingDF %>% filter(date > as.Date("2018-02-01"))
    return(list(minYear,maxYear,minMatches, maxMatches, battingDF))

}
