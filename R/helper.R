##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Jan 2021
# Function: helper
# This function is a helper for computing batting details of team
#
###########################################################################################
#' @title
#' Gets min,max date and min and max matches from dataframe
#'
#' @description
#' This function gets min,max date and min and max matches from dataframe
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
#' @return minDate,maxDate, minMatches, maxMatches
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
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
helper<- function(teamNames,dir=".",type="IPL") {
    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
    year=NULL
    print("Helper")
    print(getwd())
    setwd(dir)
    print(getwd())
    teams = unlist(teamNames)

    battingDF<-NULL
    battingDetails <- paste(type,"-BattingDetails.RData",sep="")
    print(battingDetails)
    load(battingDetails)
    print(dim(battingDF))
    print(names(battingDF))
    cat("Dir helper =====",getwd(),"\n")
    maxDate= as.Date(max(battingDF$date))
    minDate= as.Date(min(battingDF$date))
    print(minDate,maxDate)

    df <- select(battingDF,batsman,runs,strikeRate)

    b=summarise(group_by(df,batsman),matches=n())
    minMatches = min(b$matches)
    maxMatches = max(b$matches)
    setwd(currDir)



    cat("Helper **********************************************\n")
    return(list(minDate,maxDate,minMatches, maxMatches))

}
