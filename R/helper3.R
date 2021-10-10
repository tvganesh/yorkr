##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 28 Jan 2021
# Function: helper3
# This function is a helper for computing batting details of team given the year
#
###########################################################################################
#' @title
#' Gets min,max date and min and max matches from dataframe for the year
#'
#' @description
#' This function gets min,max date and min and max matches from dataframe
#' @usage
#' helper3(teamNames,dateRange,odir=".")
#'
#' @param teamNames
#' The team names
#'
#'@param dateRange
#' Date interval to consider
#'
#'
#' @param odir
#' The output directory
#'
#'
#' @return minDate,maxDate, bowingDF
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
helper3<- function(teamNames,dateRange, odir=".") {

    currDir= getwd()

    year=bowler=NULL
    teams = unlist(teamNames)
    #Change dir
    setwd(odir)
    bowlingDF<-NULL


    # Compute wickets by bowler in each team
    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(team1 in teams){
        bowlingDetails <- NULL
        val <- paste(team1,"-BowlingDetails.RData",sep="")
        print(val)
        tryCatch(load(val),
                 error = function(e) {
                     print("No data1")
                     setNext=TRUE
                 }


        )
        details <- bowlingDetails
        bowlingDF <- rbind(bowlingDF,details)
    }
    maxDate= as.Date(max(bowlingDF$date))
    minDate= as.Date(min(bowlingDF$date))

    a=bowlingDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])
    # Compute number of matches played
    b=a %>% select(bowler,date) %>% unique()
    c=summarise(group_by(b,bowler),matches=n())

    minMatches = min(c$matches)
    maxMatches = max(c$matches)
    setwd(currDir)

    cat("max matxhes =", maxMatches)
    return(list(minMatches, maxMatches))

}
