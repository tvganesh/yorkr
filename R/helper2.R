##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: helper2
# This function ranks the IPL batsmen
#
###########################################################################################
#' @title
#' Gets min and max date from dataframe
#'
#' @description
#' This function creates a single datframe of all IPL batsmen and then ranks them
#' @usage
#' helper2(teamNames,yearValue,odir=".")
#'
#' @param teamNames
#' The team names
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
helper2<- function(teamNames, odir=".") {

    currDir= getwd()
    cat("Dir helper2=====",getwd()," odir=", odir, "\n")
    teams = unlist(teamNames)
    #Change dir
    setwd(odir)
    cat("Dir helper2=====",getwd(),"\n")
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
    maxDate= max(bowlingDF$date)
    minDate= min(bowlingDF$date)
    maxYear = year(maxDate)
    minYear = year(minDate)

    # Compute number of matches played
    a=bowlingDF %>% select(bowler,date) %>% unique()
    b=summarise(group_by(a,bowler),matches=n())

    minMatches = min(b$matches)
    maxMatches = max(b$matches)
    setwd(currDir)

    #a=battingDF %>% filter(date > as.Date("2018-02-01"))
    return(list(minYear,maxYear,minMatches, maxMatches, bowlingDF))

}
