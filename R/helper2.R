##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 28 Jan 2021
# Function: helper2
# his function is a helper for computing batting details of team
#
###########################################################################################
#' @title
#' Gets min,max date and min and max matches from dataframe
#'
#'
#' @description
#' This function gets min,max date and min and max matches from dataframe
#'
#' @usage
#' helper2(teamNames,dir=".",type="IPL")
#'
#'
#' @param teamNames
#' The team names
#'
#' @param odir
#' The output directory
#'
#' @param type
#' T20 format
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
helper2<- function(teamNames,dir=".",type="IPL") {

    currDir= getwd()
    setwd(dir)
    year=bowler=NULL
    teams = unlist(teamNames)

    cat("Dir helper2=====",getwd(),"Dir=", dir, "\n")
    bowlingDF<-NULL

    bowlingDetails <- paste(type,"-BowlingDetails.RData",sep="")
    print(bowlingDetails)
    load(bowlingDetails)

    maxDate= as.Date(max(bowlingDF$date))
    minDate= as.Date(min(bowlingDF$date))
    print(minDate,maxDate)

    # Compute number of matches played
    a=bowlingDF %>% select(bowler,date) %>% unique()
    b=summarise(group_by(a,bowler),matches=n())

    minMatches = min(b$matches)
    maxMatches = max(b$matches)
    setwd(currDir)

    #a=battingDF %>% filter(date > as.Date("2018-02-01"))
    return(list(minDate,maxDate,minMatches, maxMatches))

}
