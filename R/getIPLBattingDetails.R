##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Jan 2021
# Function: getIPLBattingDetails
# This function creates IPL batting details
#
###########################################################################################
#' @title
#' Gets  the IPL batting details
#'
#' @description
#' This function creates a single datframe of all IPL batting
#' @usage
#' getIPLBattingDetails(dir='.',odir=".")
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
#'
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
#'
#' @seealso
#' \code{\link{getBBLBattingDetails}}\cr
#' \code{\link{getBBLBowlingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' \code{\link{getNTBBowlingDetails}}\cr
#' @export
#'
getIPLBattingDetails <- function(dir='.',odir=".") {

    currDir= getwd()
    teams <-c("Chennai Super Kings","Delhi Capitals", "Deccan Chargers","Delhi Daredevils",
              "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
              "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
              "Royal Challengers Bangalore","Sunrisers Hyderabad","Gujarat Lions",
              "Rising Pune Supergiants")


    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
    details=df=NULL
    teams1 <- NULL
    for(team in teams){
        print(team)
        tryCatch({
            batting <- getTeamBattingDetails(team,dir=dir, save=TRUE,odir=odir)
            teams1 <- c(teams1,team)
        },
        error = function(e) {
            print("No data")

        }
        )
    }

}
