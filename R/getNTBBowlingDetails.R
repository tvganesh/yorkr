##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Jan 2021
# Function: getNTBBowlingDetails
# This function creates NTB bowling details
#
###########################################################################################
#' @title
#' Gets  the NTB bowling details
#'
#' @description
#' This function creates a single datframe of all NTB bowling
#' @usage
#' getNTBBowlingDetails(dir='.',odir=".")
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
#'
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
#' \code{\link{getIPLBattingDetails}}\cr
#' \code{\link{getIPLBowlingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' @export
#'
getNTBBowlingDetails <- function(dir='.',odir=".") {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    currDir= getwd()
    teams <- c("Birmingham Bears","Derbyshire", "Durham", "Essex", "Glamorgan",
               "Gloucestershire", "Hampshire", "Kent","Lancashire",
               "Leicestershire", "Middlesex","Northamptonshire",
               "Nottinghamshire","Somerset","Surrey","Sussex","Warwickshire",
               "Worcestershire","Yorkshire")


    # Get all bowling details

    details=df=NULL
    teams1 <- NULL
    for(team in teams){
        print(team)
        tryCatch({
            bowling <- getTeamBowlingDetails(team,dir=dir, save=TRUE,odir=odir)
            teams1 <- c(teams1,team)
        },
        error = function(e) {
            print("No data")

        }
        )
    }
}
