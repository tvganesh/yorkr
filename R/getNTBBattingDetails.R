##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Jan 2021
# Function: getNTBBattingDetails
# This function creates NTB batting details
#
###########################################################################################
#' @title
#' Gets  the NTB batting details
#'
#' @description
#' This function creates a single datframe of all NTB batting
#' @usage
#' getNTBBattingDetails(dir='.',odir=".")
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
#' \code{\link{getIPLBattingDetails}}\cr
#' \code{\link{getIPLBowlingDetails}}\cr
#' @export
#'
getNTBBattingDetails <- function(dir='.',odir=".") {

    currDir= getwd()
    teams <- c("Birmingham Bears","Derbyshire", "Durham", "Essex", "Glamorgan",
               "Gloucestershire", "Hampshire", "Kent","Lancashire",
               "Leicestershire", "Middlesex","Northamptonshire",
               "Nottinghamshire","Somerset","Surrey","Sussex","Warwickshire",
               "Worcestershire","Yorkshire")


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
