##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Jan 2021
# Function: getCPLBattingDetails
# This function creates CPL batting details
#
###########################################################################################
#' @title
#' Gets  the CPL batting details
#'
#' @description
#' This function creates a single datframe of all CPL batting
#' @usage
#' getCPLBattingDetails(dir='.',odir=".")
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
getCPLBattingDetails <- function(dir='.',odir=".") {

    currDir= getwd()
    teams <-c("Antigua Hawksbills","Barbados Tridents","Guyana Amazon Warriors","Jamaica Tallawahs",
              "St Kitts and Nevis Patriots","St Lucia Zouks","Trinbago Knight Riders")


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
