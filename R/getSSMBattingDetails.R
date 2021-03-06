##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 May 2021
# Function: getSSMBattingDetails
# This function creates SSM batting details
#
###########################################################################################
#' @title
#' Gets  the SSM batting details
#'
#' @description
#' This function creates a single datframe of all SSM batsmen
#' @usage
#' getSSMBattingDetails(dir='.',odir=".")
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
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
#' \code{\link{getIPLBattingDetails}}\cr
#' \code{\link{getIPLBowlingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' \code{\link{getNTBBowlingDetails}}\cr
#' @export
#'
getSSMBattingDetails <- function(dir='.',odir=".") {

    currDir= getwd()
    teams <-c("Auckland", "Canterbury", "Central Districts",
              "Northern Districts", "Otago", "Wellington")

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
