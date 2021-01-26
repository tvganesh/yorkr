##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Jan 2021
# Function: getBBLBattingDetails
# This function creates BBL batting details
#
###########################################################################################
#' @title
#' Gets  the BBL batting details
#'
#' @description
#' This function creates a single datframe of all BBL batsmen
#' @usage
#' getBBLBattingDetails(dir='.',odir=".")
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
getBBLBattingDetails <- function(dir='.',odir=".") {

    currDir= getwd()
    teams <-c("Adelaide Strikers", "Brisbane Heat", "Hobart Hurricanes",
              "Melbourne Renegades", "Melbourne Stars", "Perth Scorchers", "Sydney Sixers",
              "Sydney Thunder")


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
