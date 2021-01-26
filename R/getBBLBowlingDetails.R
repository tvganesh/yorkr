##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Jan 2021
# Function: getBBLBowlingDetails
# This function creates BBL bowling details
#
###########################################################################################
#' @title
#' Gets  the BBL bowling details
#'
#' @description
#' This function creates a single datframe of all BBL bowling
#' @usage
#' getBBLBowlingDetails(dir='.',odir=".")
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
#' \code{\link{getIPLBattingDetails}}\cr
#' \code{\link{getIPLBowlingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' \code{\link{getNTBBattingDetails}}\cr
#' @export
#'
getBBLBowlingDetails <- function(dir='.',odir=".") {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    currDir= getwd()
    teams <-c("Adelaide Strikers", "Brisbane Heat", "Hobart Hurricanes",
              "Melbourne Renegades", "Melbourne Stars", "Perth Scorchers", "Sydney Sixers",
              "Sydney Thunder")


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
