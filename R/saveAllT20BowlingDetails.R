##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 08 Dec 2021
# Function: saveAllT20BowlingDetails
# This function saves all T20 batting Details
#
#
###########################################################################################
#' @title
#' Save all T20 batting details
#'
#' @description
#' This function creates a single dataframe of all T20 batting details
#' @usage
#' saveAllT20BowlingDetails(teamNames,dir=".",odir=".",type="IPL",save=TRUE)
#'
#' @param teamNames
#' The team names
#'
#' @param dir
#' The output directory
#'
#' @param odir
#' The output directory
#'
#' @param type
#' T20 format
#'
#' @param save
#' To save or not
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
#' @examples
#' \dontrun{
#' saveAllT20BowlingDetails(teamNames,dir=".",odir=".",type="IPL",save=TRUE)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
saveAllT20BowlingDetails <- function(teamNames,dir=".",odir=".",type="IPL",save=TRUE) {

    currDir= getwd()
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    teams = unlist(teamNames)

    bowlingDF<-NULL
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

    if(save){
        fl <-paste(odir,"/",type,"-BowlingDetails.RData",sep="")
        print(fl)
        save(bowlingDF,file=fl)
    }
}
