##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 04 Dec 2021
# Function: saveAllT20BattingDetails
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
#' saveAllT20BattingDetails(teamNames,dir=".",odir=".",type="IPL",save=TRUE)
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
#'
#' @return The ranked T20 batsmen
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
#' saveAllT20BattingDetails(teamNames,dir=".",odir=".",type="IPL",save=TRUE)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
saveAllT20BattingDetails <- function(teamNames,dir=".",odir=".",type="IPL",save=TRUE) {

    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=year=NULL
    teams = unlist(teamNames)
    #Change dir

    battingDF<-NULL
    for(team in teams){
        battingDetails <- NULL
        val <- paste(team,"-BattingDetails.RData",sep="")
        print(val)
        tryCatch(load(val),
                 error = function(e) {
                     print("No data1")
                     setNext=TRUE
                 }


        )
        details <- battingDetails
        battingDF <- rbind(battingDF,details)

    }

    if(save){
        fl <-paste(odir,"/",type,"-BattingDetails.RData",sep="")
        print(fl)
        save(battingDF,file=fl)
    }
}
