##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankNTBBatsmen
# This function ranks the NTB batsmen
#
#
###########################################################################################
#' @title
#' Ranks the NTB batsmen
#'
#' @description
#' This function creates a single datframe of all NTB batsmen and then ranks them
#' @usage
#' rankNTBBatsmen(dir='.',odir=".",minMatches=50)
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
#'
#' @param minMatches
#' Minimum matches
#'
#' @return The ranked NTB batsmen
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}\cr
#' \url{https://github.com/tvganesh/yorkrData}
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' #
#' ntbBatsmanRank <- rankNTBBatsmen()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankNTBBatsmen <- function(dir='.',odir=".",minMatches=50) {

    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
    teams <- c("Birmingham Bears","Derbyshire", "Durham", "Essex", "Glamorgan",
               "Gloucestershire", "Hampshire", "Kent","Lancashire",
               "Leicestershire", "Middlesex","Northamptonshire",
               "Nottinghamshire","Somerset","Surrey","Sussex","Warwickshire",
               "Worcestershire","Yorkshire")

    #Change dir
    setwd(odir)
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


    df <- select(battingDF,batsman,runs,strikeRate)

    b=summarise(group_by(df,batsman),matches=n(), meanRuns=mean(runs),meanSR=mean(strikeRate))
    b[is.na(b)] <- 0
    # Reset to currDir
    setwd(currDir)
    # Select only players based on minMatches
    c <- filter(b,matches >= minMatches)

    NTBBatsmenRank <- arrange(c,desc(meanRuns),desc(meanSR))
    NTBBatsmenRank

}
