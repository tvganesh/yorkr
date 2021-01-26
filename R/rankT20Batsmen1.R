##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankT20Batsmen1
# This function ranks the  batsmen
#
#
###########################################################################################
#' @title
#' Ranks the T20 batsmen
#'
#' @description
#' This function creates a single datframe of all T20 batsmen and then ranks them
#' @usage
#' rankT20Batsmen1(dir='.',odir=".",minMatches=50)
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
#' #
#' t20BatsmanRank <- rankT20Batsmen()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankT20Batsmen1 <- function(dir='.',odir=".",minMatches=50) {

    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
    teams <-c("Australia","India","Pakistan","West Indies", 'Sri Lanka',
              "England", "Bangladesh","Netherlands","Scotland", "Afghanistan",
              "Zimbabwe","Ireland","New Zealand","South Africa","Canada",
              "Bermuda","Kenya","Hong Kong","Nepal","Oman","Papua New Guinea",
              "United Arab Emirates","Namibia","Cayman Islands","Singapore",
              "United States of America","Bhutan","Maldives","Botswana","Nigeria",
              "Denmark","Germany","Jersey","Norway","Qatar","Malaysia","Vanuatu",
              "Thailand")
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

    T20BatsmenRank <- arrange(c,desc(meanRuns),desc(meanSR))
    T20BatsmenRank

}

