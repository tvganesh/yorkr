##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankPSLBatsmen
# This function ranks the PSL batsmen
#
#
###########################################################################################
#' @title
#' Ranks the PSL batsmen
#'
#' @description
#' This function creates a single datframe of all PSL batsmen and then ranks them
#' @usage
#' rankPSLBatsmen(dir='.',odir=".",minMatches=50)
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
#' @return The ranked PSL batsmen
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
#' pslBatsmanRank <- rankPSLBatsmen()
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
rankPSLBatsmen <- function(dir='.',odir=".",minMatches=50) {

    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
    teams <- c("Islamabad United","Karachi Kings", "Lahore Qalandars", "Multan Sultans",
               "Peshawar Zalmi", "Quetta Gladiators")

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
    batsmen <- unique(df$batsman)
    o <- NULL
    n <- data.frame(name=character(0),matches=numeric(0),meanRuns=numeric(0),meanSR=numeric(0))
    for (x in 1:length(batsmen)){
        m <- filter(df,batsman==batsmen[x])
        m <- mutate(m,matches=n(),meanRuns=mean(runs),meanSR=mean(strikeRate))
        m <- select(m,batsman,matches,meanRuns,meanSR)
        n <- m[1,]
        o <- rbind(o,n)
    }

    # Reset to currDir
    setwd(currDir)
    # Select only players who have played 60 matches or more
    p <- filter(o,matches >= minMatches)

    PSLBatsmenRank <- arrange(p,desc(meanRuns),desc(meanSR))
    PSLBatsmenRank

}




