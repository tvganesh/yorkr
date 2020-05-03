##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 3 May 2020
# Function: rankODIBatsmen
# This function creates a dataframe of all T20 batsmen performances and then
# ranks the  batsmen
#
###########################################################################################
#' @title
#' Ranks the ODI batsmen
#'
#' @description
#' This function creates a single datframe of all ODI batsmen and then ranks them
#' @usage
#' rankODIBatsmen(dir='.',odir=".")
#'
#' @param dir
#' The input directory
#'
#' @param odir
#' The output directory
#'
#'
#' @return The ranked ODI batsmen
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
#' odiBatsmanRank <- rankODIBatsmen()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankODIBatsmen <- function(dir='.',odir=".") {
    # This needs to be done once. After it is done, we can use the RData files
    currDir= getwd()
    teams <-c("Australia","India","Pakistan","West Indies", 'Sri Lanka',
              "England", "Bangladesh","Netherlands","Scotland", "Afghanistan",
              "Zimbabwe","Ireland","New Zealand","South Africa","Canada",
              "Bermuda","Kenya","Hong Kong","Nepal","Oman","Papua New Guinea",
              "United Arab Emirates","Namibia","Cayman Islands","Singapore",
              "United States of America","Bhutan","Maldives","Botswana","Nigeria",
              "Denmark","Germany","Jersey","Norway","Qatar","Malaysia","Vanuatu",
              "Thailand")

    #teams <- c("Australia","India","Singapore","West Indies")

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
    #Change dir
    setwd(odir)
    battingDF<-NULL
    for(team in teams1){
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
    p <- filter(o,matches >= 50)

    ODIBatsmenRank <- arrange(p,desc(meanRuns),desc(meanSR))
    ODIBatsmenRank

}
