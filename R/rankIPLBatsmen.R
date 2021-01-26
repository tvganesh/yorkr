##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankIPLBatsmen
# This function ranks the IPL batsmen
#
###########################################################################################
#' @title
#' Ranks the IPL batsmen
#'
#' @description
#' This function creates a single datframe of all IPL batsmen and then ranks them
#' @usage
#' rankIPLBatsmen(dir='.',odir=".",minMatches=50)
#'
#'
#' @param odir
#' The  directory
#'
#' @param dateValue
#' Minimum matches
#'
#' @return The ranked IPL batsmen
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
#' iplBatsmanRank <- rankIPLBatsmen()
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
rankIPLBatsmen <- function(odir=".",minMatches=1, yearSelected="2020", runsvsSR="Runs over SR") {

    cat("Entering rank Batsmen1 \n")
    currDir= getwd()
    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=NULL
    teams <-c("Chennai Super Kings","Delhi Capitals", "Deccan Chargers","Delhi Daredevils",
              "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
              "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
              "Royal Challengers Bangalore","Sunrisers Hyderabad","Gujarat Lions",
              "Rising Pune Supergiants")
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
    print(dim(battingDF))
    maxDate= max(battingDF$date)
    minDate= min(battingDF$date)
    maxYear = year(maxDate)
    minYear = year(minDate)
    if(is.null(yearSelected) | length(yearSelected)==0){
        print("Here")
        return
    }
    cat("year err=",yearSelected," minMatches=", minMatches," runsVsSR=", runsvsSR,"\n")
    dateValue=as.Date(paste(yearSelected,"-01-01",sep=""))
    if (dateValue < minDate)
        dateValue=minDate
    df=battingDF %>% filter(date > as.Date(dateValue))

    df1 <- select(df,batsman,runs,strikeRate)

    b=summarise(group_by(df1,batsman),matches=n(), meanRuns=mean(runs),meanSR=mean(strikeRate))
    print(dim(b))
    b[is.na(b)] <- 0

    c <- filter(b,matches >= minMatches)
    # Reset to currDir
    setwd(currDir)

    if(runsvsSR == "Runs over SR"){
        IPLBatsmenRank <- arrange(c,desc(meanRuns),desc(meanSR))
    } else if (runsvsSR == "SR over Runs"){
        IPLBatsmenRank <- arrange(c,desc(meanSR),desc(meanRuns))
    }
    IPLBatsmenRank

}

