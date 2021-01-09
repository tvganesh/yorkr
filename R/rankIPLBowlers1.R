##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankIPLBowlers1
# This function ranks the IPL bowlers
#
###########################################################################################
#' @title
#' Ranks the IPL bowlers1
#'
#' @description
#' This function creates a single datframe of all IPL bowlers and then ranks them
#' @usage
#' rankIPLBowlers1(dir='.',odir=".",minMatches=20)
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
#' @return The ranked IPL batsmen
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
#' iplBowlersRank <- rankIPLBowlers()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBatsmen}}\cr
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankIPLBowlers1 <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    teams <-c("Chennai Super Kings","Delhi Capitals", "Deccan Chargers","Delhi Daredevils",
              "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
              "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
              "Royal Challengers Bangalore","Sunrisers Hyderabad","Gujarat Lions",
              "Rising Pune Supergiants")
    currDir= getwd()
    print("new bowlers1111")
    #Change dir
    setwd(odir)
    bowlingDF<-NULL
    print("loop1")
    # Compute wickets by bowler in each team
    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
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
    b <- filter(bowlingDF,wicketPlayerOut != "nobody")
    c <- select(b,bowler,wicketPlayerOut,economyRate,date,opposition,venue)
    d <- summarise(group_by(c,bowler,date,economyRate),wickets=length(unique(wicketPlayerOut)))
    e=summarise(group_by(d,bowler),matches=n(), totalWickets=sum(wickets),meanER=mean(economyRate))

    f <- filter(e,matches >= minMatches)
    IPLBowlersRank <- arrange(f,desc(totalWickets),desc(meanER))
    IPLBowlersRank <- distinct(IPLBowlersRank)
    IPLBowlersRank

}
