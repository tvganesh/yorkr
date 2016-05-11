##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 May 2016
# Function: rankIPLBatsmen
# This function creates a dataframe of all IPL batsmen performances and then
# ranks the IPL batsmen
#
###########################################################################################
#' @title
#' Ranks the IPL batsmen
#'
#' @description
#' This function creates a single datframe of all IPL batsmen and then ranks them
#' @usage
#' rankIPLBatsmen()
#'
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
rankIPLBatsmen <- function() {
    #setwd("C:/software/cricket-package/york-test/yorkrData/IPL/IPL-T20-matches")
    #csk_details <- getTeamBattingDetails("Chennai Super Kings",dir=".", save=TRUE)
    #dc_details <- getTeamBattingDetails("Deccan Chargers",dir=".", save=TRUE)
    #dd_details <- getTeamBattingDetails("Delhi Daredevils",dir=".",save=TRUE)
    #kxip_details <- getTeamBattingDetails("Kings XI Punjab",dir=".",save=TRUE)
    #ktk_details <- getTeamBattingDetails("Kochi Tuskers Kerala",dir=".",save=TRUE)
    #kkr_details <- getTeamBattingDetails("Kolkata Knight Riders",dir=".",save=TRUE)
    #mi_details <- getTeamBattingDetails("Mumbai Indians",dir=".",save=TRUE)
    #pw_details <- getTeamBattingDetails("Pune Warriors",dir=".",save=TRUE)
    #rr_details <- getTeamBattingDetails("Rajasthan Royals",dir=".",save=TRUE)
    #rcb_details <- getTeamBattingDetails("Royal Challengers Bangalore",dir=".",save=TRUE)
    #sh_details <- getTeamBattingDetails("Sunrisers Hyderabad",dir=".",save=TRUE)

    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=NULL
    load("Chennai Super Kings-BattingDetails.RData")
    csk_details <- battingDetails
    load("Deccan Chargers-BattingDetails.RData")
    dc_details <- battingDetails
    load("Delhi Daredevils-BattingDetails.RData")
    dd_details <- battingDetails
    load("Kings XI Punjab-BattingDetails.RData")
    kxip_details <- battingDetails
    load("Kochi Tuskers Kerala-BattingDetails.RData")
    ktk_details <- battingDetails
    load("Kolkata Knight Riders-BattingDetails.RData")
    kkr_details <- battingDetails
    load("Mumbai Indians-BattingDetails.RData")
    mi_details <- battingDetails
    load("Pune Warriors-BattingDetails.RData")
    pw_details <- battingDetails
    load("Rajasthan Royals-BattingDetails.RData")
    rr_details <- battingDetails
    load("Royal Challengers Bangalore-BattingDetails.RData")
    rcb_details <- battingDetails
    load("Sunrisers Hyderabad-BattingDetails.RData")
    sh_details <- battingDetails


    a <- select(csk_details,batsman,runs,strikeRate)
    b <- select(dc_details,batsman,runs,strikeRate)
    c <- select(dd_details,batsman,runs,strikeRate)
    d <- select(kxip_details,batsman,runs,strikeRate)
    e <- select(ktk_details,batsman,runs,strikeRate)
    f <- select(kkr_details,batsman,runs,strikeRate)
    g <- select(mi_details,batsman,runs,strikeRate)
    h <- select(pw_details,batsman,runs,strikeRate)
    i <- select(rr_details,batsman,runs,strikeRate)
    j <- select(rcb_details,batsman,runs,strikeRate)
    k <- select(sh_details,batsman,runs,strikeRate)

    df <- rbind(a,b,c,d,e,f,g,h,i,j,k)

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

    # Select only players who have played 60 matches or more
    p <- filter(o,matches >= 60)

    IPLBatsmenRank <- arrange(p,desc(meanRuns),desc(meanSR))
    IPLBatsmenRank
}

