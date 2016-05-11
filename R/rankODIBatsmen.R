##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 May 2016
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
#' rankODIBatsmen()
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
rankODIBatsmen <- function() {
    # This needs to be done once. After it is done, we can use the RData files
    #setwd("C:/software/cricket-package/york-test/yorkrData/ODI/ODI-matches")
    #afg_details <- getTeamBattingDetails("Afghanistan",dir=".", save=TRUE)
    #aus_details <- getTeamBattingDetails("Australia",dir=".", save=TRUE)
    #ind_details <- getTeamBattingDetails("India",dir=".",save=TRUE)
    #pak_details <- getTeamBattingDetails("Pakistan",dir=".",save=TRUE)
    #wi_details <- getTeamBattingDetails("West Indies",dir=".",save=TRUE)
    #sl_details <- getTeamBattingDetails("Sri Lanka",dir=".",save=TRUE)
    #eng_details <- getTeamBattingDetails("England",dir=".",save=TRUE)
    #ban_details <- getTeamBattingDetails("Bangladesh",dir=".",save=TRUE)
    #nth_details <- getTeamBattingDetails("Netherlands",dir=".",save=TRUE)
    #sco_details <- getTeamBattingDetails("Scotland",dir=".",save=TRUE)
    #zim_details <- getTeamBattingDetails("Zimbabwe",dir=".",save=TRUE)
    #ire_details <- getTeamBattingDetails("Ireland",dir=".",save=TRUE)
    #nz_details <- getTeamBattingDetails("New Zealand",dir=".",save=TRUE)
    #sa_details <- getTeamBattingDetails("South Africa",dir=".",save=TRUE)
    #can_details <- getTeamBattingDetails("Canada",dir=".",save=TRUE)
    #ber_details <- getTeamBattingDetails("Bermuda",dir=".",save=TRUE)
    #ken_details <- getTeamBattingDetails("Kenya",dir=".",save=TRUE)

    battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=NULL
    load("Afghanistan-BattingDetails.RData")
    afg_details <- battingDetails
    load("Australia-BattingDetails.RData")
    aus_details <- battingDetails
    load("India-BattingDetails.RData")
    ind_details <- battingDetails
    load("Pakistan-BattingDetails.RData")
    pak_details <- battingDetails
    load("West Indies-BattingDetails.RData")
    wi_details <- battingDetails
    load("Sri Lanka-BattingDetails.RData")
    sl_details <- battingDetails
    load("England-BattingDetails.RData")
    eng_details <- battingDetails
    load("Bangladesh-BattingDetails.RData")
    ban_details <- battingDetails
    load("Netherlands-BattingDetails.RData")
    nth_details <- battingDetails
    load("Scotland-BattingDetails.RData")
    sco_details <- battingDetails
    load("Zimbabwe-BattingDetails.RData")
    zim_details <- battingDetails
    load("Ireland-BattingDetails.RData")
    ire_details <- battingDetails
    load("New Zealand-BattingDetails.RData")
    nz_details <- battingDetails
    load("South Africa-BattingDetails.RData")
    sa_details <- battingDetails
    load("Canada-BattingDetails.RData")
    can_details <- battingDetails
    load("Bermuda-BattingDetails.RData")
    ber_details <- battingDetails
    load("Kenya-BattingDetails.RData")
    ken_details <- battingDetails

    a <- select(afg_details,batsman,runs,strikeRate)
    b <- select(aus_details,batsman,runs,strikeRate)
    c <- select(ban_details,batsman,runs,strikeRate)
    d <- select(ber_details,batsman,runs,strikeRate)
    e <- select(can_details,batsman,runs,strikeRate)
    f <- select(eng_details,batsman,runs,strikeRate)
    g <- select(ind_details,batsman,runs,strikeRate)
    h <- select(ire_details,batsman,runs,strikeRate)
    i <- select(ken_details,batsman,runs,strikeRate)
    j <- select(nth_details,batsman,runs,strikeRate)
    k <- select(nz_details,batsman,runs,strikeRate)
    l <- select(pak_details,batsman,runs,strikeRate)
    m <- select(sa_details,batsman,runs,strikeRate)
    n <- select(sco_details,batsman,runs,strikeRate)
    o <- select(sl_details,batsman,runs,strikeRate)
    p <- select(wi_details,batsman,runs,strikeRate)
    q <- select(zim_details,batsman,runs,strikeRate)


    df <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

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
    p <- filter(o,matches >= 50)

    ODIBatsmenRank <- arrange(p,desc(meanRuns),desc(meanSR))
    ODIBatsmenRank
}

