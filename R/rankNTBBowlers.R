##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 12 May 2020
# Function: rankNTBBowlers
# This function creates a dataframe of all batsmen performances and then
# ranks the NTB bowlers
#
###########################################################################################
#' @title
#' Ranks the NTB bowlers
#'
#' @description
#' This function creates a single datframe of all NTB bowlers and then ranks them
#' @usage
#' rankNTBBowlers(dir='.',odir=".",minMatches=20)
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
#' ntbBowlersRank <- rankNTBBowlers()
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
rankNTBBowlers <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    wicketPlayerOut=opposition=venue=NULL
    currDir= getwd()
    teams <- c("Birmingham Bears","Derbyshire", "Durham", "Essex", "Glamorgan",
               "Gloucestershire", "Hampshire", "Kent","Lancashire",
               "Leicestershire", "Middlesex","Northamptonshire",
               "Nottinghamshire","Somerset","Surrey","Sussex","Warwickshire",
               "Worcestershire","Yorkshire")

    #Change dir
    setwd(odir)
    bowlingDF<-NULL

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
    # Compute number of matches played
    a=bowlingDF %>% select(bowler,date) %>% unique()
    b=summarise(group_by(a,bowler),matches=n())

    # Compute wickets
    c <- filter(bowlingDF,wicketPlayerOut != "nobody")
    d <- select(c,bowler,wicketPlayerOut,economyRate,date,opposition,venue)
    e <- summarise(group_by(d,bowler,date,economyRate),wickets=length(unique(wicketPlayerOut)))
    f=summarise(group_by(e,bowler), totalWickets=sum(wickets),meanER=mean(economyRate))

    # Join
    g=merge(b,f,by="bowler",all.x = TRUE)
    g[is.na(g)] <- 0
    h <- filter(g,matches >= minMatches)
    setwd(currDir)
    NTBBowlersRank <- arrange(h,desc(totalWickets),desc(meanER))
    NTBBowlersRank <- distinct(NTBBowlersRank)
    NTBBowlersRank

}
