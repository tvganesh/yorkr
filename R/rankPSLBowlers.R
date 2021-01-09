##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date :  05 Jan 2021
# Function: rankPSLBowlers
# This function ranks the PSL bowlers
#
#
###########################################################################################
#' @title
#' Ranks the PSL bowlers
#'
#' @description
#' This function creates a single datframe of all PSL bowlers and then ranks them
#' @usage
#' rankPSLBowlers(dir='.',odir=".",minMatches=20)
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
#' pslBowlersRank <- rankPSLBowlers()
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
rankPSLBowlers <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    wicketPlayerOut=opposition=venue=NULL
    currDir= getwd()
    teams <- c("Islamabad United","Karachi Kings", "Lahore Qalandars", "Multan Sultans",
               "Peshawar Zalmi", "Quetta Gladiators")


    # Get all bowling details
    details=df=NULL
    teams1 <- NULL

    #Change dir
    setwd(odir)
    bowlingDF<-NULL

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
    PSLBowlersRank <- arrange(h,desc(totalWickets),desc(meanER))
    PSLBowlersRank <- distinct(PSLBowlersRank)
    PSLBowlersRank

}
