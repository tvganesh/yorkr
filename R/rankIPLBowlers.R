##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankIPLBowlers
# This function ranks the IPL bowlers
#
###########################################################################################
#' @title
#' Ranks the IPL bowlers
#'
#' @description
#' This function creates a single datframe of all IPL bowlers and then ranks them
#' @usage
#' rankIPLBowlers(dir='.',odir=".",minMatches=20)
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
rankIPLBowlers <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    teams <-c("Chennai Super Kings","Delhi Capitals", "Deccan Chargers","Delhi Daredevils",
              "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
              "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
              "Royal Challengers Bangalore","Sunrisers Hyderabad","Gujarat Lions",
              "Rising Pune Supergiants")
    currDir= getwd()

    #Change dir
    setwd(odir)
    bowlingDF<-NULL

    # Compute wickets by bowler in each team
    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(team1 in teams){
        print("loop1")
        bowlingDetails <- NULL
        val <- paste(team1,"-BowlingDetails.RData",sep="")
        print(val)
        tryCatch({
            load(val)
            bowlers <- unique(bowlingDetails$bowler)
            for (y in 1:length(bowlers)){
                print("loop2")
                #cat("x=",x,"team",theTeams[x],"\n")
                tryCatch(l <- getBowlerWicketDetails(team=team1,name=bowlers[y],dir="."),
                         error = function(e) {
                             #print("Error!")

                         }

                )
                if(exists("l")){
                    l1 <- l %>% group_by(bowler,wickets,economyRate) %>%  distinct(date)
                    l2 <-summarise(group_by(l1,bowler),matches=n(),totalWickets=sum(wickets),
                                   meanER=mean(economyRate))

                    o <-rbind(o,l2)
                }

            }
        },
        error = function(e) { # Error in load
            print("No data1")
            setNext=TRUE
        }
        )

    }
    setwd(currDir)
    q <- filter(o,matches >= minMatches)
    IPLBowlersRank <- arrange(q,desc(totalWickets),desc(meanER))
    IPLBowlersRank <- distinct(IPLBowlersRank)
    IPLBowlersRank

}
