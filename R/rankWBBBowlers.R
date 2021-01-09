##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 05 Jan 2021
# Function: rankWBBBowlers
# This function ranks the WBB bowlers
#
#
###########################################################################################
#' @title
#' Ranks the WBB bowlers
#'
#' @description
#' This function creates a single datframe of all WBB bowlers and then ranks them
#' @usage
#' rankWBBBowlers(dir='.',odir=".",minMatches=20)
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
#' @return The ranked WBB batsmen
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
#' WBBBowlersRank <- rankWBBBowlers()
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
rankWBBBowlers <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    currDir= getwd()
    teams <-c("Adelaide Strikers", "Brisbane Heat", "Hobart Hurricanes",
              "Melbourne Renegades", "Melbourne Stars", "Perth Scorchers", "Sydney Sixers",
              "Sydney Thunder")
    #Change dir
    setwd(odir)
    bowlingDF<-NULL

    # Compute wickets by bowler in each team
    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(team1 in teams){
        bowlingDetails <- NULL
        val <- paste(team1,"-BowlingDetails.RData",sep="")
        print(val)
        tryCatch({
            load(val)
            bowlers <- unique(bowlingDetails$bowler)
            for (y in 1:length(bowlers)){
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
    WBBBowlersRank <- arrange(q,desc(totalWickets),desc(meanER))
    WBBBowlersRank <- distinct(WBBBowlersRank)
    WBBBowlersRank

}
