##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 3 May 2020
# Function: rankODIBowlers
# This function creates a dataframe of all ODI bowlers performances and then
# ranks the  bowlers
#
###########################################################################################
#' @title
#' Ranks the ODI bowlers
#'
#' @description
#' This function creates a single datframe of all ODI bowlers and then ranks them
#' @usage
#' rankODIBowlers(dir=".",odir=".",minMatches=20)
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
#'
#' @return The ranked ODI bowlers
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
#' odiBowlersRank <- rankODIBowlers()
#' }
#'
#' @seealso
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankIPLBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' @export
#'
rankODIBowlers <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
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
    # Get all bowling details

    details=df=NULL
    teams1 <- NULL
    for(team in teams){
        print(team)
        tryCatch({
            bowling <- getTeamBowlingDetails(team,dir=dir, save=TRUE,odir=odir)
            teams1 <- c(teams1,team)
        },
        error = function(e) {
            print("No data")

        }
        )
    }
    #Change dir
    setwd(odir)


    # Compute wickets by bowler in each team
    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(team1 in teams1){
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
    # Reset to currDir
    setwd(currDir)
    q <- filter(o,matches >= minMatches)
    ODIBowlersRank <- arrange(q,desc(totalWickets),desc(meanER))
    ODIBowlersRank <- distinct(ODIBowlersRank)
    ODIBowlersRank

}
