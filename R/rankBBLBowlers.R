##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 May 2020
# Function: rankBBLLBowlers
# This function creates a dataframe of all batsmen performances and then
# ranks the BBL batsmen
#
###########################################################################################
#' @title
#' Ranks the BBL bowlers
#'
#' @description
#' This function creates a single datframe of all BBL bowlers and then ranks them
#' @usage
#' rankBBLBowlers(dir='.',odir=".",minMatches=20)
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
#' @return The ranked BBL bowlers
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
#' bblBowlersRank <- rankBBLBowlers()
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
rankBBLBowlers <- function(dir='.',odir=".",minMatches=20) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=NULL
    currDir= getwd()
    teams <-c("Adelaide Strikers", "Brisbane Heat", "Hobart Hurricanes",
              "Melbourne Renegades", "Melbourne Stars", "Perth Scorchers", "Sydney Sixers",
              "Sydney Thunder")


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
    bowlingDF<-NULL

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
    setwd(currDir)
    q <- filter(o,matches >= minMatches)
    BBLBowlersRank <- arrange(q,desc(totalWickets),desc(meanER))
    BBLBowlersRank

}
