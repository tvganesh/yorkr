##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 13 Dec 2016
# Function: rankIPLBowlers
# This function creates a dataframe of all batsmen performances and then
# ranks the IPL batsmen
#
###########################################################################################
#' @title
#' Ranks the IPL bowlers
#'
#' @description
#' This function creates a single datframe of all IPL bowlers and then ranks them
#' @usage
#' rankIPLBowlers()
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
rankIPLBowlers <- function() {
    bowlingDetails=bowler=wickets=economyRate=meanWickets=meanER=NULL
    #setwd("C:/software/cricket-package/cricsheet/ipl2016/data")
    #csk_details <- getTeamBowlingDetails("Chennai Super Kings",dir=".", save=TRUE)
    #dc_details <- getTeamBowlingDetails("Deccan Chargers",dir=".", save=TRUE)
    #dd_details <- getTeamBowlingDetails("Delhi Daredevils",dir=".",save=TRUE)
    #kxip_details <- getTeamBowlingDetails("Kings XI Punjab",dir=".",save=TRUE)
    #ktk_details <- getTeamBowlingDetails("Kochi Tuskers Kerala",dir=".",save=TRUE)
    #kkr_details <- getTeamBowlingDetails("Kolkata Knight Riders",dir=".",save=TRUE)
    #mi_details <- getTeamBowlingDetails("Mumbai Indians",dir=".",save=TRUE)
    #pw_details <- getTeamBowlingDetails("Pune Warriors",dir=".",save=TRUE)
    #rr_details <- getTeamBowlingDetails("Rajasthan Royals",dir=".",save=TRUE)
    #rcb_details <- getTeamBowlingDetails("Royal Challengers Bangalore",dir=".",save=TRUE)
    #sh_details <- getTeamBowlingDetails("Sunrisers Hyderabad",dir=".",save=TRUE)
    #gl_details <- getTeamBowlingDetails("Gujarat Lions",dir=".",save=TRUE)
    #rps_details <- getTeamBowlingDetails("Rising Pune Supergiants",dir=".",save=TRUE)

    load("Chennai Super Kings-BowlingDetails.RData")
    csk_details <- bowlingDetails
    load("Deccan Chargers-BowlingDetails.RData")
    dc_details <- bowlingDetails
    load("Delhi Daredevils-BowlingDetails.RData")
    dd_details <- bowlingDetails
    load("Kings XI Punjab-BowlingDetails.RData")
    kxip_details <- bowlingDetails
    load("Kochi Tuskers Kerala-BowlingDetails.RData")
    ktk_details <- bowlingDetails
    load("Kolkata Knight Riders-BowlingDetails.RData")
    kkr_details <- bowlingDetails
    load("Mumbai Indians-BowlingDetails.RData")
    mi_details <- bowlingDetails
    load("Pune Warriors-BowlingDetails.RData")
    pw_details <- bowlingDetails
    load("Rajasthan Royals-BowlingDetails.RData")
    rr_details <- bowlingDetails
    load("Royal Challengers Bangalore-BowlingDetails.RData")
    rcb_details <- bowlingDetails
    load("Sunrisers Hyderabad-BowlingDetails.RData")
    sh_details <- bowlingDetails
    load("Gujarat Lions-BowlingDetails.RData")
    gl_details <- bowlingDetails
    load("Rising Pune Supergiants-BowlingDetails.RData")
    rps_details <- bowlingDetails

    aa <- list(csk_details,dc_details,dd_details,kxip_details,ktk_details,kkr_details,
               mi_details,pw_details,rr_details,rcb_details,sh_details,gl_details,rps_details)

    theTeams <-c("Chennai Super Kings","Deccan Chargers","Delhi Daredevils",
                 "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
                 "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
                 "Royal Challengers Bangalore","Sunrisers Hyderabad",
                 "Gujarat Lions","Rising Pune Supergiants")


    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(x in 1:length(aa)){
        bowlers <- unique(aa[[x]]$bowler)
        for (y in 1:length(bowlers)){
            #cat("x=",x,"team",theTeams[x],"\n")
            tryCatch(l <- getBowlerWicketDetails(team=theTeams[x],name=bowlers[y],dir="."),
                     error = function(e) {
                         #print("Error!")

                     }

            )
            if(exists("l")){

                l1 <- l %>% group_by(bowler,wickets,economyRate) %>%  distinct(date)
                l2 <-summarise(group_by(l1,bowler),matches=n(),meanWickets=mean(wickets),
                               meanER=mean(economyRate))

                o <-rbind(o,l2)
            }

        }
    }

    # Select only players who have played 60 matches or more
    q <- filter(o,matches >= 30)

    IPLBowlersRank <- arrange(q,desc(meanWickets),desc(meanER))
    IPLBowlersRank



}
