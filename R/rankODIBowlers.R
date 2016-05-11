##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 May 2016
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
#' rankODIBowlers()
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
rankODIBowlers <- function() {
    # This needs to be done once. After it is done, we can use the RData files
    #setwd("C:/software/cricket-package/york-test/yorkrData/ODI/ODI-matches")
    #afg_details <- getTeamBowlingDetails("Afghanistan",dir=".", save=TRUE)
    #aus_details <- getTeamBowlingDetails("Australia",dir=".", save=TRUE)
    #ind_details <- getTeamBowlingDetails("India",dir=".",save=TRUE)
    #pak_details <- getTeamBowlingDetails("Pakistan",dir=".",save=TRUE)
    #wi_details <- getTeamBowlingDetails("West Indies",dir=".",save=TRUE)
    #sl_details <- getTeamBowlingDetails("Sri Lanka",dir=".",save=TRUE)
    #eng_details <- getTeamBowlingDetails("England",dir=".",save=TRUE)
    #ban_details <- getTeamBowlingDetails("Bangladesh",dir=".",save=TRUE)
    #nth_details <- getTeamBowlingDetails("Netherlands",dir=".",save=TRUE)
    #sco_details <- getTeamBowlingDetails("Scotland",dir=".",save=TRUE)
    #zim_details <- getTeamBowlingDetails("Zimbabwe",dir=".",save=TRUE)
    #ire_details <- getTeamBowlingDetails("Ireland",dir=".",save=TRUE)
    #nz_details <- getTeamBowlingDetails("New Zealand",dir=".",save=TRUE)
    #sa_details <- getTeamBowlingDetails("South Africa",dir=".",save=TRUE)
    #can_details <- getTeamBowlingDetails("Canada",dir=".",save=TRUE)
    #ber_details <- getTeamBowlingDetails("Bermuda",dir=".",save=TRUE)
    #ken_details <- getTeamBowlingDetails("Kenya",dir=".",save=TRUE)

    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=NULL
    load("Afghanistan-BowlingDetails.RData")
    afg_details <- bowlingDetails
    load("Australia-BowlingDetails.RData")
    aus_details <- bowlingDetails
    load("India-BowlingDetails.RData")
    ind_details <- bowlingDetails
    load("Pakistan-BowlingDetails.RData")
    pak_details <- bowlingDetails
    load("West Indies-BowlingDetails.RData")
    wi_details <- bowlingDetails
    load("Sri Lanka-BowlingDetails.RData")
    sl_details <- bowlingDetails
    load("England-BowlingDetails.RData")
    eng_details <- bowlingDetails
    load("Bangladesh-BowlingDetails.RData")
    ban_details <- bowlingDetails
    load("Netherlands-BowlingDetails.RData")
    nth_details <- bowlingDetails
    load("Scotland-BowlingDetails.RData")
    sco_details <- bowlingDetails
    load("Zimbabwe-BowlingDetails.RData")
    zim_details <- bowlingDetails
    load("Ireland-BowlingDetails.RData")
    ire_details <- bowlingDetails
    load("New Zealand-BowlingDetails.RData")
    nz_details <- bowlingDetails
    load("South Africa-BowlingDetails.RData")
    sa_details <- bowlingDetails
    load("Canada-BowlingDetails.RData")
    can_details <- bowlingDetails
    load("Bermuda-BowlingDetails.RData")
    ber_details <- bowlingDetails
    load("Kenya-BowlingDetails.RData")
    ken_details <- bowlingDetails

    aa <- list(aus_details,ind_details,pak_details,wi_details,sl_details,eng_details,
               ban_details,nth_details,sco_details,afg_details,zim_details,
               ire_details,nz_details,sa_details,can_details,ber_details,ken_details)


    theTeams <-c("Australia","India","Pakistan","West Indies", 'Sri Lanka',
              "England", "Bangladesh","Netherlands","Scotland", "Afghanistan",
              "Zimbabwe","Ireland","New Zealand","South Africa","Canada",
              "Bermuda","Kenya")


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

    q <- filter(o,matches >= 20)
    ODIBowlersRank <- arrange(q,desc(meanWickets),desc(meanER))
    ODIBowlersRank


}
