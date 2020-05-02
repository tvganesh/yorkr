##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 May 2020
# Function: saveAllMatchesBetween2IPLTeams
# This function saves all matches between 2 teams as a single dataframe
##################################################################################
#' @title
#' Saves all matches between 2 IPL teams as  dataframe
#'
#' @description
#' This function saves all matches between 2 IPL teams as a single dataframe in the
#' current directory
#'
#' @usage
#' saveAllMatchesBetween2IPLTeams(dir=".",odir=".")
#'
#' @param dir
#' Input Directory
#'
#' @param odir
#' Output Directory to store saved matches
#'
#' @return None
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}\cr
#' \url{https://github.com/tvganesh/yorkrData}
#'
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' saveAllMatchesBetween2IPLTeams(dir=".",odir=".")
#' }
#' @seealso
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{batsmanRunsVsDeliveries}}\cr
#' \code{\link{batsmanRunsVsStrikeRate}}\cr
#' \code{\link{getAllMatchesAllOpposition}}\cr
#' \code{\link{getAllMatchesBetweenTeams}}\cr
#'
#' @export
#'

saveAllMatchesBetween2IPLTeams <- function(dir=".",odir="."){

    teams <-c("Chennai Super Kings","Deccan Chargers","Delhi Daredevils",
              "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
              "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
              "Royal Challengers Bangalore","Sunrisers Hyderabad","Gujarat Lions",
              "Rising Pune Supergiants")

    #teams <-c("Bermuda","Kenya")

    matches <- NULL
    #Create all combinations of teams
    for(i in seq_along(teams)){
        for(j in seq_along(teams)){
            if(teams[i] != teams[j]){
                cat("Team1=",teams[i],"Team2=",teams[j],"\n")
                tryCatch(matches <- getAllMatchesBetweenTeams(teams[i],teams[j],dir=dir,save=TRUE,odir=odir),
                         error = function(e) {
                             print("No matches")

                         }
                )
            }
        }
        matches <- NULL
    }
}

