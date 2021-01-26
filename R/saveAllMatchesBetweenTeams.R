##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 May 2020
# Function: saveAllMatchesBetweenTeams
# This function saves all matches between 2 teams as a single dataframe
##################################################################################
#' @title
#' Saves all matches between 2 teams as  dataframe
#'
#' @description
#' This function saves all matches between 2 teams as a single dataframe in the
#' current directory
#'
#' @usage
#' saveAllMatchesBetweenTeams(dir=".",odir=".")
#'
#' @param dir
#' Input Directory to store saved matches
#'
#' @param odir
#' Output Directory to store matches between teams
#'
#' @return None
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#'
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' saveAllMatchesBetweenTeams(dir=".",odir=".")
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

saveAllMatchesBetweenTeams <- function(dir=".",odir="."){

    teams <-c("Australia","India","Pakistan","West Indies", 'Sri Lanka',
              "England", "Bangladesh","Netherlands","Scotland", "Afghanistan",
              "Zimbabwe","Ireland","New Zealand","South Africa","Canada",
              "Bermuda","Kenya","Hong Kong","Nepal","Oman","Papua New Guinea",
              "United Arab Emirates","Namibia","Cayman Islands","Singapore",
              "United States of America","Bhutan","Maldives","Botswana","Nigeria",
              "Denmark","Germany","Jersey","Norway","Qatar","Malaysia","Vanuatu",
              "Thailand")

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

