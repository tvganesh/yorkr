##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Mar 2016
# Function: saveAllMatchesAllOppositionIPLT20
# This function saves all IPL matches between a team and all opposition as a
# single dataframe
##################################################################################
#' @title
#' Saves matches against all IPL teams as dataframe for an IPL team
#'
#' @description
#' This function saves all IPL matches agaist all opposition as a single dataframe in the
#' current directory
#'
#' @usage
#' saveAllMatchesAllOppositionIPLT20(dir)
#'
#' @param dir
#' Directory to store saved matches
#'
#' @return None
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
#' saveAllMatchesAllOppositionT20
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

saveAllMatchesAllOppositionIPLT20 <- function(dir) {

    teams <-c("Chennai Super Kings","Deccan Chargers","Delhi Daredevils",
              "Kings XI Punjab", 'Kochi Tuskers Kerala',"Kolkata Knight Riders",
              "Mumbai Indians", "Pune Warriors","Rajasthan Royals",
              "Royal Challengers Bangalore","Sunrisers Hyderabad","Gujarat Lions",
              "Rising Pune Supergiants")

    matches <- NULL
    for(i in seq_along(teams)){
        cat("Team1=",teams[i],"\n")
        tryCatch(matches <- getAllMatchesAllOpposition(teams[i],dir=dir,save=TRUE),
                 error = function(e) {
                     print("No matches")

                 }
        )
        matches <- NULL
    }
}




