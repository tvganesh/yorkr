##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 30 Mar 2016
# Function: saveAllMatchesAllOpposition
# This function saves all matches between all opposition of a  teams as a
# single dataframe
##################################################################################
#' @title
#' Saves matches of all opposition as dataframe
#'
#' @description
#' This function saves all matches agaist all opposition as a single dataframe in the
#' current directory
#'
#' @param dir
#' Directory to store saved matches
#'
#' @usage
#' saveAllMatchesAllOpposition(dir)
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
#' saveAllMatchesBetweenTeams
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

saveAllMatchesAllOpposition <- function(dir) {
    teams <-c("Australia","India","Pakistan","West Indies", 'Sri Lanka',
              "England", "Bangladesh","Netherlands","Scotland", "Afghanistan",
              "Zimbabwe","Ireland","New Zealand","South Africa","Canada",
              "Bermuda","Kenya","Hong Kong","Nepal","Oman","Papua New Guinea",
              "United Arab Emirates")

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




