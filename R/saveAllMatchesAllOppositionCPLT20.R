##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Jan 2021
# Function: saveAllMatchesAllOppositionCPLT20
# This function saves all CPL matches between a team and all opposition as a
# single dataframe
##################################################################################
#' @title
#' Saves matches against all CPL teams as dataframe for an CPL team
#'
#' @description
#' This function saves all CPL matches agaist all opposition as a single dataframe in the
#' output directory
#'
#' @usage
#' saveAllMatchesAllOppositionCPLT20(dir=".",odir=".")
#'
#' @param dir
#' Input Directory
#'
#' @param odir
#' Output Directory
#'
#' @return None
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
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

saveAllMatchesAllOppositionCPLT20 <- function(dir=".",odir=".") {

    teams <-c("Antigua Hawksbills","Barbados Tridents","Guyana Amazon Warriors","Jamaica Tallawahs",
              "St Kitts and Nevis Patriots","St Lucia Zouks","Trinbago Knight Riders")

    matches <- NULL
    for(i in seq_along(teams)){
        cat("Team1=",teams[i],"\n")
        tryCatch(matches <- getAllMatchesAllOpposition(teams[i],dir=dir,save=TRUE,odir=odir),
                 error = function(e) {
                     print("No matches")

                 }
        )
        matches <- NULL
    }
}
