##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 May 2020
# Function: saveAllMatchesAllOppositionPSLT20
# This function saves all PSL matches between a team and all opposition as a
# single dataframe
##################################################################################
#' @title
#' Saves matches against all PSL teams as dataframe for an PSL team
#'
#' @description
#' This function saves all PSL matches agaist all opposition as a single dataframe in the
#' output directory
#'
#' @usage
#' saveAllMatchesAllOppositionPSLT20(dir=".",odir=".")
#'
#' @param dir
#' Input Directory
#'
#' @param odir
#' Output Directory
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

saveAllMatchesAllOppositionPSLT20 <- function(dir=".",odir=".") {

    teams <- c("Islamabad United","Karachi Kings", "Lahore Qalandars", "Multan Sultans",
               "Peshawar Zalmi", "Quetta Gladiators")

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

