##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 11 May 2020
# Function: saveAllMatchesAllOppositionNTBT20
# This function saves all NTB matches between a team and all opposition as a
# single dataframe
##################################################################################
#' @title
#' Saves matches against all NTB teams as dataframe for an NTB team
#'
#' @description
#' This function saves all NTB matches agaist all opposition as a single dataframe in the
#' output directory
#'
#' @usage
#' saveAllMatchesAllOppositionNTBT20(dir=".",odir=".")
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

saveAllMatchesAllOppositionNTBT20 <- function(dir=".",odir=".") {

    teams <- c("Birmingham Bears","Derbyshire", "Durham", "Essex", "Glamorgan",
               "Gloucestershire", "Hampshire", "Kent","Lancashire",
               "Leicestershire", "Middlesex","Northamptonshire",
               "Nottinghamshire","Somerset","Surrey","Sussex","Warwickshire",
               "Worcestershire","Yorkshire")

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

