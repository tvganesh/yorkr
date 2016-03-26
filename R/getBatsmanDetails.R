##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: getBatsmanDetails
# This function gets the batting details of a batsman
#
###########################################################################################
#' @title
#' Get the batting details of a batsman from the match
#'
#' @description
#' This function gets the batting details of a batsman given the match data as a RData file
#' @usage
#' getBatsmanDetails(team,name)
#'
#' @param team
#' The team of the batsman e.g. India
#'
#' @param name
#' Name of batsman
#'
#' @return None
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#'
#' getBatsmanDetails(team="India",name="Kohli")
#'
#'
#' @seealso
#' \code{\link{batsmansRunsPredict}}
#' \code{\link{batsmanMovingAverage}}
#' \code{\link{bowlerWicketsVenue}}
#' \code{\link{bowlerMeanRunsConceded}}
#'
#' @export
#'

getBatsmanDetails <- function(team, name){

    fl <- paste("./",team,"-BattingDetails.RData",sep="")
    load(fl)
    details <- battingDetails
    batsmanDetails <- filter(details,grepl(name,batsman))
    batsmanDetails
}
