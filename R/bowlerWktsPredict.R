##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWktsPredict
# This function predicts the number of deliveries to wickets for a bowler using
#classification trees
#
###########################################################################################
#' @title
#' Compute the deliveries required for taking  the number of wickets
#'
#' @description
#' This function uses a classification tree to compute the number deliveries needed
#' versus the wickets taken
#'
#' @usage
#' bowlerWktsPredict(df, name)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of bowler
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
#' # Get the data frame for RA Jadeja
#' jadeja1 <- getDeliveryWickets(team="India",name="Jadeja",save=FALSE)
#' bowlerWktsPredict(jadeja1,"RA Jadeja")
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}
#' \code{\link{bowlerWicketPlot}}
#' \code{\link{bowlerWicketsVenue}}
#' \code{\link{bowlerMeanRunsConceded}}
#'
#' @export
#'

bowlerWktsPredict <- function(df,name){
    rpart = NULL
    m <-rpart(wicketNo~delivery,data=df)
    atitle <- paste(name,"- No of deliveries to Wicket")
    rpart.plot(m,main=atitle)

}
