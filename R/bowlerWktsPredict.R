##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWktsPredict
# This function predicts the number of deliveries to wickets for a bowler using
#classification trees
#
###########################################################################################
#' @title
#' Predict the deliveries required to  wickets
#'
#' @description
#' This function uses a classification tree to compute the number deliveries needed
#' versus the wickets taken
#'
#' @usage
#' bowlerWktsPredict(df, name,dateRange)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of bowler
#'
#' @param dateRange
#' Date interval to consider
#'
#' @return None
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' # Get the data frame for RA Jadeja
#' jadeja1 <- getDeliveryWickets(team="India",name="Jadeja",save=FALSE)
#' bowlerWktsPredict(jadeja1,"RA Jadeja",dateRange)
#' }
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}\cr
#' \code{\link{bowlerWicketPlot}}\cr
#' \code{\link{bowlerWicketsVenue}}\cr
#' \code{\link{bowlerMeanRunsConceded}}\cr
#'
#' @export
#'

bowlerWktsPredict <- function(df,name,dateRange){
    rpart = NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    print(names(df))
    m <-rpart(wicketNo~delivery,data=df)
    atitle <- paste(name,"- No of deliveries to Wicket")
    rpart.plot(m,main=atitle)

}
