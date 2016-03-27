##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMeanEconomyRate
# This function gets the bowling details of a bowler
#
###########################################################################################
#' @title
#' Compute and plot mean economy rate versus number of overs
#'
#' @description
#' This function computes and plots mean economy rate and the number of
#' overs bowled by the bowler
#' @usage
#' bowlerMeanEconomyRate(df, name)
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
#' jadeja <- getBowlerWicketDetails(team="India",name="Jadeja")
#' bowlerMeanEconomyRate(jadeja,"RA Jadeja")
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}
#' \code{\link{bowlerWicketPlot}}
#' \code{\link{bowlerWicketsVenue}}
#' \code{\link{bowlerMeanRunsConceded}}
#'
#' @export
#'

bowlerMeanEconomyRate <- function(df,name){
    overs =meanEconomyRate = economyRate = NULL
    c <- summarise(group_by(df,overs),meanEconomyRate=mean(economyRate))

    plot.title <- paste(name,"- Mean Economy Rate vs Overs")
    ggplot(c,aes(x=overs, y=meanEconomyRate,fill=overs)) +
        geom_bar(data=c,stat="identity" ) +
        xlab("Overs") + ylab("Mean Economy Rate") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))


}
