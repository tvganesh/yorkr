##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketPlot
# This function plots the average wickets taken by bowler versus number of overs bowled
#
###########################################################################################
#' @title
#' Compute and plot average wickets taken versus the number of overs bowled
#'
#' @description
#' This function computes and plots the average wickets taken by the bowler versus the
#' number of overs bowled
#' @usage
#' bowlerWicketPlot(df, name)
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
#' bowlerWicketPlot(jadeja,"RA Jadeja")
#'
#' @seealso
#' \code{\link{bowlerMeanEconomyRate}}
#' \code{\link{bowlerWicketsVenue}}
#' \code{\link{bowlerMeanRunsConceded}}
#'
#' @export
#'
bowlerWicketPlot <- function(df,name){
    overs = runs = maidens = meanRuns = wickets = bowler = meanWickets = NULL
     c <- summarise(group_by(df,overs),meanRuns=mean(runs),meanMaidens=mean(maidens),
                    meanWickets=mean(wickets))

     plot.title <- paste(name,"- Average wickets vs overs")
     ggplot(c,aes(x=overs, y=meanWickets,fill=overs)) +
         geom_bar(data=c,stat="identity" ) +
         xlab("Overs") + ylab("Mean Wickets") +
         ggtitle(bquote(atop(.(plot.title),
                             atop(italic("Data source:http://cricsheet.org/"),""))))
}
