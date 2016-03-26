##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketsAgainstOpposition
# This function plots the mean wickets taken by the bowler against different oppositions
#
###########################################################################################
#' @title
#' Compute and plot mean wicket taken by the bowler versus  against different teams
#'
#' @description
#' This function computes and plots mean number of wickets taken by the bowler  against different
#' opposition
#' @usage
#' bowlerWicketsAgainstOpposition(df, name)
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
#' bowlerWicketsAgainstOpposition(jadeja,"RA Jadeja")
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}
#' \code{\link{bowlerWicketPlot}}
#' \code{\link{bowlerWicketsVenue}}
#' \code{\link{bowlerMeanRunsConceded}}
#'
#' @export
#'

bowlerWicketsAgainstOpposition <- function(df,name){

    c <- summarise(group_by(df,opposition),meanWickets=mean(wickets),numMatches=n())
    d <- mutate(c,opposition=paste(opposition,"(",numMatches,")",sep=""))
    plot.title = paste(name,"- Wickets against Opposition(number innings)")
    ggplot(d, aes(x=opposition, y=meanWickets, fill=opposition))+
        geom_bar(stat = "identity",position="dodge") +
        geom_hline(aes(yintercept=2))+
        xlab("Opposition") + ylab("Average wickets taken") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))
}
