##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: batsmanRunsAgainstOpposition
# This function computes and plots the runs scored by the batsman against different oppositions
#
###########################################################################################
#' @title
#' Compute and plot the average runs scored by the batsman against different oppositions
#'
#' @description
#' This function computes and plots the mean runs scored by the batsman against different
#' oppositions
#' @usage
#' batsmanRunsAgainstOpposition(df, name="Kohli")
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
#'
#' @return None
#' @references
#' \url{http://cricsheet.org/}
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' # Get the data frame for Kohli
#' kohli <- getBatsmanDetails(team="India",name="Kohli")
#' batsmanRunsAgainstOpposition(kohli,"Kohli")
#'
#' @seealso
#' \code{\link{batsmanFoursSixes}}
#' \code{\link{batsmanRunsVsDeliveries}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmansRunsPredict}}
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatches}}
#'
#' @export
#'

batsmanRunsAgainstOpposition <- function(df,name){
    b <- select(df,batsman,runs,opposition)
    c <-b[complete.cases(b),]
    d <- summarise(group_by(c,opposition),meanRuns=mean(runs),numMatches=n())
    plot.title = paste(name,"- Runs against opposition")
    ggplot(d, aes(x=opposition, y=meanRuns, fill=opposition))+
        geom_bar(stat = "identity",position="dodge") +
        xlab("Opposition") + ylab("Runs") +
        geom_hline(aes(yintercept=50))+
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))
}
