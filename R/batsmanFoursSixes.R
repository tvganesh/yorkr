##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: batsmanFoursSixes
# This function computes and plots the total runs,fours and sixes hit by the batsman
#
###########################################################################################
#' @title
#' Compute and plot the total runs, fours and sixes of the batsman
#'
#' @description
#' This function computes and plots the total runs, fours and sixes of
#' the batsman
#' @usage
#' batsmanFoursSixes(df, name="Kohli")
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
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' # Get the data frame for Kohli
#' kohli <- getBatsmanDetails(team="India",name="Kohli")
#' kohli46 <- select(kohli,batsman,ballsPlayed,fours,sixes,runs)
#' batsmanFoursSixes(kohli46,"Kohli")
#'
#' @seealso
#' \code{\link{batsmanDismissals}}
#' \code{\link{batsmanRunsVsDeliveries}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmansRunsPredict}}
#'
#' @export
#'
batsmanFoursSixes <- function(df,name){
    names(df) <- c("batsman","ballsPlayed","fours","sixes","TotalRuns")
    c <- mutate(df, RunsFromFours=fours*4,RunsFromSixes=sixes*6)
    d <- select(c, batsman,ballsPlayed,RunsFromFours,RunsFromSixes,TotalRuns)
    e <- melt(d,id=c("batsman","ballsPlayed"))

    plot.title = paste(name,"- Total runs, 4s and 6s vs Balls Faced")
    ggplot(e) + geom_point(aes(x=ballsPlayed, y=value, colour=variable)) +
        geom_smooth(aes(x=ballsPlayed, y=value, colour=variable)) +
        scale_colour_manual(values=c("red","green","blue")) +
        xlab("Deliveries faced") + ylab("Runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))


}
