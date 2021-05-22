##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: batsmanFoursSixes
# This function computes and plots the total runs,fours and sixes hit by the batsman
#
###########################################################################################
#' @title
#' Batsman's total runs, fours and sixes
#'
#' @description
#' This function computes and plots the total runs, fours and sixes of
#' the batsman
#' @usage
#' batsmanFoursSixes(df,name= "A Leg Glance")
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
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
#' #Get the data frame for Kohli
#' kohli <- getBatsmanDetails(team="India",name="Kohli",dir=pathToFile)
#' kohli46 <- select(kohli,batsman,ballsPlayed,fours,sixes,runs)
#' batsmanFoursSixes(kohli46,"Kohli")
#' }
#' @seealso
#' \code{\link{batsmanDismissals}}
#' \code{\link{batsmanRunsVsDeliveries}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmanRunsPredict}}
#'
#' @export
#'
batsmanFoursSixes <- function(df,name= "A Leg Glance",staticIntv=1){
    fours <- sixes <- batsman <- ballsPlayed <- RunsFromFours <- NULL
    RunsFromSixes <- TotalRuns <- value <- variable <- NULL
    df <- select(df,batsman,ballsPlayed,fours,sixes,runs)
    names(df) <- c("batsman","ballsPlayed","fours","sixes","TotalRuns")
    print(head(df,30))
    c <- mutate(df, RunsFromFours=fours*4,RunsFromSixes=sixes*6)
    d <- select(c, batsman,ballsPlayed,RunsFromFours,RunsFromSixes,TotalRuns)
    e <- melt(d,id=c("batsman","ballsPlayed")
    print(e)

    plot.title = paste(name,"- Total runs, 4s and 6s vs Balls Faced")
    if(staticIntv ==1){ #ggplot2{
        ggplot(e) + geom_point(aes(x=ballsPlayed, y=value, colour=variable)) +
            geom_smooth(aes(x=ballsPlayed, y=value, colour=variable)) +
            scale_colour_manual(values=c("red","green","blue")) +
            xlab("Deliveries faced") + ylab("Runs") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else {
        g <- ggplot(e) + geom_point(aes(x=ballsPlayed, y=value, colour=variable)) +
            geom_smooth(aes(x=ballsPlayed, y=value, colour=variable)) +
            scale_colour_manual(values=c("red","green","blue")) +
            xlab("Deliveries faced") + ylab("Runs") +
            ggtitle(plot.title)

        ggplotly(g)
    }


}
