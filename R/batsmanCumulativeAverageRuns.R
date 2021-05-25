##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 14 Apr 2016
# Function: batsmanCumulativeAverageRuns
# This function computes and plots the cumulative average runs by a batsman
#
###########################################################################################
#' @title
#' Batsman's cumulative average runs
#'
#' @description
#' This function computes and plots the cumulative average runs of a batsman
#'
#' @usage
#' batsmanCumulativeAverageRuns(df,name= "A Leg Glance",staticIntv=1)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
#'
#' @param staticIntv
#' Static or interactive -staticIntv =1 (static plot) &  staticIntv =2 (interactive  plot)
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
#' batsmanCumulativeAverageRuns(kohli,"Kohli")
#' }
#' @seealso
#' \code{\link{batsmanCumulativeStrikeRate}}
#' \code{\link{bowlerCumulativeAvgEconRate}}
#' \code{\link{bowlerCumulativeAvgWickets}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmanRunsPredict}}
#'
#' @export
#'
batsmanCumulativeAverageRuns <- function(df,name="A Leg Glance",staticIntv=1){
    runs=cs=no=NULL
    ggplotly=NULL
    b <- select(df,runs)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,runs)

    d <- mutate(c,cs=cumsum(runs)/no)
    plot.title= paste(name,"- Cumulative Average vs No of innings")
    if(staticIntv ==1){ #ggplot2
        ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
            xlab("No of innings") + ylab("Cumulative Avg. runs") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else { #ggplotly
        g <- ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
            ggtitle(plot.title) +
            xlab("No of innings") + ylab("Cumulative Avg. runs")

        ggplotly(g)
     }


}
