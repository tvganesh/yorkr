##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMovingAverage
# This function plots the moving average of wickets taken by bowler in career
#
###########################################################################################
#' @title
#' Bowler's moving average of wickets
#'
#' @description
#' This function computes and plots the wickets taken by the bowler over career. A loess
#' regression fit plots the moving average of wickets taken by bowler
#' @usage
#' bowlerMovingAverage(df, name,dateRange,staticIntv1=1)
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
#' @param staticIntv1
#' Static or interactive -staticIntv1 =1 (static plot) &  staticIntv1 =2 (interactive  plot)
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
#' jadeja <- getBowlerWicketDetails(team="India",name="Jadeja",dir=pathToFile)
#' bowlerMeanRunsConceded(jadeja,"RA Jadeja",dateRange,staticIntv=1)
#' }
#'
#' @seealso
#' \code{\link{bowlerMeanEconomyRate}}\cr
#' \code{\link{bowlerWicketPlot}}\cr
#' \code{\link{bowlerWicketsVenue}}\cr
#' \code{\link{bowlerMeanRunsConceded}}\cr
#'
#' @export
#'
bowlerMovingAverage <- function(df,name,dateRange,staticIntv1=1){
    bowler = wickets = NULL
    ggplotly=NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    c <- select(df,bowler,wickets,date)

    plot.title = paste(name,"- Moving average of wickets in career")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(c) + geom_line(aes(x=date, y=wickets),colour="darkgrey") +
            geom_smooth(aes(x=date, y=wickets)) +
            xlab("Date") + ylab("Wickets") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else { #ggplotly
        g <- ggplot(c) + geom_line(aes(x=date, y=wickets),colour="darkgrey") +
            geom_smooth(aes(x=date, y=wickets)) +
            xlab("Date") + ylab("Wickets") +
            ggtitle(plot.title)
        ggplotly(g)
    }
}
