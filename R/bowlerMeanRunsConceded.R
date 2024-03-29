##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMeanRunsConceded
# This function plots the overs versus the mean runs conceded
#
###########################################################################################
#' @title
#' Mean runs conceded versus overs
#'
#' @description
#' This function computes and plots mean runs conceded by the bowler for the
#' number of overs bowled by the bowler
#' @usage
#' bowlerMeanRunsConceded(df, name,dateRange,staticIntv1=1)
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
#' bowlerMeanRunsConceded(jadeja,"RA Jadeja",dateRange)
#' }
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}\cr
#' \code{\link{bowlerWicketsVenue}}\cr
#' \code{\link{bowlerMeanRunsConceded}}\cr
#'
#' @export
#'

bowlerMeanRunsConceded <- function(df,name,dateRange,staticIntv1=1){
    overs = runs = maidens = meanRuns = wickets = NULL
    ggplotly=NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    c <- summarise(group_by(df,overs),meanRuns=mean(runs),meanMaidens=mean(maidens),
                   meanWickets=mean(wickets))
    plot.title <- paste(name,"- Average runs conceded vs Overs")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(c,aes(x=overs, y=meanRuns,fill=overs)) +
            geom_bar(data=c,stat="identity" ) +
            xlab("Overs") + ylab("Average runs conceded") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    }else { #ggplotly
        g <- ggplot(c,aes(x=overs, y=meanRuns,fill=overs)) +
            geom_bar(data=c,stat="identity" ) +
            xlab("Overs") + ylab("Average runs conceded") +
            ggtitle(plot.title)
        ggplotly(g)
    }
}
