##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 14 Apr 2016
# Function: bowlerCumulativeAvgEconRate
# This function computes and plots the cumulative average economy rate s of a bowler
#
###########################################################################################
#' @title
#' Bowler's cumulative average economy rate
#'
#' @description
#' This function computes and plots the cumulative average economy rate  of a bowler
#'
#' @usage
#' bowlerCumulativeAvgEconRate(df,name,dateRange,staticIntv1=1)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
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
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{)
#' #'Get the data frame for RA Jadeja
#' jadeja <- getBowlerWicketDetails(team="India",name="Jadeja",dir=pathToFile)
#' bowlerCumulativeAvgEconRate(jadeja,"RA Jadeja",dateRange)
#' }
#' @seealso
#' \code{\link{batsmanCumulativeAverageRuns}}
#' \code{\link{bowlerCumulativeAvgWickets}}
#' \code{\link{batsmanCumulativeStrikeRate}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#' \code{\link{batsmanRunsPredict}}
#'
#' @export
#'
bowlerCumulativeAvgEconRate <- function(df,name,dateRange,staticIntv1=1){
    economyRate=cs=no=NULL
    ggplotly=NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    b <- select(df,economyRate)
    b$no<-seq.int(nrow(b))
    c <- select(b,no,economyRate)

    d <- mutate(c,cs=cumsum(economyRate)/no)
    plot.title= paste(name,"- Cum. avg Econ Rate vs No innings")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
            xlab("No of innings") + ylab("Cumulative Avg. Economy Rate") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else { #ggplotly

        g <- ggplot(d) + geom_line(aes(x=no,y=cs),col="blue") +
            xlab("No of innings") + ylab("Cumulative Avg. Economy Rate") +
            ggtitle(plot.title)
        ggplotly(g)

    }
}
