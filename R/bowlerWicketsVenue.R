##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketsVenue
# This function plots the performance of bowlers at different venues
#
###########################################################################################
#' @title
#' Bowler performance at different venues
#'
#' @description
#' This function computes and plots mean number of wickets taken by the bowler  in different
#' venues
#' @usage
#' bowlerWicketsVenue(df, name,dateRange,staticIntv1=1)
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
#' bowlerWicketsVenue(jadeja,"RA Jadeja",dateRange)
#' }
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}\cr
#' \code{\link{bowlerWicketsVenue}}\cr
#' \code{\link{bowlerMeanRunsConceded}}\cr
#'
#' @export
#'

bowlerWicketsVenue <- function(df,name,dateRange,staticIntv1=1){
    meanWickets = numMatches =wickets = venue = NULL
    ggplotly=NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    c <- summarise(group_by(df,venue),meanWickets=mean(wickets),numMatches=n())
    d <- mutate(c,venue=paste(venue,"(",numMatches,")",sep=""))
    e <- arrange(d,desc(meanWickets))
    f <- e[1:20,]
    plot.title = paste(name,"- Wickets in venue(number innings)")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(f,aes(x=venue, y=meanWickets, fill=venue))+
            geom_bar(stat = "identity",position="dodge") +
            geom_hline(aes(yintercept=2))+
            xlab("Venue") + ylab("Average wickets taken") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))

    } else { #ggplotly
        g <- ggplot(f, aes(x=venue, y=meanWickets, fill=venue))+
            geom_bar(stat = "identity",position="dodge") +
            geom_hline(aes(yintercept=2))+
            xlab("Venue") + ylab("Average wickets taken") +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplotly(g,height=600)

    }
}
