##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketsAgainstOpposition
# This function plots the mean wickets taken by the bowler against different oppositions
#
###########################################################################################
#' @title
#' Bowler wickets versus different teams
#'
#' @description
#' This function computes and plots mean number of wickets taken by the bowler  against different
#' opposition
#' @usage
#' bowlerWicketsAgainstOpposition(df, name,staticIntv1=1)
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of bowler
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
#' bowlerWicketsAgainstOpposition(jadeja,"RA Jadeja")
#' }
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}\cr
#' \code{\link{bowlerWicketPlot}}\cr
#' \code{\link{bowlerWicketsVenue}}\cr
#' \code{\link{bowlerMeanRunsConceded}}\cr
#'
#' @export
#'

bowlerWicketsAgainstOpposition <- function(df,name,staticIntv1=1){
    meanWickets = numMatches = wickets = opposition = NULL
    ggplotly=NULL
    c <- summarise(group_by(df,opposition),meanWickets=mean(wickets),numMatches=n())
    d <- mutate(c,opposition=paste(opposition,"(",numMatches,")",sep=""))
    plot.title = paste(name,"- Wickets against Opposition(number innings)")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(d, height=600,aes(x=opposition, y=meanWickets, fill=opposition))+
            geom_bar(stat = "identity",position="dodge") +
            geom_hline(aes(yintercept=2))+
            xlab("Opposition") + ylab("Average wickets taken") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else { #ggplotly
        g <- ggplot(d, aes(x=opposition, y=meanWickets, fill=opposition))+
            geom_bar(stat = "identity",position="dodge") +
            geom_hline(aes(yintercept=2))+
            xlab("Opposition") + ylab("Average wickets taken") +
            ggtitle(plot.title)+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplotly(g,height=600)

    }
}
