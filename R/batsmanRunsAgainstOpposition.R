##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: batsmanRunsAgainstOpposition
# This function computes and plots the runs scored by the batsman against different oppositions
#
###########################################################################################
#' @title
#' Batsman runs against different oppositions
#'
#' @description
#' This function computes and plots the mean runs scored by the batsman against different
#' oppositions
#' @usage
#' batsmanRunsAgainstOpposition(df, name= "A Leg Glance",dateRange,staticIntv=1)
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
#' @param staticIntv
#' Static or interactive -staticIntv =1 (static plot) &  staticIntv =2 (interactive  plot)
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
#' #Get the data frame for Kohli
#' kohli <- getBatsmanDetails(team="India",name="Kohli",dir=pathToFile)
#' batsmanRunsAgainstOpposition(kohli,"Kohli",dateRange)
#' }
#'
#' @seealso
#' \code{\link{batsmanFoursSixes}}\cr
#' \code{\link{batsmanRunsVsDeliveries}}\cr
#' \code{\link{batsmanRunsVsStrikeRate}}\cr
#' \code{\link{batsmanRunsPredict}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
#'
#' @export
#'


batsmanRunsAgainstOpposition <- function(df,name= "A Leg Glance",dateRange,staticIntv=1){
    batsman = runs = opposition = meanRuns =  NULL
    ggplotly=NULL
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    b <- select(df,batsman,runs,opposition)
    c <-b[complete.cases(b),]
    d <- summarise(group_by(c,opposition),meanRuns=mean(runs),numMatches=n())
    plot.title = paste(name,"- Mean runs against opposition")

    if(staticIntv ==1){ #ggplot2
        ggplot(d,height=600, aes(x=opposition, y=meanRuns, fill=opposition))+
            geom_bar(stat = "identity",position="dodge") +
            xlab("Opposition") + ylab("Mean Runs") +
            geom_hline(aes(yintercept=50))+
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else { #ggplotly

        g <- ggplot(d, aes(x=opposition, y=meanRuns, fill=opposition))+
            geom_bar(stat = "identity",position="dodge") +
            xlab("Opposition") + ylab("Mean Runs") +
            geom_hline(aes(yintercept=50))+
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))

        ggplotly(g,height=600)
    }

}
