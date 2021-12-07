##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMeanEconomyRate
#
#
###########################################################################################
#' @title
#' Mean economy rate versus number of overs
#'
#' @description
#' This function computes and plots mean economy rate and the number of
#' overs bowled by the bowler
#' @usage
#' bowlerMeanEconomyRate(df, name,dateRange,staticIntv1=1)
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
#' bowlerMeanEconomyRate(jadeja,"RA Jadeja")
#' }
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}\cr
#' \code{\link{bowlerWicketsVenue}}\cr
#' \code{\link{bowlerMeanRunsConceded}}\cr
#'
#' @export
#'

bowlerMeanEconomyRate <- function(df,name,dateRange,staticIntv1=1){
    overs =meanEconomyRate = economyRate = NULL
    ggplotly=NULL
    print(dateRange[1])
    df=df %>% filter(date >= dateRange[1] & date <= dateRange[2])
    c <- summarise(group_by(df,overs),meanEconomyRate=mean(economyRate))

    plot.title <- paste(name,"- Mean Economy Rate vs Overs")
    if(staticIntv1 ==1){ #ggplot2
        ggplot(c,  width = "auto",
               height = "auto",aes(x=overs, y=meanEconomyRate,fill=overs)) +
            geom_bar(data=c,stat="identity" ) +
            xlab("Overs") + ylab("Mean Economy Rate") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),""))))
    } else { #ggplotly
      g<- ggplot(c,aes(x=overs, y=meanEconomyRate,fill=overs)) +
            geom_bar(data=c,stat="identity" ) +
            xlab("Overs") + ylab("Mean Economy Rate") +
            ggtitle(plot.title)
      ggplotly(g)

    }


}
