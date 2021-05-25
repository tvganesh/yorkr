##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBatsmenVsBowlersAllOppnAllMatchesPlot
# This function computes performance of batsmen/batsman against bowlers of the opposition.
# It provides the names of the bowlers against whom the batsman scored the most.
# This is plotted as a chart
###########################################################################################
#' @title
#' Plot of Team batsmen vs bowlers against all opposition all matches
#'
#' @description
#' This function computes the performance of batsmen against all bowlers of all oppositions in all matches.
#' The data frame can be either plotted or returned to the user
#'
#' @usage
#' teamBatsmenVsBowlersAllOppnAllMatchesPlot(df,plot=1)
#'
#' @param df
#' The dataframe of all the matches of the team against all oppositions
#'
#' @param plot
#' plot=1 (static),plot=2(interactive), plot=3 (table)
#'
#'
#' @return None or dataframe
#'
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
#' # Get all matches for team India against all oppositions in all matches
#' matches <-getAllMatchesAllOpposition("India",dir="../data/",save=TRUE)
#'
#' # Also load directly from file
#' #load("allMatchesAllOpposition-India.RData")
#'
#' d <- teamBatsmanVsBowlersAllOppnAllMatchesRept(matches,"India",rank=1,dispRows=50)
#' teamBatsmenVsBowlersAllOppnAllMatchesPlot(d)
#' e <- teamBatsmenVsBowlersAllOppnAllMatchesPlot(d,plot=FALSE)
#' }
#'
#' @seealso
#' \code{\link{teamBatsmenVsBowlersAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenVsBowlersOppnAllMatches}}\cr
#'
#' @export
#'
teamBatsmenVsBowlersAllOppnAllMatchesPlot <- function(df,plot=1)
{
    runs=bowler=NULL
    ggplotly=NULL
    bman <- df$batsman
    if(plot == 1){ #ggplot2
        plot.title <- paste(bman,"-Performances against all bowlers")
        ggplot(data=df,aes(x=bowler,y=runs,fill=factor(bowler))) +
            facet_grid(~ batsman) + geom_bar(stat="identity") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else if(plot == 2){ #ggplotly

        plot.title <- paste(bman,"-Performances against all bowlers")
        g <- ggplot(data=df,aes(x=bowler,y=runs,fill=factor(bowler))) +
            facet_grid(~ batsman) + geom_bar(stat="identity") +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplotly(g,height=500)

    }else{
        df
    }

}
