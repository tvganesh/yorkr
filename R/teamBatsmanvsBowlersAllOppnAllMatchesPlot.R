##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBatsmanVsBowlersAllOppnAllMatchesPlot
# This function computes performance of batsmen/batsman against bowlers of the opposition.
# It provides the names of the bowlers against whom the batsman scored the most.
# This is plotted as a chart
###########################################################################################
#' @title
#' This function computes the performance of the batsmen of a team in all matches against all
#' oppositions. The result can be plotted or returned as a dataframe
#'
#' @description
#' This function computes the performance of batsmen against all bowlers of all oppositions in all matches.
#' The data frame can be either plotted or returned to the user
#'
#' @usage
#' teamBatsmanVsBowlersAllOppnAllMatchesPlot(df,plot=TRUE)
#'
#' @param df
#' The dataframe of all the matches of the team against all oppositions
#'
#' @param plot
#' If plot=TRUE the result is plotted or else the data frame is returned
#'
#'
#' @return None or dataframe
#'
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' # Get all matches for team India against all oppositions in all matches
#' matches <-getAllMatchesAllOpposition("India",dir="../data/",save=TRUE)
#'
#' # Also load directly from file
#' #load("allMatchesAllOpposition-India.RData")
#'
#' d <- teamBatsmanVsBowlersAllOppnAllMatchesRept(matches,"India",rank=1,dispRows=50)
#' teamBatsmanVsBowlersAllOppnAllMatchesPlot(d)
#' e <- teamBatsmanVsBowlersAllOppnAllMatchesPlot(d,plot=FALSE)
#'
#' @seealso
#' \code{\link{teamBatsmanvsBowlersAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanPartnershipOppnAllMatchesChart}}
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanVsBowlerOppnAllMatches}}
#'
#' @export
#'
teamBatsmanVsBowlersAllOppnAllMatchesPlot <- function(df,plot=TRUE)
{
    runs=bowler=NULL
    bman <- df$batsman
    if(plot==TRUE){
        plot.title <- paste(bman,"-Performances against all bowlers ODIs")
        ggplot(data=df,aes(x=bowler,y=runs,fill=factor(bowler))) +
            facet_grid(~ batsman) + geom_bar(stat="identity") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }else{
        df
    }

}
