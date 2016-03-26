##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: teamBowlersVsBatsmanMatch
# This function computes  performance of the team bowlers against the opposition batsmen
#
###########################################################################################
#' @title
#' Compute the performance of bowlers of a team in a specific match
#'
#' @description
#' This function computes performance of bowlers of a team against an opposition in a match
#'
#' @usage
#' teamBowlersVsBatsmanMatch(match,theTeam,plot=TRUE)
#'
#' @param match
#' The data frame of the match. This can be obtained with the call for e.g
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#'
#' @param theTeam
#' The team against which the performance is required
#'
#' @param plot
#' This parameter specifies if a plot is required, If plot=FALSE then a data frame is returned
#'
#' @return None or dataframe
#' If plot=TRUE there is no return. If plot=TRUE then the dataframe is returned
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
#' # Get the match between England and Pakistan
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#' teamBowlersVsBatsmanMatch(a,"Pakistan")
#' teamBowlersVsBatsmanMatch(a,"England")
#' m <- teamBowlersVsBatsmanMatch(a,"Pakistan")
#'
#'
#'
#' @seealso
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatches}}
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanPartnershipOppnAllMatchesChart}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesRept}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesPlot}}
#'
#' @export
#'
teamBowlersVsBatsmanMatch <- function(match,theTeam,plot=TRUE){
    a <-filter(match,team==theTeam)
    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")

    # Output plot or dataframe
    if(plot == TRUE){
        ggplot(data=b,aes(x=batsman,y=runsConceded,fill=factor(batsman))) +
            facet_grid(. ~ bowler) + geom_bar(stat="identity") +
            ggtitle(expression(atop("Bowler vs Batsman",
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else{
        b
    }
}
