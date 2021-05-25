##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: teamBowlersVsBatsmenMatch
# This function computes  performance of the team bowlers against the opposition batsmen
#
###########################################################################################
#' @title
#' Team bowlers vs batsmen in a match
#'
#' @description
#' This function computes performance of bowlers of a team against an opposition in a match
#'
#' @usage
#' teamBowlersVsBatsmenMatch(match,theTeam,opposition, plot=1)
#'
#' @param match
#' The data frame of the match. This can be obtained with the call for e.g
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#'
#' @param theTeam
#' The team against which the performance is required
#'
#' @param opposition
#' The opposition team
#'
#' @param plot
#' plot=1 (static),plot=2(interactive),plot=3(table)
#'
#' @return None or dataframe
#' If plot=TRUE there is no return. If plot=TRUE then the dataframe is returned
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
#' # Get the match between England and Pakistan
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#' teamBowlersVsBatsmenMatch(a,"Pakistan","England")
#' teamBowlersVsBatsmenMatch(a,"England","Pakistan")
#' m <- teamBowlersVsBatsmenMatch(a,"Pakistan","England")
#' }
#'
#'
#' @seealso
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesRept}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'
teamBowlersVsBatsmenMatch <- function(match,theTeam,opposition, plot=1){

    batsman=runsConceded=team=runs=bowler=NULL
    ggplotly=NULL
    bowler=batsman=NULL
    c <- filter(match,team !=theTeam)
    b <-summarise(group_by(c,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")

    # Output plot or dataframe
    if(plot == 1){ #ggplot2
        plot.title <- paste(theTeam,"Bowler vs Batsman (against",opposition,")")
        p <- ggplot(data=b,aes(x=batsman,y=runsConceded,fill=factor(batsman))) +
            facet_grid(. ~ bowler) + geom_bar(stat="identity") +
            ggtitle(bquote(atop(.(plot.title),
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(plot.title = element_text(size=14, face="bold.italic",margin=margin(10)))

        p
    }  else if(plot == 2){ #ggplotly
        plot.title <- paste(theTeam,"Bowler vs Batsman (against",opposition,")")
        p <- ggplot(data=b,aes(x=batsman,y=runsConceded,fill=factor(batsman))) +
            facet_grid(. ~ bowler) + geom_bar(stat="identity") +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            theme(plot.title = element_text(size=14, face="bold.italic",margin=margin(10)))
           ggplotly(p)
    }
    else{
        b
    }
}
