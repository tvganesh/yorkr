##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Mar 2016
# Function: teamBowlerWicketsOppnAllMatches
# This function computes the total wickets taken by the top 20(default) bowlers against the
# opposition
#
#
###########################################################################################
#' @title
#' Compute the performance of bowlers of a team  and the wicket taken against an
#' opposition in all matches
#'
#' @description
#' This function computes performance of bowlers of a team and the wickets taken against an
#' opposition in all matches against the opposition
#'
#' @usage
#' teamBowlerWicketsOppnAllMatches(matches,main,opposition,plot=TRUE,top=20)
#'
#' @param matches
#' The data frame of all matches between a team the opposition. This dataframe can be obtained with
#' matches <- getAllMatchesBetweenTeams("Australia","India",dir="../data")
#'
#' @param main
#' The team for which the performance is required
#'
#' @param opposition
#' The opposing team
#'
#' @param plot
#' If plot=TRUE then a plot is displayed else a dataframe is returned
#'
#' @param top
#' The number of top bowlers to be included in the result
#'
#' @return None or dataframe
#' The return depends on the value of the plot
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
#' # Get all matches between India and Australia
#' matches <- getAllMatchesBetweenTeams("Australia","India",dir="../data")
#'
#' #Display top 20
#' teamBowlerWicketsOppnAllMatches(matches,"India","Australia")
#' #Display and plot top 10
#' teamBowlerWicketsOppnAllMatches(matches,"Australia","India",top=10)
#'
#' #Do not plot but return as dataframe
#' teamBowlerWicketsOppnAllMatches(matches,"India","Australia",plot=FALSE)
#'
#'
#' @seealso
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatches}}
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanPartnershipOppnAllMatchesChart}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesRept}}
#' \code{\link{teamBowlerWicketRunsOppnAllMatches}}
#'
#' @export
#'
teamBowlerWicketsOppnAllMatches <- function(matches,main,opposition,plot=TRUE,top=20){

    noBalls=wides=team=runs=bowler=wicketKind=wicketPlayerOut=NULL
    team=bowler=ball=wides=noballs=runsConceded=overs=NULL
    #Filter the matches by the team
    a <-filter(matches,team!=main)

    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub("\\.\\d+","",over))

    #Compute number of wickets
    c <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    d <- summarise(group_by(c,bowler),wickets=length(unique(wicketPlayerOut)))

    e <- arrange(d,desc(wickets))
    e <- e[1:top,]

    names(e) <- c("bowler","wickets")

    if(plot==TRUE){
        ggplot(data=e,aes(x=bowler,y=wickets,fill=factor(bowler))) +
             geom_bar(stat="identity") +
            #facet_wrap( ~ bowler,scales = "free", ncol=3,drop=TRUE) + #Does not work.Check!
            xlab("Batsman") + ylab("Runs conceded") +
            ggtitle(expression(atop("Performances of bowlers against opposition",
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else{
        e
    }
}
