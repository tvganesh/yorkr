##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Mar 2016
# Function: teamBowlersVsBatsmanOppnAllMatches
# This function computes the performance of the bowlers and the runs conceded and the batsman
# who scored most
#
#
###########################################################################################
#' @title
#' Compute the performance of bowlers of a team against an opposition in all matches
#'
#' @description
#' This function computes performance of bowlers of a team against an opposition in all matches
#' against the opposition
#'
#' @usage
#' teamBowlersVsBatsmanOppnAllMatches(match,main,opposition,plot=TRUE,top=5)
#'
#' @param match
#' The data frame of all matches between a team the opposition. This dataframe can be obtained with
#' matches <- getAllMatchesBetweenTeams("Australia","India",dir="../data")
#'
#' @param theTeam
#' The team against which the performance is requires
#'
#' @param rank
#' When the rank is 0 then the performance of all the bowlers is displayed. If rank=n (1,2,3 ..) then
#' the performance of that bowler is given
#'
#' @return dataframe
#' The dataframe with all performances
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
#' #  Plot the performance of top 5 Indian bowlers against Australia
#' teamBowlersVsBatsmanOppnAllMatches(matches,'India',"Australia",top=5)
#'
#' # Plot the performance of top 3 Australian bowlers against India
#' teamBowlersVsBatsmanOppnAllMatches(matches,"Australia","India",top=3)
#'
#' # Get the top 5 bowlers of Australia. Do not plot but get as a dataframe
#' teamBowlersVsBatsmanOppnAllMatches(matches,"Australia","India",plot=FALSE)
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
teamBowlersVsBatsmanOppnAllMatches <- function(match,main,opposition,plot=TRUE,top=5){
    noBalls=wides=team=runs=bowler=wicketKind=wicketPlayerOut=NULL
    team=bowler=ball=wides=noballs=runsConceded=overs=NULL
    a <-filter(match,team != main)

    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")
    # Compute total runs conceded
    c <- summarise(group_by(b,bowler),runs=sum(runsConceded))
    # Sort by descneding
    d <- arrange(c,desc(runs))

    # Pick 5 highest run givers
    d <- head(d,top)

    bowlers <- as.character(d$bowler)
    e <- NULL
    for(i in 1:length(bowlers)){
        f <- filter(b,bowler==bowlers[i])
        e <- rbind(e,f)

    }
    names(e) <- c("bowler","batsman","runsConceded")

    if( plot == TRUE){
        plot.title = paste("Bowlers vs batsmen -",main," Vs ",opposition,"(all matches)",sep="")
        ggplot(data=e,aes(x=batsman,y=runsConceded,fill=factor(batsman))) +
            facet_grid(. ~ bowler) + geom_bar(stat="identity") +
            #facet_wrap( ~ bowler,scales = "free", ncol=3,drop=TRUE) + #Does not work.Check!
            xlab("Batsman") + ylab("Runs conceded") +
            ggtitle(bquote(atop(.(plot.title),
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else{
        e
    }
}
