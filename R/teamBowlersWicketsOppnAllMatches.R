##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Mar 2016
# Function: teamBowlersWicketsOppnAllMatches
# This function computes the total wickets taken by the top 20(default) bowlers against the
# opposition
#
#
###########################################################################################
#' @title
#' Team bowlers  wickets against an opposition in all matches
#'
#' @description
#' This function computes performance of bowlers of a team and the wickets taken against an
#' opposition in all matches against the opposition
#'
#' @usage
#' teamBowlersWicketsOppnAllMatches(matches,main,opposition,plot=1,top=20)
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
#' plot=1 (static),plot=2(interactive),plot=3(table)
#'
#' @param top
#' The number of top bowlers to be included in the result
#'
#' @return None or dataframe
#' The return depends on the value of the plot
#'
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
#' # Get all matches between India and Australia
#' matches <- getAllMatchesBetweenTeams("Australia","India",dir="../data")
#'
#' #Display top 20
#' teamBowlersWicketsOppnAllMatches(matches,"India","Australia")
#' #Display and plot top 10
#' teamBowlersWicketsOppnAllMatches(matches,"Australia","India",top=10)
#'
#' #Do not plot but return as dataframe
#' teamBowlersWicketsOppnAllMatches(matches,"India","Australia",plot=FALSE)
#' }
#'
#' @seealso
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesRept}}\cr
#' \code{\link{teamBowlersWicketRunsOppnAllMatches}}\cr
#'
#' @export
#'
teamBowlersWicketsOppnAllMatches <- function(matches,main,opposition,plot=1,top=20){

    team=bowler=ball=noballs=runs=NULL
    ggplotly=NULL
    wicketKind=wicketPlayerOut=over=wickets=NULL
    batsman=wides=NULL

    a = NULL
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
    # Display maximum or the requested 'top' size
    sz <- dim(e)
    if(sz[1] > top){
        e <- e[1:top,]
    }else{
        e <- e[1:sz[1],]
    }

    names(e) <- c("bowler","wickets")

    if(plot == 1){ #ggplot2
        plot.title = paste(main," Bowler performances ","(against ",opposition," all matches)",sep="")
        ggplot(data=e,aes(x=bowler,y=wickets,fill=factor(bowler))) +
            geom_bar(stat="identity") +
            #facet_wrap( ~ bowler,scales = "free", ncol=3,drop=TRUE) + #Does not work.Check!
            xlab("Batsman") + ylab("Wickets taken") +
            ggtitle(bquote(atop(.(plot.title),
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else if(plot == 2){ #ggplotly

        plot.title = paste(main," Bowler performances ","(against ",opposition," all matches)",sep="")
        g <- ggplot(data=e,aes(x=bowler,y=wickets,fill=factor(bowler))) +
            geom_bar(stat="identity") +
            #facet_wrap( ~ bowler,scales = "free", ncol=3,drop=TRUE) + #Does not work.Check!
            xlab("Batsman") + ylab("Wickets taken") +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplotly(g,500)

    } else{
        e
    }
}
