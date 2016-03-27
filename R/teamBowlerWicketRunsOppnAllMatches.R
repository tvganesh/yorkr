##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Mar 2016
# Function: teamBowlerWicketRunsOppnAllMatches
# This function computes the number of wickets taken and runs conceded by the bowlers in
# all matches against the opposition
#
#
###########################################################################################
#' @title
#' Compute the performance of bowlers of a team  and the wicket kind against an
#' opposition in all matches
#'
#' @description
#' This function computes performance of bowlers of a team and the runs conceded against an
#' opposition in all matches against the opposition
#'
#' @usage
#' teamBowlerWicketRunsOppnAllMatches(matches,main,opposition,plot=TRUE)
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
#' teamBowlerWicketRunsOppnAllMatches(matches,"India","Australia")
#' m <-teamBowlerWicketRunsOppnAllMatches(matches,"Australia","India",plot=FALSE)
#'
#'
#' @seealso
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatches}}
#' \code{\link{teamBowlerWicketsOppnAllMatches}}
#' \code{\link{teamBatsmanPartnershipOppnAllMatchesChart}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesRept}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesPlot}}
#'
#' @export
#'
teamBowlerWicketRunsOppnAllMatches <- function(matches,main,opposition,plot=TRUE){
    team=bowler=ball=NULL

    byes=legbyes=noballs=wides=runConceded=NULL
    extras=wicketFielder=wicketKind=wicketPlayerOut=NULL

    # Compute the maidens,runs conceded and overs for the bowlers
    a <-filter(matches,team !=main)

    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub("\\.\\d+","",over))

    #Calculate the number of maiden overs
    c <- summarise(group_by(b,bowler,over),sum(runs,wides,noballs))
    names(c) <- c("bowler","over","runsConceded")
    d <-summarize(group_by(c,bowler),maidens=sum(runsConceded==0))

    #Compute total runs conceded (runs_wides+noballs)
    e <- summarize(group_by(c,bowler),runs=sum(runsConceded))

    # Calculate the number of overs bowled by each bwler
    f <- select(c,bowler,over)
    g <- summarise(group_by(f,bowler),overs=length(unique(over)))


    #Compute number of wickets
    h <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    i <- summarise(group_by(h,bowler),wickets=length(unique(wicketPlayerOut)))

    #Join the over & maidens
    j <- full_join(g,d,by="bowler")
    # Add runs
    k <- full_join(j,e,by="bowler")
    # Add wickets
    l <- full_join(k,i,by="bowler")

    # Set NAs to 0
    if(sum(is.na(l$wickets)) != 0){
        l[is.na(l$wickets),]$wickets=0
    }

    if(plot==TRUE){
        plot.title = paste("Wicket taken cs Runs conceded -",main," Vs ",opposition,"(all matches)",sep="")
        ggplot(data=l,aes(x=factor(wickets),y=runs,fill=factor(wickets))) +
            facet_wrap( ~ bowler,scales = "fixed", ncol=8) +
            geom_bar(stat="identity") +
            xlab("Number of wickets") + ylab('Runs conceded') +
            ggtitle(bquote(atop(.(plot.title),
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
        l
    }

}
