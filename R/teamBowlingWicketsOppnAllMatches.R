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
#' Compute and plot the wickets taken in all matches against an opposition
#'
#' @description
#' This function computes returns the wickets taken against an opposition in all matches
#' The user can choose to plot or return a dataframe
#'
#' @usage
#' teamBowlerWicketsOppnAllMatches(matches,main,opposition,plot=TRUE,top=20)
#'
#' @param matches
#' The matches between the teams
#'
#' @param main
#' Team for which bowling performance is required
#'
#' @param opposition
#' The oppositionteam
#'
#' @param plot
#' If plot= TRUE the dataframe will be plotted else a data frame will be returned
#'
#' top
#' The number of rows of data frame to be returned, Default is 20
#'
#' @return None or data fame
#'
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
#' #Get the match details
#' matches <- getAllMatchesBetweenTeams("Australia","India",dir="../data")
#'
#' # Or the data can be directly loaded
#' #load("India-Australia-allMatches.RData")
#'
#' teamBowlerWicketsOppnAllMatches(matches,"India","Australia")
#' teamBowlerWicketsOppnAllMatches(matches,"Australia","India",top=10)
#' teamBowlerWicketsOppnAllMatches(matches,"India","Australia",plot=FALSE)
#'
#' @seealso
#' \code{\link{teamBowlingWicketMatch}}\cr
#' \code{\link{teamBowlingWicketRunsMatch}}\cr
#' \code{\link{teamBowlerVsBatsmanMatch}}\cr
#'
#' @export
#'
teamBowlerWicketsOppnAllMatches <- function(matches,main,opposition,plot=TRUE,top=20){

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
            xlab("Batsman") + ylab("Total wickets") +
            ggtitle(expression(atop("Performances of bowlers against opposition",
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else{
        e
    }
}
