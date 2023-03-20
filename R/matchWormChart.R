##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 15 Apr 2016
# Function: matchWormGraph
# This function computes  and plots the match worm chart
#
###########################################################################################
#' @title
#' Plot the match worm graph
#'
#' @description
#' This function  plots the match worm graph between 2 teams in a match
#'
#' @usage
#' matchWormGraph(match,t1,t2,plot=1)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The 1st team of the match
#'
#' @param t2
#' the 2nd team in the match
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
#'
#' @return none
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
#' #Get the match details
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#' # Plot tne match worm plot
#' matchWormGraph(a,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
matchWormGraph <- function(match,t1,t2,plot=1) {
    team=ball=totalRuns=total=NULL
    ggplotly=NULL
    # Filter the performance of team1
    a <-filter(match,team==t1)
    b <- select(a,ball,totalRuns)
    # Check for both possibilities
    if(grepl("1st",b$ball[1])){
        c <-mutate(b,ball=gsub("1st\\.","",ball))
    } else{
        c <-mutate(b,ball=gsub("2nd\\.","",ball))
    }

    # Compute cumulative sum vs balls bowled
    d <- mutate(c,total=cumsum(totalRuns))

    # Filter performance of team2
    a <-filter(match,team==t2)
    b1 <- select(a,ball,totalRuns)
    # Check for both possibilities
    if(grepl("2nd",b1$ball[1])){
        c1 <-mutate(b1,ball=gsub("2nd\\.","",ball))
    } else{
        c1 <-mutate(b1,ball=gsub("1st\\.","",ball))
    }

    # Compute cumulative sum vs balls bowled
    d1 <- mutate(c1,total=cumsum(totalRuns))

    # Convert to numeric
    d$ball=as.numeric(d$ball)
    d1$ball=as.numeric(d1$ball)
    # Plot both lines
    if(plot ==1){ #ggplot2
      ggplot() +
        geom_line(data = d, aes(x = ball, y = total, color = t1)) +
        geom_line(data = d1, aes(x = ball, y = total, color = t2))+
            xlab("Overs") +
         ggtitle(bquote(atop(.("Worm chart of match"),
                             atop(italic("Data source:http://cricsheet.org/"),""))))

    }else { #ggplotly
        g <- ggplot() +
            geom_line(data = d, aes(x = ball, y = total, color = t1)) +
            geom_line(data = d1, aes(x = ball, y = total, color = t2))+
            xlab("Overs") +
            ggtitle("Worm chart of match")


        ggplotly(g)

    }


}
