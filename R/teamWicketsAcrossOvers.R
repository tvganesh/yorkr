##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 4 Nov 2021
# Function: teamWicketsAcrossOvers
# This function the computes the wickets taken  in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the wickets  in powerplay, middle and death overs
#'
#' @description
#' This function  plots wickets scored in powerplay, middle and death overs
#'
#' @usage
#' teamWicketsAcrossOvers(match,t1,t2,plot=1)
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
#'
#' # Plot tne match worm plot
#' teamWicketsAcrossOvers(match,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
teamWicketsAcrossOvers <- function(match,t1,t2,plot=1) {
    team=ball=totalRuns=total=NULL
    ggplotly=NULL

    # Filter the performance of team1
    a <-filter(match,team==t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,team,wicketPlayerOut)
    a3 <- a2 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())

    a3wickets=ifelse(!is.na(a3$wickets[1]), a3$wickets[1], 0)
    if(!is.na(a3$wickets[1])){
        a3$opposition=t2
        a3$type="1-Power Play"
    }


    # Middle overs I
    b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b2 <- select(b1,team,wicketPlayerOut)
    b3 <- b2 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    b3wickets=ifelse(!is.na(b3$wickets[1]), b3$wickets[1], 0)

    if(!is.na(b3$wickets[1])){
        b3$opposition=t2
        b3$type="2-Middle Overs"
    }


    #Midle overs 2
    c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c2 <- select(c1,team,wicketPlayerOut)
    c3 <- c2 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    c3wickets=ifelse(!is.na(c3$wickets[1]), c3$wickets[1], 0)

    if(!is.na(c3$wickets[1])){
        c3$opposition=t2
        c3$type="3-Death Overs"
    }


    ####################
    # Filter the performance of team2
    a <-filter(match,team==t2)
    # Power play
    a11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a21 <- select(a11,team,wicketPlayerOut)
    a31 <- a21 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    a31wickets=ifelse(!is.na(a31$wickets[1]), a31$wickets[1], 0)

    if(!is.na(a31$wickets[1])){
      a31$opposition=t1
      a31$type="1-Power Play"
    }

    b11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
    b21 <- select(b11,team,wicketPlayerOut)
    b31 <- b21 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    b31wickets=ifelse(!is.na(b31$wickets[1]), b31$wickets[1], 0)

    if(!is.na(b31$wickets[1])){
      b31$type="2-Middle Overs"
      b31$opposition=t1
    }

    #Midle overs 2
    c11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
    c21 <- select(c11,team,wicketPlayerOut)
    c31 <- c21 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    c31wickets=ifelse(!is.na(c31$wickets[1]), c31$wickets[1], 0)

    if(!is.na(c31$wickets[1])){
       c31$opposition=t1
       c31$type="3-Death Overs"
    }

    m=rbind(a3,b3,c3,a31,b31,c31)

    plot.title= paste("Wickets across 20 overs of ",t1, "and", t2, sep=" ")

    # Plot both lines
    if(plot ==1){ #ggplot2
      plot.title= paste("Wickets across 20 overs of ",t1, "and", t2, sep=" ")
      ggplot(m,aes(x=type, y=wickets, fill=opposition)) +
        geom_bar(stat="identity", position = "dodge") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))


    }else { #ggplotly
        g <-       ggplot(m,aes(x=type, y=wickets, fill=opposition)) +
          geom_bar(stat="identity", position = "dodge") +
          ggtitle(plot.title)

        ggplotly(g)

    }


}
