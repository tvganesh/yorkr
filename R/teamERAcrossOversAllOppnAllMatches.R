##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 5 Nov 2021
# Function: teamERAcrossOversAllOppnAllMatches
# This function computes economy rate across overs in all matches against all opposition in powerplay, middle and death overs
#
###########################################################################################
#' @title
#' Compute the ER by team against all team in powerplay, middle and death overs in all matches
#'
#' @description
#' This function  plots the ER by team against all team in in powerplay, middle and death overs
#'
#' @usage
#' teamERAcrossOversAllOppnAllMatches(matches,t1,plot=1)
#'
#' @param matches
#' The dataframe of the matches
#'
#' @param t1
#' The 1st team of the match
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
#' teamERAcrossOversAllOppnAllMatches(matches,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
teamERAcrossOversAllOppnAllMatches <- function(matches,t1,plot=1) {
  team=ball=totalRuns=total=ER=type=meanER=str_extract=NULL
  ggplotly=NULL

  # Filter the performance of team1
  a <-filter(matches,team==t1)
  # Power play
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,team,date,totalRuns)
  a3 <- a2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  a3$ER=a3$total/a3$count * 6
  a4 = a3 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  a4$type="1-Power Play"

  # Middle overs I
  b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b2 <- select(b1,team,date,totalRuns)
  b3 <- b2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  b3$ER=b3$total/b3$count * 6
  b4 = b3 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  b4$type="2-Middle Overs"

  ##Death overs
  c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c2 <- select(c1,team,date,totalRuns)
  c3 <- c2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  c3$ER=c3$total/c3$count * 6
  c4 = c3 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  c4$type="3-Death Overs"

  m=rbind(a4,b4,c4)
  plot.title= paste("Wickets across 20 overs by ",t1,"in all matches against all teams", sep=" ")

  # Plot both lines
  if(plot ==1){ #ggplot2
    ggplot(m,aes(x=type, y=meanER, fill=t1)) +
      geom_bar(stat="identity", position = "dodge") +
      ggtitle(bquote(atop(.(plot.title),
                          atop(italic("Data source:http://cricsheet.org/"),""))))



  }else { #ggplotly
    g <-   ggplot(m,aes(x=type, y=meanER, fill=t1)) +
      geom_bar(stat="identity", position = "dodge") +
      ggtitle(plot.title)


    ggplotly(g)

  }


}
