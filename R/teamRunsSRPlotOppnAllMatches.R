##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 19 Nov 2021
# Function: teamRunsSRPlotOppnAllMatches
# This function computes the Runs vs SR of the team batsman against opposition in
# all matches
#
#
###########################################################################################
#' @title
#' Team batting Runs vs SR  against  oppositions in all matches
#'
#' @description
#' This function computes and plots the Runs vs SR  of a team in all matches against an
#' oppositions.
#'
#' @usage
#' teamRunsSRPlotOppnAllMatches(matches,t1,t2,plot=1)
#'
#' @param matches
#' All matches of the team in all matches with all oppositions
#'
#' @param t1
#' The team for which the the batting partnerships are sought
#'
#' @param t2
#' The opposition team
#'

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
#' # Top batsman is displayed in descending order of runs
#' teamRunsSRPlotOppnAllMatches(matches,theTeam="India")
#'
#' # The best England players scorecard against India is shown
#' teamRunsSRPlotOppnAllMatches(matches,theTeam="England")
#' }
#'
#' @seealso
#' \code{\link{teamBatsmenVsBowlersAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBowlingWicketRunsAllOppnAllMatches}}
#'
#' @export
#'
teamRunsSRPlotOppnAllMatches <- function(matches,t1,t2,plot=1){
  team=batsman=runs=fours=sixes=NULL
  byes=legbyes=noballs=wides=NULL

  a <-filter(matches,team==t1)
  b <- select(a,batsman,runs)
  names(b) <-c("batsman","runs")

  #Compute the number of 4s
  c <-
    b %>%
    mutate(fours=(runs>=4 & runs <6)) %>%
    filter(fours==TRUE)

  # Group by batsman. Count 4s
  d <-    summarise(group_by(c, batsman),fours=n())

  # Get the total runs for each batsman
  e <-summarise(group_by(a,batsman),sum(runs))
  names(b) <-c("batsman","runs")
  details <- full_join(e,d,by="batsman")
  names(details) <-c("batsman","runs","fours")

  f <-
    b %>%
    mutate(sixes=(runs ==6)) %>%
    filter(sixes == TRUE)
  # Group by batsman. COunt 6s
  g <- summarise(group_by(f, batsman),sixes=n())
  names(g) <-c("batsman","sixes")
  #Full join with 4s and 6s
  details <- full_join(details,g,by="batsman")

  # Count the balls played by the batsman
  ballsPlayed <-
    a  %>%
    select(batsman,byes,legbyes,wides,noballs,runs) %>%

    filter(wides ==0,noballs ==0,byes ==0,legbyes == 0) %>%
    select(batsman,runs)

  ballsPlayed<- summarise(group_by(ballsPlayed,batsman),count=n())
  names(ballsPlayed) <- c("batsman","ballsPlayed")
  details <- full_join(details,ballsPlayed,by="batsman")
  details$SR= details$runs/details$ballsPlayed *100.00
  cat("Total=",sum(details$runs),"\n")
  details <- arrange(details,desc(runs),desc(sixes),desc(fours))
  details <- select(details,batsman,ballsPlayed,fours,sixes,runs,SR)

  x_lower <- quantile(details$runs,p=0.66)
  y_lower <- quantile(details$SR,p=0.66)

  plot.title <- paste("Runs vs SR of ", t1, "in all matches against", t2)
  if(plot == 1){ #ggplot2
    details %>%
      mutate(quadrant = case_when(runs > x_lower & SR > y_lower   ~ "Q1",
                                  runs <= x_lower & SR > y_lower  ~ "Q2",
                                  runs <= x_lower & SR <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SR,color=quadrant)) +
      geom_text(aes(runs,SR,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

  } else if(plot == 2){ #ggplotly
    g <-  details %>%
      mutate(quadrant = case_when(runs > x_lower & SR > y_lower   ~ "Q1",
                                  runs <= x_lower & SR > y_lower  ~ "Q2",
                                  runs <= x_lower & SR <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SR,color=quadrant)) +
      geom_text(aes(runs,SR,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)

    ggplotly(g)
  }



}
