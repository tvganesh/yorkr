##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 27 Nov 2021
# Function: teamWicketsERPowerPlayPlotMatch
# This function computes the  wickets vs ER of team in powewrplay against  opposition in match
#
###########################################################################################
#' @title
#' Team wickets vs ER in powewrplay against   opposition in match
#'
#' @description
#' This function computes wickets vs ER in powewrplay against  oppositions in  match
#'
#' @usage
#' teamWicketsERPowerPlayPlotMatch(match,t1,t2, plot=1)
#'
#' @param matches
#' The matches of the team against all oppositions and all matches
#'
#' @param t1
#' Team for which bowling performance is required
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
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
#' teamWicketsERPowerPlayPlotMatch(matches, t1, plot=1)
#'
#' @seealso
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'
teamWicketsERPowerPlayPlotMatch <- function(match,t1,t2,plot=1) {
    team=ball=totalRuns=total=wickets=wicketsPowerPlay=wicketsMiddleOvers=wicketsDeathOvers=bowler=str_extract=NULL
    ggplotly=wicketPlayerOut=NULL

    # Filter the performance of team1
    a <-filter(match,team!=t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,date,bowler,wicketPlayerOut)
    a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsPowerPlay=sum(wickets))

    a21 <- select(a1,team,bowler,date,totalRuns)
    a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERPowerPlay=total/count *6)
    a41 <- a31 %>% select(bowler,ERPowerPlay)
    a42=inner_join(a4,a41,by="bowler")


    x_lower <- quantile(a42$wicketsPowerPlay,p=0.66)
    y_lower <- quantile(a42$ERPowerPlay,p=0.33)

    plot.title <- paste("Wickets-ER Plot of ", t1, "against ", t2)
    if(plot == 1){ #ggplot2
        a42 %>%
            mutate(quadrant = case_when(wicketsPowerPlay > x_lower & ERPowerPlay > y_lower   ~ "Q1",
                                        wicketsPowerPlay <= x_lower & ERPowerPlay > y_lower  ~ "Q2",
                                        wicketsPowerPlay <= x_lower & ERPowerPlay <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(wicketsPowerPlay,ERPowerPlay,color=quadrant)) +
            geom_text(aes(wicketsPowerPlay,ERPowerPlay,label=bowler,color=quadrant)) + geom_point() +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

} else if(plot == 2){ #ggplotly
        g <- a42 %>%
            mutate(quadrant = case_when(wicketsPowerPlay > x_lower & ERPowerPlay > y_lower   ~ "Q1",
                                        wicketsPowerPlay <= x_lower & ERPowerPlay > y_lower  ~ "Q2",
                                        wicketsPowerPlay <= x_lower & ERPowerPlay <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(wicketsPowerPlay,ERPowerPlay,color=quadrant)) +
            geom_text(aes(wicketsPowerPlay,ERPowerPlay,label=bowler,color=quadrant)) + geom_point() +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

        ggplotly(g)
    }

}


