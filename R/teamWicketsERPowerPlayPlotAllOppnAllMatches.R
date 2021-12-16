##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Nov 2021
# Function: teamWicketsERPowerPlayPlotAllOppnAllMatches
# This function computes the  wickets vs ER of team in powewrplay against all opposition in all matches
#
###########################################################################################
#' @title
#' Team wickets vs ER in powewrplay against  all opposition all matches
#'
#' @description
#' This function computes wickets vs ER in powewrplay against all oppositions in all matches
#'
#' @usage
#' teamWicketsERPowerPlayPlotAllOppnAllMatches(matches,t1, plot=1)
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
#' teamWicketsERPowerPlayPlotAllOppnAllMatches(matches, t1, plot=1)
#'}
#' @seealso
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesMain}}\cr
#' \code{\link{teamBowlersVsBatsmenAllOppnAllMatchesPlot}}\cr
#'
#' @export
#'


teamWicketsERPowerPlayPlotAllOppnAllMatches <- function(matches,t1,plot=1) {
    team=ball=totalRuns=total=wickets=wicketsPowerPlay=wicketsMiddleOvers=wicketsDeathOvers=bowler=str_extract=NULL
    ggplotly=wicketPlayerOut=str_extract=quantile=quadrant=ERPowerPlay=NULL

    # Filter the performance of team1
    a <-filter(matches,team!=t1)
    # Power play
    a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
    a2 <- select(a1,date,bowler,wicketPlayerOut)
    a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
    a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsPowerPlay=sum(wickets))

    a21 <- select(a1,team,bowler,date,totalRuns)
    a31 <- a21 %>% group_by(bowler) %>% summarise(total=sum(totalRuns),count=n(), ERPowerPlay=total/count *6)
    a41 <- a31 %>% select(bowler,ERPowerPlay)
    a42=inner_join(a4,a41,by="bowler")

    x_lower <- 1/2 * min(a42$wicketsPowerPlay + max(a42$wicketsPowerPlay))
    y_lower <- 1/2 * min(a42$ERPowerPlay + max(a42$ERPowerPlay))

    x_lower <- quantile(a42$wicketsPowerPlay,p=0.66,na.rm = TRUE)
    y_lower <- quantile(a42$ERPowerPlay,p=0.66,na.rm = TRUE)


    plot.title <- paste("Wickets-ER Plot of", t1, "in Power play against all opposition all matches")
    if(plot == 1){ #ggplot2
        a42 %>%
            mutate(quadrant = case_when(wicketsPowerPlay > x_lower & ERPowerPlay > y_lower   ~ "Q1",
                                        wicketsPowerPlay <= x_lower & ERPowerPlay > y_lower  ~ "Q2",
                                        wicketsPowerPlay <= x_lower & ERPowerPlay <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(wicketsPowerPlay,ERPowerPlay,color=quadrant)) +
            geom_text(aes(wicketsPowerPlay,ERPowerPlay,label=bowler,color=quadrant)) + geom_point() +
            xlab("Wickets - Power play") + ylab("Economy rate - Power play") +
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
            xlab("Wickets - Power play") + ylab("Economy rate - Power play") +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

        ggplotly(g)
    }

}


