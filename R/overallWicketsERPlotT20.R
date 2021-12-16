##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Nov 2021
# Function: overallWicketsERPlotT20
# This function plots the overall wickets vs ER in T20 men
#
###########################################################################################
#' @title
#' Ranks the T20 bowlers
#'
#' @description
#' This function plots the overall wickets vs ER in T20 men
#'
#' @usage
#' overallWicketsERPlotT20(dir=".",minMatches, dateRange,type="IPL",plot=1)
#'
#'
#' @param dir
#' The input directory
#'
#' @param minMatches
#' Minimum matches played
#'
#' @param dateRange
#' Date interval to consider
#'
#' @param type
#' T20 league
#'
#' @param plot
#' plot=1 (static),plot=2(interactive), plot=3 (table)
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
#'
#'@examples
#' \dontrun{
#' overallWicketsERPlotT20(dir=".",minMatches, dateRange,type="IPL",plot=1)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
overallWicketsERPlotT20 <- function(dir=".",minMatches, dateRange,type="IPL",plot=1) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=year=NULL
    wicketPlayerOut=opposition=venue=quantile=quadrant=bowlingDF=ggplotly=NULL
    currDir= getwd()
    bowlingDF<-NULL


    setwd(dir)
    bowlingDF<-NULL


    bowlingDetails <- paste(type,"-BowlingDetails.RData",sep="")
    print(bowlingDetails)
    load(bowlingDetails)
    print(dim(bowlingDF))

    # Note: If the date Range is NULL setback to root directory
    tryCatch({

        df=bowlingDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])

    },
    warning=function(war)
    {
        print(paste("NULL values: ", war))
    },
    error=function(err)
    {
        # Change to root directory on error
        setwd(currDir)
        cat("Back to root",getwd(),"\n")
    })


    # Compute number of matches played
    a=df %>% select(bowler,date) %>% unique()
    b=summarise(group_by(a,bowler),matches=n())

    # Compute wickets
    c <- filter(df,wicketPlayerOut != "nobody")
    d <- select(c,bowler,wicketPlayerOut,economyRate,date,opposition,venue)
    e <- summarise(group_by(d,bowler,date,economyRate),wickets=length(unique(wicketPlayerOut)))
    f=summarise(group_by(e,bowler), totalWickets=sum(wickets),meanER=mean(economyRate))

    # Join
    g=merge(b,f,by="bowler",all.x = TRUE)
    g[is.na(g)] <- 0
    h <- filter(g,matches >= minMatches)
    setwd(currDir)

    x_lower <- quantile(h$totalWickets,p=0.66,na.rm = TRUE)
    y_lower <- quantile(h$meanER,p=0.66,na.rm = TRUE)

    plot.title <- paste("Overall Wickets vs ER in ",type,sep="")
    if(plot == 1){ #ggplot2
        h %>%
            mutate(quadrant = case_when(totalWickets > x_lower & meanER > y_lower   ~ "Q1",
                                        totalWickets <= x_lower & meanER > y_lower  ~ "Q2",
                                        totalWickets <= x_lower & meanER <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(totalWickets,meanER,color=quadrant)) +
            geom_text(aes(totalWickets,meanER,label=bowler,color=quadrant)) + geom_point() +
            xlab("Wickets") + ylab("Economy rate") +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

    } else if(plot == 2){ #ggplotly
        g <-    h %>%
            mutate(quadrant = case_when(totalWickets > x_lower & meanER > y_lower   ~ "Q1",
                                        totalWickets <= x_lower & meanER > y_lower  ~ "Q2",
                                        totalWickets <= x_lower & meanER <= y_lower ~ "Q3",
                                        TRUE ~ "Q4")) %>%
            ggplot(aes(totalWickets,meanER,color=quadrant)) +
            geom_text(aes(totalWickets,meanER,label=bowler,color=quadrant)) + geom_point() +
            xlab("Wickets") + ylab("Economy rate") +
            geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
            geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
            ggtitle(plot.title)

        ggplotly(g)
    }

}
