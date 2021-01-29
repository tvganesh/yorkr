##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 28 Jan 2021
# Function: rankT20Bowlers
# This function ranks the t20 bowlers
#
###########################################################################################
#' @title
#' Ranks the T20 bowlers
#'
#' @description
#' This function creates a single datframe of all T20 bowlers and then ranks them
#'
#' @usage
#' rankT20Bowlers(teamNames,odir=".",minMatches, yearSelected, wicketsVsER)
#'
#' @param teamNames
#' The team names
#'
#' @param odir
#' The output directory
#'
#' @param minMatches
#' Minimum matches played
#'
#' @param yearSelected
#'  Selected year
#'
#' @param wicketsVsER
#' Wickets  or economy rate
#'
#' @return The ranked T20 bowlers
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
#' rankT20Bowlers(teamNames,odir=".",minMatches, yearSelected, wicketsVsER)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Batsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
rankT20Bowlers <- function(teamNames,odir=".",minMatches, yearSelected, wicketsVsER) {
    bowlingDetails=bowler=wickets=economyRate=matches=meanWickets=meanER=totalWickets=year=NULL
    wicketPlayerOut=opposition=venue=NULL
    teams = unlist(teamNames)
    currDir= getwd()

    #Change dir
    setwd(odir)
    bowlingDF<-NULL

    # Compute wickets by bowler in each team
    o <- data.frame(bowler=character(0),wickets=numeric(0),economyRate=numeric(0))
    for(team1 in teams){
        bowlingDetails <- NULL
        val <- paste(team1,"-BowlingDetails.RData",sep="")
        print(val)
        tryCatch(load(val),
                 error = function(e) {
                     print("No data1")
                     setNext=TRUE
                 }


        )
        details <- bowlingDetails
        bowlingDF <- rbind(bowlingDF,details)
    }

    maxDate= max(bowlingDF$date)
    minDate= min(bowlingDF$date)
    maxYear = lubridate::year(maxDate)
    minYear = lubridate::year(minDate)
    cat("year selected**********************=",yearSelected,"\n")

    # Filter out from data frame greater than date
    dateValue=as.Date(paste(yearSelected,"-01-01",sep=""))
    if (dateValue < minDate)
        dateValue=minDate
    df=bowlingDF %>% filter(date > as.Date(dateValue))

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
    if(wicketsVsER == "Wickets over Economy rate"){
          T20BowlersRank <- arrange(h,desc(totalWickets),desc(meanER))
    } else if(wicketsVsER == "Economy rate over Wickets"){

        T20BowlersRank <- arrange(h,meanER,desc(totalWickets))
    }
    T20BowlersRank <- distinct(T20BowlersRank)
    T20BowlersRank

}
