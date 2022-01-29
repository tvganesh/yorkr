##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Jan 2022
# Function: batsmanVsBowlerPerf
# This function computes the performance of the batsman vs bowler
#
###########################################################################################
#' @title
#' Performance of batsman vs bowler
#'
#' @description
#' This function computes the performance of batsman vs bowler
#'
#' @usage
#' batsmanVsBowlerPerf(t20MDF,batsman1,bowler1))
#'
#' @param df
#' Dataframe
#'
#' @param batsman
#' The batsman
#'
#' @param bowler1
#' The bowler
#'
#'
#' #@param dateRange
#' #Date Range
#'
#'
#' @return None
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
#' batsmanVsBowlerPerf((t20MDF,batsman1,bowler1))
#' }
#' @seealso
#' \code{\link{batsmanFoursSixes}}\cr
#' \code{\link{batsmanRunsVsDeliveries}}\cr
#' \code{\link{batsmanRunsVsStrikeRate}}\cr
#'
#'
#' @export
#'

batsmanVsBowlerPerf <- function(t20MDF,batsman1,bowler1){
    print(batsman1)
    print(bowler1)
    print(dim(t20MDF))
    if(is.null(batsman1) || is.null(bowler1))
          return

    a <- t20MDF %>% filter(batsman==batsman1 & bowler==bowler1)
    b <- select(a,batsman,bowler,runs)
    c <- b %>% summarize(ballsFaced=n(),totalRuns=sum(runs))

    d <- b %>%
        mutate(fours=(runs>=4 & runs <6)) %>%
        filter(fours==TRUE) %>% summarise(fours=n())

    e <- b %>%
        mutate(sixes=(runs ==6)) %>%
        filter(sixes == TRUE) %>% summarise(sixes=n())

    f <- cbind(batsman1,bowler1,c,d,e)
    g <- f %>% mutate(SR=(totalRuns/ballsFaced)*100)
    h <- select(a,batsman,bowler,wicketPlayerOut)
    i <- h %>% filter(wicketPlayerOut==batsman1) %>% summarise(timesOut=n())
    j <- cbind(g,i)
    j

}
