##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Jan 2022
# Function: bowlerVsBatsmanPerf
# This function computes the performance of the bowler vs batsman
#
###########################################################################################
#' @title
#' Performance of bowler vs batsman
#'
#' @description
#' This function computes the performance of bowler vs batsman
#'
#' @usage
#' bowlerVsBatsmanPerf(t20MDF,batsman1,bowler1)
#'
#' @param t20MDF
#' Dataframe
#'
#' @param batsman1
#' The batsman
#'
#' @param bowler1
#' The bowler
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
#' bowlerVsBatsmanPerf(t20MDF,batsman1,bowler1)
#' }
#' @seealso
#' \code{\link{batsmanFoursSixes}}\cr
#' \code{\link{batsmanRunsVsDeliveries}}\cr
#' \code{\link{batsmanRunsVsStrikeRate}}\cr
#'
#'
#' @export
#'
bowlerVsBatsmanPerf <- function(t20MDF,batsman1,bowler1){
    batsman=bowler=runs=wides=noballs=wicketKind=wicketPlayerOut=runsConceded=balls=ER=wicketTaken=NULL
    a <- t20MDF %>% filter(batsman==batsman1 & bowler==bowler1)

    b <- select(a,batsman,bowler,runs,wides,noballs,wicketKind,wicketPlayerOut)
    c <- summarise(group_by(a,bowler),balls=n(), runsConceded=sum(runs,wides,noballs))
    d <- c %>% mutate(ER=runsConceded*6/balls) %>% select(balls,runsConceded,ER)
    e <- b %>% select(bowler,batsman,wicketKind,wicketPlayerOut)
    f <- e %>% filter(wicketPlayerOut==batsman1) %>% summarise(wicketTaken=n())
    if(nrow(a) == 0){
        df <- data.frame(matrix(ncol = 4, nrow = 0))
        names <- c("balls","runsConceded","ER","wicketTaken")
        colnames(df) <- names
        df[1,] <- c(0,0,0,0)
        g <- cbind(bowler1,batsman1,df)

    }
    else{
       g <- cbind(bowler1,batsman1,d,f)
       g <- g %>% arrange(bowler1)
    }
    g
}
