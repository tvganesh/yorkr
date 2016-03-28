##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Mar 2016
# Function: getAllMatchesAllOpposition
# This function gets the data for all matches against all opposition for a given team.
# The user can choose to save the file for later use
#
#
###########################################################################################
#' @title
#' This function gets all the data on all matches against all opposition for a team
#'
#' @description
#' This function gets all the matches for a particular team for e.g India, Pakistan,
#' Australia etc against all other oppositions. It constructs a huge dataframe of all these
#' matches. This can be saved by the user which can be used in function in which analyses are
#' done for all matches and for all oppositions. This is done by loading the saved .RData for
#' each match and performing an rbind of the data frames for each match
#'
#' @usage
#' getAllMatchesAllOpposition(team,dir=".",save=FALSE)
#'
#' @param team
#' The team for which all matches and all opposition has to be obtained e.g. India, Pakistan
#'
#' @param dir
#' The directory in which the saved .RData files exist
#'
#'@param save
#' Default=FALSE. This parameter indicates whether the combined data frame needs to be saved or not. It is recommended
#' to save this large dataframe as the creation of this data frame takes a several seconds depending
#' on the number of matches
#'
#' @return match
#' The combined data frame
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' # Get all matches for team India
#' getAllMatchesAllOpposition("India",dir="../data/",save=TRUE)
#' getAllMatchesAllOpposition("Australia",dir="./mysavedata/",save=TRUE)
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}
#' \code{\link{bowlerWicketPlot}}
#' \code{\link{bowlerWicketsVenue}}
#' \code{\link{bowlerMeanRunsConceded}}
#'
#' @export
#'

getAllMatchesAllOpposition <- function(team,dir=".",save=FALSE){
    overs=NULL
    # Gather team data  against all ooposition
    d1 <- paste("*",team,"*",sep="")
    path=paste(dir,"/",d1,sep="")
    fl <- Sys.glob(path)


    matches <- NULL
    for(i in 1:length(fl)){
        load(fl[i])
        matches <- rbind(matches,overs)
    }
    b <- paste("allMatchesAllOpposition-",team,".RData",sep="")
    if(save){
        save(matches,file=b)
    }
    matches
}
