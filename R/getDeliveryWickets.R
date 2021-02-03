##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 May 2020
# Function: getDeliveryWickets
# This function creates a data frame of delivery and wickets
#
###########################################################################################
#' @title
#' Get datframe of deliveries bowled and wickets taken
#'
#' @description
#' This function  creates a data frame of deliveries bowled and wickets taken. This data frame is
#' then used by bowlerWktsPredict to predict the number of deliveries to wickets taken
#'
#' @usage
#' getDeliveryWickets(team,dir=".",name,save=FALSE)
#'
#' @param team
#' The team for which dataframe is to be obtained
#'
#' @param dir
#' The source directory in which the match .RData files exist
#'
#' @param name
#' The name of the bowler
#'
#' @param save
#' Whether the data frame needs to be saved to a file or nor
#'
#' @return dataframe
#' The dataframe of delivery wickets
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
#' # Create a data frame of deliveries to wickets from the stored .RData files
#' jadeja1 <- getDeliveryWickets(team="India",dir="../data",name="Jadeja",save=FALSE)
#'
#' # Use this to create a classification tree of deliveries to wickets
#' bowlerWktsPredict(jadeja1,"RA Jadeja")
#' }
#'
#' @seealso
#' \code{\link{bowlerMovingAverage}}\cr
#' \code{\link{getTeamBowlingDetails}}\cr
#' \code{\link{bowlerWktsPredict}}\cr
#' \code{\link{teamBowlersWicketRunsOppnAllMatches}}
#'
#' @export
#'
getDeliveryWickets <- function(team,dir=".",name,save=FALSE){
    overs=bowlingDetails=NULL
    a <- paste(dir,"/","*",team,"*",sep="")

    # Gather team against all opposition
    fl <- Sys.glob(a)
    deliveryWKts <- NULL
    for(i in 1:length(fl)){
        tryCatch({
        load(fl[i])
            match <- bowlingDetails
            #print(i)
            #print(dim(match))
            details <- bowlerDeliveryWickets(match,team,name)
            # If the side has not batted details will be NULL. Skip in that case
            if(!is.null(dim(details))){
                deliveryWKts <- rbind(deliveryWKts,details)
            }else {
                #print("Empty")

                next
            }
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(names(deliveryWKts))
    deliveryWKts
}
