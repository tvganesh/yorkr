##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 May 2020
# Function: getTeamBowlingDetails
# This function uses gets the bowling details of a team
#
###########################################################################################
#' @title
#' Get the team bowling details
#'
#' @description
#' This function  gets the bowling details of a team in all matchs against all
#' oppositions. This gets all the details of the bowlers for e.g deliveries, maidens, runs,
#' wickets, venue, date, winner ec
#'
#' @usage
#' getTeamBowlingDetails(team,dir=".",save=FALSE, odir=".")
#'
#' @param team
#' The team for which detailed bowling info is required
#'
#' @param dir
#' The source directory of RData files obtained with  convertAllYaml2RDataframes()
#'
#' @param odir
#' The output directory
#'
#' @param save
#' Whether the data frame needs to be saved as RData or not. It is recommended to set save=TRUE
#' as the data can be used for a lot of analyses of batsmen
#'
#' @return bowlingDetails
#' The dataframe with the bowling details
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
#' a <- getTeamBowlingDetails("India",dir="../data",save=TRUE,odir=".")
#' }
#'
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
getTeamBowlingDetails <- function(team,dir=".",save=FALSE,odir="."){
    overs=bowler=NULL
    a <- paste(dir,"/","*",team,"*",sep="")
    # Gather team against all ooposition
    fl <- Sys.glob(a)
    bowlingDetails <- NULL
    for(i in 1:length(fl)){
        # Add try-catch to handle issues
        tryCatch({
            load(fl[i])
            match <- overs
            details <- teamBowlingPerfDetails(match,team,includeInfo=TRUE)
            # If the side has not batted details will be NULL. Skip in that case
            if(!is.null(dim(details))){
                bowlingDetails <- rbind(bowlingDetails,details)
            }else {
                #print("Empty")

                next
            }
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }

    if(save==TRUE){
        fl <- paste(odir,"/",team,"-BowlingDetails.RData",sep="")
        save(bowlingDetails,file=fl)
    }
    bowlingDetails <- arrange(bowlingDetails,bowler,date)
    bowlingDetails


}
