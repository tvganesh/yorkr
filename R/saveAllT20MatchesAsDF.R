##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 07  Dec 2021
# Function: saveAllT20MatchesAsDF
# This function ranks the  T20  batsmen
#
#
###########################################################################################
#' @title
#' Overall picture Ranks the T20 batsmen
#'
#' @description
#' This function creates a single datframe of all T20 batsmen and then ranks them
#' @usage
#' saveAllT20MatchesAsDF(teamNames,dir=".",odir=".",type="IPL",save=TRUE)
#'
#' @param teamNames
#' The team names
#'
#' @param dir
#' The output directory
#'
#' @param odir
#' The output directory
#'
#' @param type
#' T20 format
#'
#' @param save
#' To save or not
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
#' saveAllT20MatchesAsDF(teamNames,dir=".",odir=".",type="IPL",save=TRUE)
#' }
#'
#' @seealso
#' \code{\link{rankODIBowlers}}\cr
#' \code{\link{rankODIBatsmen}}\cr
#' \code{\link{rankT20Bowlers}}\cr
#' @export
#'
saveAllT20MatchesAsDF <- function(teamNames,dir=".",odir=".",type="IPL",save=TRUE){

  cat("Entering rank Batsmen1 \n")
  currDir= getwd()
  cat("T20batmandir=",currDir,"\n")
  battingDetails=batsman=runs=strikeRate=matches=meanRuns=meanSR=battingDF=val=year=overs=NULL

  teams = unlist(teamNames)

  t20MDF <-NULL
  a <- paste(dir,"/","*",".RData",sep="")
  fl <- Sys.glob(a)
  for(i in 1:length(fl)){
    # Add try-catch to handle issues
    tryCatch({
      load(fl[i])
      match <- overs

      # If the side has not batted details will be NULL. Skip in that case
      if(!is.null(dim(match))){
        t20MDF <- rbind(t20MDF,match)
      }else {
        #print("Empty")

        next
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  if(save==TRUE){
    fl <-paste(odir,"/",type,"-MatchesDataFrame.RData",sep="")
    print(fl)
    save(t20MDF,file=fl)
  }

}
