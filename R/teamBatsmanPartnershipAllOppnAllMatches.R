##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Mar 2016
# Function: teamBatsmanPartnershiAllOppnAllMatches
# This function computes the partnetship of the batsman against all opposition
#
#
###########################################################################################
#' @title
#' This function gets and returns the batting partnership of a team in all matches against all
#' oppositions
#'
#' @description
#' This function computes the batting partnership of a team againt all oppositions in all matches
#' This function returns a dataframe which is a summary of the batsman with the highest partnerships
#' or the partnership of an individual batsman
#'
#' @usage
#' teamBatsmanPartnershiAllOppnAllMatches(matches,theTeam,report="summary")
#'
#' @param matches
#' All the matches of the team against all oppositions
#'
#' @param theTeam
#' The team for which the the batting partnerships are sought
#'
#'@param report
#' if the report="summary" then the data frame returned gives a list of the batsmen with the highest
#' partnerships. If report="detailed" then the detailed breakup of the partnership is returned.
#'
#' @return partnerships
#' The data frame with the partnerships
#'
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' # Get all matches for team India against all oppositions
#' m <-teamBattingScorecardAllOppnAllMatches(matches,theTeam="India")
#' # Get the  summary report
#' teamBatsmanPartnershiAllOppnAllMatches(matches,theTeam='India')
#'
#' # Get the detailed report
#' teamBatsmanPartnershiAllOppnAllMatches(matches,theTeam='India',report="detailed")
#'
#' @seealso
#' \code{\link{teamBatsmanvsBowlersAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanPartnershipOppnAllMatchesChart}}
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanVsBowlerOppnAllMatches}}
#'
#' @export
#'
teamBatsmanPartnershiAllOppnAllMatches <- function(matches,theTeam,report="summary"){


    a <-filter(matches,team==theTeam)
    #Get partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","partnershipRuns")
    b <- summarise(group_by(df,batsman),totalRuns=sum(partnershipRuns))
    c <- arrange(b,desc(totalRuns))
    d <- full_join(df,c,by="batsman")
    if(report == "detailed"){
        partnerships <- arrange(d,desc(totalRuns))
    } else{
        partnerships <- arrange(c,desc(totalRuns))
    }
    partnerships
}
