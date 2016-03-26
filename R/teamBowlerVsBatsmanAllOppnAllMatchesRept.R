##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBowlersVsBatsmanAllOppnAllMatchesRept
# This function computes the performance of bowlers of team against all opposition in all matches
# This function returns a dataframe
#
###########################################################################################
#' @title
#' Compute the performance of bowlers of a team against all opposition in all matches
#'
#' @description
#' This function computes performance of bowlers of a team against all opposition in all matches
#'
#' @usage
#' teamBowlersVsBatsmanAllOppnAllMatchesRept(matches,theTeam,rank=0)
#'
#' @param matches
#' the data frame of all matches between a team and aall opposition  and all obtained with
#' the call getAllMatchesAllOpposition()
#'
#' @param theTeam
#' The team against which the performance is requires
#'
#' @param rank
#' When the rank is 0 then the performance of all the bowlers is displayed. If rank=n (1,2,3 ..) then
#' the performance of that bowler is given
#'
#' @return dataframe
#' The dataframe with all performances
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
#' # Get all matches between India and all oppostions
#' matches <-getAllMatchesAllOpposition("India",dir="../data/",save=TRUE)
#'
#' # You could also load directly from the saved file
#' #load("allMatchesAllOpposition-India.RData")
#' # The call below gives the best bowlers against India
#' teamBowlersVsBatsmanAllOppnAllMatchesRept(matches,theTeam="India",rank=0)
#'
#' # The call with rank=1 gives the performace of the bowler with rank
#'  teamBowlersVsBatsmanAllOppnAllMatchesRept(matches,theTeam="India",rank=1)
#'
#' # The call below gives the overall performance of India bowlers against South Africa
#'  teamBatsmanVsBowlersAllOppnAllMatchesRept(matches,"South Africa",rank=0)
#'
#' # The call below gives the performance of best Indias bowlers against Australia
#' teamBowlersVsBatsmanAllOppnAllMatchesRept(matches,"Australia",rank=1)
#'
#' @seealso
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatches}}
#' \code{\link{teamBatsmanPartnershipAllOppnAllMatchesPlot}}
#' \code{\link{teamBatsmanPartnershipOppnAllMatchesChart}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesMain}}
#' \code{\link{teamBowlersVsBatsmanAllOppnAllMatchesPlot}}
#' @export
#'
teamBowlersVsBatsmanAllOppnAllMatchesRept <- function(matches,theTeam,rank=0) {


    a <-filter(matches,team==theTeam)

    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")
    # Compute total runs conceded
    c <- summarise(group_by(b,bowler),runs=sum(runsConceded))
    # Sort by descneding
    d <- arrange(c,desc(runs))


    # Initialise to NULL
    f <- NULL
    if(rank == 0){
        f <- head(d,10)
    } else { # display dispRows for selected bowler with rank
       # Pick the chosen bowler
        bwlr <- d[rank,]

        f <- filter(b,bowler==bwlr$bowler)
        f <- arrange(f,desc(runsConceded))
    }
    f


}
