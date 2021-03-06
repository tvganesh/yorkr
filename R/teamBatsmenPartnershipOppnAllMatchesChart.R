##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: teamBatsmenPartnershipOppnAllMatchesChart
# This function computes the batting partnership of a team in all matches against
# an opposition. The report generated can be detailed or a summary
#
###########################################################################################
#' @title
#' Plot of team partnership all matches against an opposition
#'
#' @description
#' This function plots  the batting partnership of a team againt all oppositions in all matches
#' This function also returns a  dataframe  with the batting partnerships
#'
#' @usage
#' teamBatsmenPartnershipOppnAllMatchesChart(matches,main,opposition, plot=1)
#'
#' @param matches
#' All the matches of the team against all oppositions
#'
#' @param main
#' The main team for which the the batting partnerships are sought
#'
#' @param opposition
#' The opposition team for which the the batting partnerships are sought
#'
#' @param plot
#'  Plot=1 (static),plot=2(interactive),plot=3(table)
#'
#' @return None or partnerships
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
#' # Get all matches for team India against all oppositions
#' d <- teamBatsmenVsBowlersAllOppnAllMatchesRept(matches,"India",rank=1,dispRows=50)
#'  #Plot the partnerships
#' teamBatsmenVsBowlersAllOppnAllMatchesPlot(d)
#'
#' #Do not plot but get the dataframe
#' e <- teamBatsmenVsBowlersAllOppnAllMatchesPlot(d,plot=FALSE)
#' }
#'
#' @seealso
#' \code{\link{teamBatsmenPartnershipAllOppnAllMatches}}\cr
#' \code{\link{teamBatsmenPartnershipOppnAllMatchesChart}}\cr
#' \code{\link{teamBatsmenVsBowlersAllOppnAllMatchesPlot}}\cr
#' \code{\link{teamBatsmenVsBowlersOppnAllMatches}}\cr
#'
#' @export
#'
teamBatsmenPartnershipOppnAllMatchesChart <- function(matches,main,opposition,plot=1){
    team=batsman=nonStriker=runs=partnershipRuns=totalRuns=NULL
    ggplotly=NULL
    a <-filter(matches,team==main)
    #Get partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","runs")

    df <- arrange(df,desc(runs))

    print("here")
    cat("plot=")
    plot.title = paste(main," Batting partnership ","(against ",opposition," all matches)",sep="")

    # Plot the data
    if(plot == 1){ #ggplot2
        ggplot(data=df,aes(x=batsman,y=runs,fill=nonStriker))+
            geom_bar(data=df,stat="identity") +
            xlab("Batsman") + ylab("Partnership runs") +
            ggtitle(bquote(atop(.(plot.title),
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else if(plot == 2){ #ggplotly
        g <- ggplot(data=df,aes(x=batsman,y=runs,fill=nonStriker))+
            geom_bar(data=df,stat="identity") +
            xlab("Batsman") + ylab("Partnership runs") +
            ggtitle(plot.title) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        ggplotly(g)


    } else{
        df
    }


}
