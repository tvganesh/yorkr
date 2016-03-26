##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: batsmanDismissals
# This function computes and plots the dismissal types for the batsman
#
###########################################################################################
#' @title
#' Compute and plot the dismissal type of the batsman
#'
#' @description
#' This function computes and plots the type of dismissals of the
#' the batsman
#' @usage
#' batsman4s(df, name="Kohli")
#'
#' @param df
#' Data frame
#'
#' @param name
#' Name of batsman
#'
#' @return None
#' @references
#' \url{http://cricsheet.org/}\cr
#' \url{https://gigadom.wordpress.com/}
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' # Get the data frame for Kohli
#' kohli <- getBatsmanDetails(team="India",name="Kohli")
#' batsmanDismissals(kohli,"Kohli")
#'
#' @seealso
#' \code{\link{batsmanFoursSixes}}
#' \code{\link{batsmanRunsVsDeliveries}}
#' \code{\link{batsmanRunsVsStrikeRate}}
#'
#' @export
#'

batsmanDismissals <- function(df,name){
    b <-select(df,batsman,wicketKind)

    c <- summarise(group_by(b,batsman,wicketKind),dismissal=n())
    d <- mutate(c,wicketKind=paste(wicketKind,"(",dismissal,")",sep=""))
    names(d) <-c("batsman","DismissalType","dismissal")
    plot.title = paste(name,"- Dismissals")
    ggplot(d, aes(x="", y=dismissal, fill=DismissalType))+
        geom_bar(width=1,stat = "identity")+
        coord_polar("y", start=0) +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))


}
