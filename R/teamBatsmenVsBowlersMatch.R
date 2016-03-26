##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 21 Mar 2016
# Function: teamBatsmenVsBowlersMatch
# This function computes the performance of batsmen against different bowlers.
# The user has a choice of either taking the output as a plot or as a dataframe
#
###########################################################################################
teamBatsmenVsBowlersMatch <- function(match,theTeam,plot=TRUE)
{
    a <-filter(match,team==theTeam)
    # Summarise the performance of the batsmen against the bowlers vs total runs scored
    b <-summarise(group_by(a,batsman,bowler),sum(runs))
    names(b) <- c("batsman","bowler","runsConceded")

    if(plot == TRUE){

        # Plot the performance of the batsmen as a facted grid
        ggplot(data=b,aes(x=bowler,y=runsConceded,fill=factor(bowler))) +
            facet_grid(~ batsman) + geom_bar(stat="identity") +
            xlab("Opposition bowlers") + ylab("Runs scored") +
            ggtitle('Batsmen vs Bowlers in Match') +
            ggtitle(expression(atop('Batsmen vs Bowlers in Match',
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else{
        b
    }
}
