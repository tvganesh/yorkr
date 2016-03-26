##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: teamBowlersVsBatsmanMatch
# This function computes  performance of the team bowlers against the opposition batsmen
#
###########################################################################################
teamBowlersVsBatsmanMatch <- function(match,theTeam,plot=TRUE){
    a <-filter(match,team==theTeam)
    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")
    
    # Output plot or dataframe
    if(plot == TRUE){
        ggplot(data=b,aes(x=batsman,y=runsConceded,fill=factor(batsman))) + 
            facet_grid(. ~ bowler) + geom_bar(stat="identity") + 
            ggtitle(expression(atop("Bowler vs Batsman",
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else{
        b
    }
}
