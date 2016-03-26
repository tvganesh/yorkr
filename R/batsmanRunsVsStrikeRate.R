##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: batsmanRunsVsStrikeRate
# This function plots the runs scored versus the strike rate
###########################################################################################
batsmanRunsVsStrikeRate <- function(df,name){
    
    b <- select(df,batsman,runs,strikeRate)
    
    plot.title = paste(name,"- Runs vs Strike Rate")
    ggplot(b) + geom_point(aes(x=runs, y=strikeRate),colour="darkgrey") +  
        geom_smooth(aes(x=runs, y=strikeRate)) + 
        xlab("Strike rate(%)") + ylab("Runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),"")))) 
}