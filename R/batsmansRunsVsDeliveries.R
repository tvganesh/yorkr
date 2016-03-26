##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: batsmanRunsVsDeliveries
# This function plots the runs scored vs deliveries faced
#
###########################################################################################
batsmanRunsVsDeliveries <- function(df,name){
    
    plot.title = paste(name,"- Runs vs balls faced")
    ggplot(df,aes(x=ballsPlayed,y=runs)) + 
        geom_point(size=2) + geom_smooth() +
        xlab("Deliveries faced") + ylab("Runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),"")))) 
        
}