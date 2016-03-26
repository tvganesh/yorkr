##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: batsmanMovingAverage
# This function computes and plots the moving average  the batsman
#
###########################################################################################
batsmanMovingAverage <- function(df,name){

    b <- select(df,batsman,runs,date)
    
    plot.title = paste(name,"- Moving average of runs in career")
    ggplot(b) + geom_line(aes(x=date, y=runs),colour="darkgrey") +  
        geom_smooth(aes(x=date, y=runs)) + 
        xlab("Date") + ylab("Runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),"")))) 
}