##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMovingAverage
# This function plots the moving average of wickets taken by bowler in career
#
###########################################################################################
bowlerMovingAverage <- function(df,name){

    c <- select(df,bowler,wickets,date)
    
    plot.title = paste(name,"- Moving average of wickets in career")
    ggplot(c) + geom_line(aes(x=date, y=wickets),colour="darkgrey") +  
        geom_smooth(aes(x=date, y=wickets)) + 
        xlab("Date") + ylab("Runs") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),"")))) 
}