##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketPlot
# This function plots the moving average of wickets taken by bowler in career
#
###########################################################################################
bowlerWicketPlot <- function(df,name){
    
     c <- summarise(group_by(df,overs),meanRuns=mean(runs),meanMaidens=mean(maidens),
                    meanWickets=mean(wickets))
     
     plot.title <- paste(name,"- Average wickets vs overs")
     ggplot(c,aes(x=overs, y=meanWickets,fill=overs)) + 
         geom_bar(data=c,stat="identity" ) +
         xlab("Overs") + ylab("Mean Wickets") +
         ggtitle(bquote(atop(.(plot.title),
                             atop(italic("Data source:http://cricsheet.org/"),""))))
}