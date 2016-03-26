##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMeanRunsConceded
# This function plots the overs versus the mean runs conceded
#
###########################################################################################
bowlerMeanRunsConceded <- function(df,name){

    c <- summarise(group_by(df,overs),meanRuns=mean(runs),meanMaidens=mean(maidens),
                   meanWickets=mean(wickets))
    plot.title <- paste(name,"- Average runs conceded vs Overs")
    ggplot(c,aes(x=overs, y=meanRuns,fill=overs)) + 
        geom_bar(data=c,stat="identity" ) +
        xlab("Overs") + ylab("Average runs conceded") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))
}