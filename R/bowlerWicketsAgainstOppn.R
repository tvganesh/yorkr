##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketsAgainstOpposition
# This function plots the performance of bowlers against different oppositions
#
###########################################################################################
bowlerWicketsAgainstOpposition <- function(df,name){

    c <- summarise(group_by(df,opposition),meanWickets=mean(wickets),numMatches=n())
    d <- mutate(c,opposition=paste(opposition,"(",numMatches,")",sep=""))
    plot.title = paste(name,"- Wickets against Opposition(number innings)")
    ggplot(d, aes(x=opposition, y=meanWickets, fill=opposition))+
        geom_bar(stat = "identity",position="dodge") +
        geom_hline(aes(yintercept=2))+
        xlab("Opposition") + ylab("Average wickets taken") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))
}
