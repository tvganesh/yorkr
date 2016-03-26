##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWicketsVenue
# This function plots the performance of bowlers at different venues
#
###########################################################################################
bowlerWicketsVenue <- function(df,name){

    c <- summarise(group_by(df,venue),meanWickets=mean(wickets),numMatches=n())
    d <- mutate(c,venue=paste(venue,"(",numMatches,")",sep=""))
    e <- arrange(d,desc(meanWickets))
    f <- e[1:20,]
    plot.title = paste(name,"- Wickets in venue(number innings)")
    ggplot(f, aes(x=venue, y=meanWickets, fill=venue))+
        geom_bar(stat = "identity",position="dodge") +
        geom_hline(aes(yintercept=2))+
        xlab("Venue") + ylab("Average wickets taken") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}