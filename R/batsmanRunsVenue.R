##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: batsmanRunsVenue
# This function computes and plots the runs scored by the batsman at different venues
#
###########################################################################################
batsmanRunsVenue <- function(df,name){
    
    b <- select(df,batsman,runs,venue)
    c <- summarise(group_by(b,venue),meanRuns=mean(runs),numMatches=n())
    d <- mutate(c,venue=paste(venue,"(",numMatches,")",sep=""))
    e <- arrange(d,desc(numMatches))

    f <- e[1:25,]
    plot.title = paste(name,"- Mean runs at venue")
    ggplot(f, aes(x=venue, y=meanRuns, fill=venue))+
        geom_bar(stat = "identity",position="dodge") +
        geom_hline(aes(yintercept=50))+
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
}