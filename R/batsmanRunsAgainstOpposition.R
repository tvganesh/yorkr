##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: batsmanRunsAgainstOpposition
# This function computes and plots the runs scored by the batsman against different oppositions
#
###########################################################################################
batsmanRunsAgainstOpposition <- function(df,name){
    b <- select(df,batsman,runs,opposition)
    c <-b[complete.cases(b),]
    d <- summarise(group_by(c,opposition),meanRuns=mean(runs),numMatches=n())
    plot.title = paste(name,"- Runs against opposition")
    ggplot(d, aes(x=opposition, y=meanRuns, fill=opposition))+
        geom_bar(stat = "identity",position="dodge") +
        xlab("Opposition") + ylab("Runs") +
        geom_hline(aes(yintercept=50))+
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))
}