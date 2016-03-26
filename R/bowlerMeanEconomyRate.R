##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerMeanEconomyRate
# This function gets the bowling details of a bowler
#
###########################################################################################
bowlerMeanEconomyRate <- function(df,name){

    c <- summarise(group_by(df,overs),meanEconomyRate=mean(economyRate))
             
    plot.title <- paste(name,"- Mean Economy Rate vs Overs")
    ggplot(c,aes(x=overs, y=meanEconomyRate,fill=overs)) + 
        geom_bar(data=c,stat="identity" ) +
        xlab("Overs") + ylab("Mean Economy Rate") +
        ggtitle(bquote(atop(.(plot.title),
                            atop(italic("Data source:http://cricsheet.org/"),""))))
    

}