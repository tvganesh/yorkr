##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBatsmanVsBowlersAllOppnAllMatchesPlot
# This function computes performance of batsmen/batsman against bowlers of the opposition.
# It provides the names of the bowlers against whom the batsman scored the most.
# This is plotted as a chart
###########################################################################################
teamBatsmanVsBowlersAllOppnAllMatchesPlot <- function(df,plot=TRUE)
{
    
    bman <- df$batsman
    if(plot==TRUE){
        plot.title <- paste(bman,"-Performances against all bowlers ODIs")
        ggplot(data=df,aes(x=bowler,y=runs,fill=factor(bowler))) +
            facet_grid(~ batsman) + geom_bar(stat="identity") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }else{
        df
    }
    
}
