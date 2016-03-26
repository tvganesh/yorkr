##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBowlerVsBatsmanAllOppnAllMatchesPlot
# This function computes the performance of bowlers against batsman of opposition
#
###########################################################################################
teamBowlerVsBatsmanAllOppnAllMatchesPlot <- function(bowlerDF,t1,t2){

    bwlr <- bowlerDF$bowler
    if(t2 != "India"){
        plot.title <- paste(bwlr,"-Performance against",t2,"batsmen")
        print("aa")
    }else{
        plot.title <- paste(bwlr,"-Performance against all batsmen")
    }
    ggplot(data=bowlerDF,aes(x=batsman,y=runsConceded,fill=factor(batsman))) +
        facet_grid(. ~ bowler) + geom_bar(stat="identity") +
        xlab("Batsman") + ylab("Runs conceded") +
        ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
