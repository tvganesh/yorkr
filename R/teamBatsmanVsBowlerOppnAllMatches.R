##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: teamBatsmanVsBowlersOppnAllMatches
# This function computes the best performing batsman against an opposition's bowlers
# in all matches with this team.The top 5 batsman are displayed by default
# 
#
###########################################################################################
teamBatsmanVsBowlersOppnAllMatches <- function(matches,main,opposition,plot=TRUE,top=5){
    
    a <-filter(matches,team==main)
    b <-summarise(group_by(a,batsman,bowler),sum(runs))
    names(b) <- c("batsman","bowler","runs")
    c <- summarise(b,runsScored=sum(runs))
    d <- arrange(c,desc(runsScored))
    
    # Pick 9 highest run givers
    d <- head(d,top)
    
    batsmen <- as.character(d$batsman)
    e <- NULL
    for(i in 1:length(batsmen)){
        f <- filter(b,batsman==batsmen[i])
        e <- rbind(e,f)
        
    }
    if(plot == TRUE){
        plot.title = paste("Batsmen vs bowlers -",main," Vs ",opposition,"(all matches)",sep="")
        ggplot(data=e,aes(x=bowler,y=runs,fill=factor(bowler))) +
            facet_grid(~ batsman) + geom_bar(stat="identity") +
            xlab("Bowler") + ylab("Runs Scored") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else{
        e
    }
}

