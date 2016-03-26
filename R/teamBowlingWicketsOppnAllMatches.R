##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 23 Mar 2016
# Function: teamBowlerWicketsOppnAllMatches
# This function computes the total wickets taken by the top 20(default) bowlers against the
# opposition
# 
#
###########################################################################################
teamBowlerWicketsOppnAllMatches <- function(matches,main,opposition,plot=TRUE,top=20){
    
    #Filter the matches by the team
    a <-filter(matches,team!=main)
    
    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub("\\.\\d+","",over))
    
    #Compute number of wickets
    c <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    d <- summarise(group_by(c,bowler),wickets=length(unique(wicketPlayerOut)))
    
    e <- arrange(d,desc(wickets))
    e <- e[1:top,]
    
    names(e) <- c("bowler","wickets")
    
    if(plot==TRUE){
        ggplot(data=e,aes(x=bowler,y=wickets,fill=factor(bowler))) + 
             geom_bar(stat="identity") +
            #facet_wrap( ~ bowler,scales = "free", ncol=3,drop=TRUE) + #Does not work.Check!
            xlab("Batsman") + ylab("Total wickets") +
            ggtitle(expression(atop("Performances of bowlers against opposition",
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else{
        e
    }
}
