##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBowlingWicketKindAllOppnAllMatches
# This function computes the wicket kind of bowlers against all  opposition
#
###########################################################################################
teamBowlingWicketKindAllOppnAllMatches <- function(match,t1,t2="All",plot=TRUE){
    
    #a <-filter(match,team==theTeam)
    a <- NULL
    if(t2 == "All"){
        a <-filter(matches,team==t1)
    } else {
        a <-filter(matches,team==t2)
    }
    
    a1 <- unlist(strsplit(a$ball[1],"\\."))
    # Create a string for substitution 1st or 2nd
    a2 <- paste(a1[1],"\\.",sep="")
    
    # only wides and noballs need to be included with runs for bowlers.
    # Note: byes and legbyes should not be included
    b <-  a %>%
        select(bowler,ball,noballs,wides,runs,wicketKind,wicketPlayerOut) %>%
        #mutate(over=gsub("1st\\.","",ball)) %>%
        mutate(over=gsub(a2,"",ball)) %>%
        mutate(over=gsub("\\.\\d+","",over))
    
    
    #Compute number of wickets (remove nobody)
    c <- b %>%
        select(bowler,wicketKind,wicketPlayerOut) %>%
        filter(wicketPlayerOut != "nobody")
    
    # Count wickets by bowlers
    d <- summarise(group_by(c,bowler),wickets=length(wicketPlayerOut))
    
    # Arrange in descending order
    e <- arrange(d,desc(wickets))
    
    # Pick the top 8
    f <- e[1:8,]
    
    # Create a character vector of top 8 bowlers
    g <- as.character(f$bowler)
    
    
    # Select these top 8
    n <- NULL
    for(m in 1:8){
        mm <- filter(c,bowler==g[m])
        n <- rbind(n,mm)
    }
    
    # Summarise by the different wicket kinds for each bowler
    p <- summarise(group_by(n,bowler,wicketKind),m=n())
    
    if(plot==TRUE){
        plot.title <- paste(t1,"vs",t2,"wicket-kind of bowlers")
        # Plot
        ggplot(data=p,aes(x=wicketKind,y=m,fill=factor(wicketKind))) +
            facet_wrap( ~ bowler,scales = "fixed", ncol=8) +
            geom_bar(stat="identity") +
            xlab("Wicket kind") + ylab("Wickets") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }else{
        p
    }
    
}
