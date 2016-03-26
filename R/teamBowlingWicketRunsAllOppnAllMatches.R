##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBowlingWicketKindAllOppnAllMatches
# This function computes the wicket kind of bowlers against all  opposition
#
###########################################################################################
teamBowlingWicketRunsAllOppnAllMatches <- function(match,t1,t2="All",plot=TRUE){


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

    # Calculate runs
    e <- summarise(group_by(b,bowler,over),sum(runs,wides,noballs))
    names(e) <- c("bowler","over","runs")


    #Compute total runs conceded (runs_wides+noballs)
    f <- summarize(group_by(e,bowler),runsConceded=sum(runs))

    # Join the runs conceded with the wickets taken
    g <- full_join(f,d,by="bowler")

    # Set the NAs (0 wickets) to 0
    if(sum(is.na(g$wickets)) != 0){
        g[is.na(g$wickets),]$wickets=0
    }

    # Pick the top 10 bowlers
    h <- arrange(g,desc(wickets))
    k <- h[1:10,]

    if(plot==TRUE){
        plot.title <- paste(t1,"vs",t2,"wicket Runs of bowlers")
        ggplot(data=k,aes(x=factor(wickets),y=runsConceded,fill=factor(wickets))) +
            facet_grid( ~ bowler) + geom_bar(stat="identity") +
            xlab("Number of wickets") + ylab('Runs conceded') +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }else{
        k
    }

}
