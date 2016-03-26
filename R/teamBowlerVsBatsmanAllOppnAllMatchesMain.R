##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBowlersVsBatsmanAllOppnAllMatchesMain
# This function computes the performance of bowlers of team against all opposition in all matches
# This function returns a dataframe
#
###########################################################################################
teamBowlersVsBatsmanAllOppnAllMatchesMain <- function(matches,theTeam,rank=0) {


    a <-filter(matches,team !=theTeam)

    b <-summarise(group_by(a,bowler,batsman),sum(runs))
    names(b) <- c("bowler","batsman","runsConceded")
    # Compute total runs conceded
    c <- summarise(group_by(b,bowler),runs=sum(runsConceded))
    # Sort by descneding
    d <- arrange(c,desc(runs))


    # Initialise to NULL
    f <- NULL
    if(rank == 0){
        f <- head(d,10)
    } else { # display dispRows for selected bowler with rank
       # Pick the chosen bowler
        bwlr <- d[rank,]

        f <- filter(b,bowler==bwlr$bowler)
        f <- arrange(f,desc(runsConceded))
    }
    f


}
