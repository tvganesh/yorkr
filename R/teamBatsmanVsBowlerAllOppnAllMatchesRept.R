##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: teamBatsmanVsBowlersAllOppnAllMatchesRept
# This function computes performance of batsmen/batsman against bowlers of the opposition.
# It provides the names of the bowlers against whom the batsman scored the most.
# We can the over all performance of the team or the individual performances of the batsman
# If rank=10 then the overall performance of the team is displayed
# For a rank'n' the performance of the batsman at that rank against bowlers is displayed
###########################################################################################
teamBatsmanVsBowlersAllOppnAllMatchesRept <- function(matches,theTeam,rank=0,dispRows=50)
{
    
    a <-filter(matches,team==theTeam)
    b <-summarise(group_by(a,batsman,bowler),sum(runs))
    names(b) <- c("batsman","bowler","runs")
    
    c <- summarise(b,runsScored=sum(runs))
    d <- arrange(c,desc(runsScored))
    
    
    # If rank == 0 thne display top  batsman with best performance
    if(rank == 0){
        f <- d
    } else { 
        # display dispRows for selected batsman with rank and runs scored against opposing bowlers
        bman <- d[rank,]
        f <- filter(b,batsman==bman$batsman)
        f <- arrange(f,desc(runs))
        # Output only dispRows
        f <- f[1:dispRows,]
    }
    g <- complete.cases(f)
    h <- f[g,]
    
    
}
