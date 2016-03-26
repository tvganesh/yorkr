##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Mar 2016
# Function: teamBattingScorecardAllOppnAllMatches
# This function computes the performances of the teams batsman against all opposition in 
# all matches
# 
#
###########################################################################################

teamBattingScorecardAllOppnAllMatches <- function(matches,theTeam){
    
    a <-filter(matches,team==theTeam)
    b <- select(a,batsman,runs)
    names(b) <-c("batsman","runs")
    
    #Compute the number of 4s
    c <- 
        b %>%   
        mutate(fours=(runs>=4 & runs <6)) %>%
        filter(fours==TRUE)
    
    # Group by batsman. Count 4s
    d <-    summarise(group_by(c, batsman),fours=n())
    
    # Get the total runs for each batsman
    e <-summarise(group_by(a,batsman),sum(runs))
    names(b) <-c("batsman","runs")
    details <- full_join(e,d,by="batsman")
    names(details) <-c("batsman","runs","fours")
    
    f <- 
        b %>% 
        mutate(sixes=(runs ==6)) %>%
        filter(sixes == TRUE)
    # Group by batsman. COunt 6s
    g <- summarise(group_by(f, batsman),sixes=n())
    names(g) <-c("batsman","sixes")
    #Full join with 4s and 6s
    details <- full_join(details,g,by="batsman")
    
    # Count the balls played by the batsman
    ballsPlayed <- 
        a  %>% 
        select(batsman,byes,legbyes,wides,noballs,runs) %>%
        filter(wides ==0,noballs ==0,byes ==0,legbyes == 0) %>%
        select(batsman,runs)
    
    ballsPlayed<- summarise(group_by(ballsPlayed,batsman),count=n())
    names(ballsPlayed) <- c("batsman","ballsPlayed")
    details <- full_join(details,ballsPlayed,by="batsman")
    cat("Total=",sum(details$runs),"\n")
    details <- arrange(details,desc(runs),desc(sixes),desc(fours))
    details <- select(details,batsman,ballsPlayed,fours,sixes,runs)
    details
    
}