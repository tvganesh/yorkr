##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 22 Mar 2016
# Function: teamBatsmenPartnershiOppnAllMatches
# This function computes the batting partnership of a team in all matches against 
# an opposition. The report generated can be detailed or a summary
#
###########################################################################################
teamBatsmenPartnershiOppnAllMatches <- function(matches,theTeam,report="summary"){


    a <-filter(matches,team==theTeam)
    #Get partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","partnershipRuns")
    b <- summarise(group_by(df,batsman),totalRuns=sum(partnershipRuns))
    c <- arrange(b,desc(totalRuns))
    d <- full_join(df,c,by="batsman")
    if(report == "detailed"){
        partnerships <- arrange(d,desc(totalRuns))
    } else{
        partnerships <- arrange(c,desc(totalRuns))
    }
    partnerships
}
