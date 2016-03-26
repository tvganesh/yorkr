##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 20 Mar 2016
# Function: getMatchDetails
# This function loads the data for a specified match given the teams in the match
# and the date the match was played
#
###########################################################################################
getMatchDetails <- function(team1,team2,date,dir="."){
    overs <- NULL
    match <- NULL
    # Create 2 filenames with both combinations of team1 and team2
    d1 <- paste(team1,"-",team2,"-",date,".RData",sep="")
    d2 <- paste(team2,"-",team1,"-",date,".RData",sep="")
    path1=paste(dir,"/",d1,sep="")
    path2=paste(dir,"/",d2,sep="")
    if(file.exists(path1)){
        load(path1)
        match <- overs
    } else if(file.exists(path2)){
        load(path2)
        match <- overs
    }else {
        cat("Match file not found at",dir, "\n")
    }
    
}