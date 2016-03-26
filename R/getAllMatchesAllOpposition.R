##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Mar 2016
# Function: getAllMatchesAllOpposition
# This function gets the data for all matches against all opposition for a given team.
# The user can choose to save the file for later use
# 
#
###########################################################################################
getAllMatchesAllOpposition <- function(team,dir=".",save=FALSE){

   
    # Gather team data  against all ooposition
    d1 <- paste("*",team,"*",sep="")
    path=paste(dir,"/",d1,sep="")
    fl <- Sys.glob(path)


    matches <- NULL
    for(i in 1:length(fl)){
        load(fl[i])
        matches <- rbind(matches,overs)
    }
    b <- paste("allMatchesAllOpposition-",team,".RData",sep="")
    if(save){
        save(matches,file=b)
    }
    matches
}
