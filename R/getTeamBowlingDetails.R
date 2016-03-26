##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: getTeamBowlingDetails
# This function uses gets the bowling details of a team
#
###########################################################################################
getTeamBowlingDetails <- function(team,dir=".",save=FALSE){
    
    a <- paste(dir,"/","*",team,"*",sep="")
    # Gather team against all ooposition
    
    fl <- Sys.glob(a)
    
    bowlingDetails <- NULL
    for(i in 1:length(fl)){
        load(fl[i])
        match <- overs
        details <- teamBowlingPerfDetails(match,team,includeInfo=TRUE)
        # If the side has not batted details will be NULL. Skip in that case
        if(!is.null(dim(details))){
            bowlingDetails <- rbind(bowlingDetails,details)
        }else {
            #print("Empty")
            
            next
        }
        
    }
    
    if(save==TRUE){
        fl <- paste("./",team,"-BowlingDetails.RData",sep="")
        save(bowlingDetails,file=fl)
    }
    bowlingDetails <- arrange(bowlingDetails,bowler,date)
    bowlingDetails
    
    
}
