##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: getTeamBattingDetails
# This function gets the Batting details of a team against all opposition
#
###########################################################################################
getTeamBattingDetails <- function(team,dir=".",save=FALSE){

    a <- paste(dir,"/","*",team,"*",sep="")
    # Gather team against all ooposition
    fl <- Sys.glob(a)

    battingDetails <- NULL
    for(i in 1:length(fl)){
        load(fl[i])
        match <- overs
        details <- teamBattingPerfDetails(match,team,includeInfo=TRUE)
        # If the side has not batted details will be NULL. Skip in that case
        if(!is.null(dim(details))){
            battingDetails <- rbind(battingDetails,details)
        }else {
            #print("Empty")

            next
        }

    }

    if(save==TRUE){
        fl <- paste("./",team,"-BattingDetails.RData",sep="")
        save(battingDetails,file=fl)
    }
    battingDetails <- arrange(battingDetails,batsman,date)
    battingDetails


}
