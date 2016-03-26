##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: getBatsmanDetails
# This function gets the batting details of a batsman
#
###########################################################################################
getBatsmanDetails <- function(team, name){
    
    fl <- paste("./",team,"-BattingDetails.RData",sep="")
    load(fl)
    details <- battingDetails
    batsmanDetails <- filter(details,grepl(name,batsman))
    batsmanDetails
}