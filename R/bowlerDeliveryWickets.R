##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: bowlerDeliveryWickets
# This function creates a data frame of delivery and wickets
#
###########################################################################################
bowlerDeliveryWickets <- function(match,theTeam,name){
    d <- NULL
    a <-filter(match,team!=theTeam)
    b <- filter(a,grepl(name,bowler))
    if(dim(b)[1] != 0){
        b$delivery<- seq(1:dim(b)[1])
        c <- filter(b,wicketPlayerOut != "nobody")
        if(dim(c)[1] !=0){
            c$wicketNo <- seq(1:dim(c)[1])
            d <- select(c,bowler,delivery,wicketNo,date)
        }
        
    }
    d
}