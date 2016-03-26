##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Mar 2016
# Function: getDeliveryWickets
# This function creates a data frame of delivery and wickets
#
###########################################################################################
getDeliveryWickets <- function(team,dir=".",name,save=FALSE){

    a <- paste(dir,"/","*",team,"*",sep="")
    
    # Gather team against all opposition
    fl <- Sys.glob(a)
    deliveryWKts <- NULL
    for(i in 1:length(fl)){
        load(fl[i])
        match <- overs
        #print(i)
        #print(dim(match))
        details <- bowlerDeliveryWickets(match,team,name)
        # If the side has not batted details will be NULL. Skip in that case
        if(!is.null(dim(details))){
            deliveryWKts <- rbind(deliveryWKts,details)
        }else {
            #print("Empty")

            next
        }

    }
    deliveryWKts
}
