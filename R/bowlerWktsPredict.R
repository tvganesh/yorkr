##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: bowlerWktsPredict
# This function predicts the number of deliveries to wickets for a bowler using 
#classification trees
#
###########################################################################################
bowlerWktsPredict <- function(df,name){
    m <-rpart(wicketNo~delivery,data=df)
    atitle <- paste(name,"- No of deliveries to Wicket")
    rpart.plot(m,main=atitle)
    
}
