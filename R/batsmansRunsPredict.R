##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 26 Mar 2016
# Function: batsmanRunsPredict
# This function uses rpart classiication tree to predict the number of deliveries and the
# runs scored by batsman
#
###########################################################################################
batsmanRunsPredict <- function(df,name){
   
    b <- select(df,batsman,ballsPlayed,runs)
    names(b) <-c("batsman","deliveries","runs")
    m <-rpart(runs~deliveries,data=b)
    atitle <- paste(name,"- Runs vs Required number of Deliveries")
    rpart.plot(m,main=atitle)
    
}
