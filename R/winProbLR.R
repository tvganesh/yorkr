##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 25 Dec 2022
# Function: winProbabilityLR
# This function computes  the ball by ball win probability using Logistic Regression model
#
###########################################################################################
#' @title
#' Plot the  win probability using Logistic Regression model
#'
#' @description
#' This function  plots the  win probability of the teams in a T20 match
#'
#' @usage
#' winProbabilityLR(match,t1,t2,plot=1)
#'
#' @param match
#' The dataframe of the match
#'
#' @param t1
#' The 1st team of the match
#'
#' @param t2
#' the 2nd team in the match
#'
#' @param plot
#' Plot=1 (static), Plot=2(interactive)
#'
#' @return none
#'
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#'
#' @author
#' Tinniam V Ganesh
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' #Get the match details
#' a <- getMatchDetails("England","Pakistan","2006-09-05",dir="../temp")
#'
#' # Plot tne match worm plot
#' winProbabilityLR(a,'England',"Pakistan")
#' }
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#' @export
#'
winProbabilityLR <- function(match,t1,t2,plot=1){

    team=ball=totalRuns=wicketPlayerOut=ballsRemaining=runs=numWickets=runsMomentum=perfIndex=isWinner=NULL
    predict=winProbability=ggplotly=runs=runRate=batsman=bowler=NULL

    if (match$winner[1] == "NA") {
       print("Match no result ************************")
       return()
    }
    team1Size=0
    requiredRuns=0


    teamA=match$team[grep("1st.0.1",match$ball)]

    # Filter the performance of team1
    a <-filter(match,team==teamA)

    #Balls in team 1's innings
    ballsIn1stInnings= dim(a)[1]


    b <- select(a,batsman, bowler,ball,totalRuns,wicketPlayerOut,team1,team2,date)
    c <-mutate(b,ball=gsub("1st\\.","",ball))

    # Compute the total runs scored by team
    d <- mutate(c,runs=cumsum(totalRuns))
    # Check if team1 won or lost the match

    if(match$winner[1]== teamA){
      d$isWinner=1
    } else{
      d$isWinner=0
    }
    #Get the ball num
    d$ballNum = seq.int(nrow(d))

    # Compute the balls remaining for the team
    d$ballsRemaining = ballsIn1stInnings - d$ballNum +1

    # Wickets lost by team
    d$wicketNum = d$wicketPlayerOut != "nobody"
    d=d %>% mutate(numWickets=cumsum(d$wicketNum==TRUE))

    #Performance index is based on run rate (runs scored/ ball number) with wickets in hand
    d$perfIndex = (d$runs/d$ballNum) * (11 - d$numWickets)

    # Compute run rate
    d$runRate = (d$runs/d$ballNum)
    d$runsMomentum = (11 - d$numWickets)/d$ballsRemaining

    df = select(d, batsman,bowler,ballNum, ballsRemaining, runs, runRate,numWickets,runsMomentum,perfIndex, isWinner)
    print(dim(df))


    #############################################################################################
    ######## Team 2
    # Compute for Team 2

    # Required runs is the team made by team 1 + 1
    requiredRuns=d[dim(d)[1],]$runs +1
    teamB=match$team[grep("2nd.0.1",match$ball)]


    # Filter the performance of team1
    a1 <-filter(match,team==teamB)

    #Balls in team 1's innings
    ballsIn2ndInnings= dim(a1)[1] + 1


    b1 <- select(a1,batsman,bowler,ball,totalRuns,wicketPlayerOut,team1,team2,date)
    c1 <-mutate(b1,ball=gsub("2nd\\.","",ball))

    # Compute total Runs
    d1 <- mutate(c1,runs=cumsum(totalRuns))
    # Check of team2 is winner
    if(match$winner[1]== teamB){
      d1$isWinner=1
    } else{
      d1$isWinner=0
    }

    # Compute ball number
    d1$ballNum= ballsIn1stInnings + seq.int(nrow(d1))

    # Compute remaining balls in 2nd innings
    d1$ballsRemaining=  ballsIn2ndInnings -  seq.int(nrow(d1))


    # Compute wickets remaining
    d1$wicketNum = d1$wicketPlayerOut != "nobody"
    d1=d1 %>% mutate(numWickets=cumsum(d1$wicketNum==TRUE))

    ballNum=d1$ballNum - ballsIn1stInnings
    #Performance index is based on run rate (runs scored/ ball number) with wickets in hand
    d1$perfIndex = (d1$runs/ballNum) * (11 - d1$numWickets)

    #Compute required runs
    d1$requiredRuns = requiredRuns - d1$runs
    d1$runRate = (d1$requiredRuns/d1$ballsRemaining)

    d1$runsMomentum = (11 - d1$numWickets)/d1$ballsRemaining


    # Rename required runs as runs
    df1 = select(d1,batsman,bowler,ballNum,ballsRemaining, requiredRuns,runRate,numWickets,runsMomentum,perfIndex, isWinner)
    names(df1) =c("batsman","bowler","ballNum","ballsRemaining","runs","runRate","numWickets","runsMomentum","perfIndex","isWinner")
    print(dim(df1))

    df2=rbind(df,df1)
    # load the model
    #ml_model <- readRDS("glmLR.rds")

    a1=select(df,batsman,bowler,ballNum,ballsRemaining, runs,runRate,numWickets,runsMomentum,perfIndex)
    m=predict(final_lr_model,a1,type = "prob")


    m1=m$.pred_1*100
    m2=matrix(m1)

    b1=select(df1,batsman,bowler,ballNum,ballsRemaining, runs,runRate,numWickets,runsMomentum,perfIndex)
    n=predict(final_lr_model,b1,type="prob")

    n1=n1=n$.pred_1*100
    n2=matrix(n1)

    m3= 100-n2
    n3=100-m2

    team1=rbind(m2,m3)
    team2=rbind(n3,n2)

    team11=as.data.frame(cbind(df2$ballNum,team1))
    names(team11) = c("ballNum","winProbability")
    team22=as.data.frame(cbind(df2$ballNum,team2))
    names(team22) = c("ballNum","winProbability")


    # Plot both lines
    if(plot ==1){ #ggplot2
        df3 = as.data.frame(cbind(d$ballNum,m1))
        names(df3) <- c("ballNum","winProbability")
        df4 = as.data.frame(cbind(d1$ballNum,n1))
        names(df4) <- c("ballNum","winProbability")
        maxBallNum = max(df3$ballNum)
        df4$ballNum = df4$ballNum - maxBallNum
        g <- ggplot() +
            geom_line(data = df3, aes(x = ballNum, y = winProbability, color = teamA)) +
            geom_line(data = df4, aes(x = ballNum, y = winProbability, color = teamB))+
            geom_vline(xintercept=36, linetype="dashed", color = "red") +
            geom_vline(xintercept=96, linetype="dashed", color = "red") +
            ggtitle("Ball-by-ball Logistic Regression Win Probability (Overlapping)")

        ggplotly(g)

    }else { #ggplotly
      g <- ggplot() +
        geom_line(data = team11, aes(x = ballNum, y = winProbability, color = teamA)) +
        geom_line(data = team22, aes(x = ballNum, y = winProbability, color = teamB))+
        ggtitle("Ball-by-ball Logistic Regression Win Probability (Side-by-side)")


      ggplotly(g)

    }
}
