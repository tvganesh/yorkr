##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 21 Mar 2016
# Function: teamBatsmenPartnershipMatch
# This function computes and displays the partnership details in a match. The output
# can either be a plot or the data frame used in the plot
#
###########################################################################################
teamBatsmenPartnershipMatch <- function(match,theTeam,plot=TRUE){
    a <-filter(match,team==theTeam)
    # Group batsman with non strikers and compute partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","runs")

    if(plot==TRUE){
        ggplot(data=df,aes(x=batsman,y=runs,fill=nonStriker))+
            geom_bar(data=df,stat="identity") +
            xlab("Batmen") + ylab("Runs Scored") +
            ggtitle(expression(atop("Batting partnerships in match",
                                    atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else{
        # Output dataframe
        df
    }


}
