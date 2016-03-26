##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 24 Mar 2016
# Function: teamBatsmanPartnershipAllOppnAllMatchesPlot
# This function computes the batting partnerships of a team against all oppositions and
# also the partenerships of th eopposition against this team
# 
#
###########################################################################################
teamBatsmanPartnershipAllOppnAllMatchesPlot <- function(matches,theTeam,main,plot=TRUE){
    
    a <- NULL
    
    a <-filter(matches,team==theTeam)
    
    #Get partnerships
    df <- data.frame(summarise(group_by(a,batsman,nonStriker),sum(runs)))
    names(df) <- c("batsman","nonStriker","runs")
    
    # Filter all rows where runs is 0. Problem when t2="Sri Lanka"
    # Sehwag and Ganguly show up as partnerships with runs=0
    #*****Check******** when the line below is removed
    df <- filter(df,runs!=0)
    df <- arrange(df,desc(runs))
    
    if(plot==TRUE){
        if(theTeam==main){
             plot.title <- paste(theTeam," batting partnerships")
        }else if(theTeam != main){
            plot.title <- paste(theTeam," batting partnerships against ", main)
        }
        ggplot(data=df,aes(x=batsman,y=runs,fill=nonStriker))+
            geom_bar(data=df,stat="identity") +
            xlab("Batsman") + ylab("Partnership runs") +
            ggtitle(bquote(atop(.(plot.title),
                                atop(italic("Data source:http://cricsheet.org/"),"")))) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else{
        df
    }
    
    
}
