overallRunsSRDeathOversPlotT20M <- function(dir=".",minMatches, dateRange) {
  team=ball=totalRuns=total=NULL
  ggplotly=NULL
  fl <- paste(dir,"/T20MDataFrame.RData",sep="")
  load(fl)
  
  df=t20MDF %>% filter(date >= dateRange[1]  & date <= dateRange[2])
  a1 <- df  %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  a2 <- select(a1,ball,totalRuns,batsman,date)
  a3 <- a2 %>% group_by(batsman) %>% summarise(runs=sum(totalRuns),count=n(), SRDeathOvers=runs/count*100) 
  
  x_lower <- quantile(a3$runs,p=0.66)
  y_lower <- quantile(a3$SRDeathOvers,p=0.66)
  
  plot.title <- paste("Top T20 batsmen of in Death overs of Intl. T20 (men)")
  if(plot == 1){ #ggplot2
    a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRDeathOvers > y_lower   ~ "Q1",
                                  runs <= x_lower & SRDeathOvers > y_lower  ~ "Q2",
                                  runs <= x_lower & SRDeathOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRDeathOvers,color=quadrant)) +
      geom_text(aes(runs,SRDeathOvers,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)
    
  } else if(plot == 2){ #ggplotly
    g <-  a3 %>%
      mutate(quadrant = case_when(runs > x_lower & SRDeathOvers > y_lower   ~ "Q1",
                                  runs <= x_lower & SRDeathOvers > y_lower  ~ "Q2",
                                  runs <= x_lower & SRDeathOvers <= y_lower ~ "Q3",
                                  TRUE ~ "Q4")) %>%
      ggplot(aes(runs,SRDeathOvers,color=quadrant)) +
      geom_text(aes(runs,SRDeathOvers,label=batsman,color=quadrant)) + geom_point() +
      geom_vline(xintercept = x_lower,linetype="dashed") +  # plot vertical line
      geom_hline(yintercept = y_lower,linetype="dashed") +  # plot horizontal line
      ggtitle(plot.title)
    
    ggplotly(g)
  }
  
}