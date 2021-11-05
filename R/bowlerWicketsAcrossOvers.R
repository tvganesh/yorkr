matchWormGraph <- function(match,t1,t2,plot=1) {
  team=ball=totalRuns=total=NULL
  ggplotly=NULL
  
  # Filter the performance of team1
  a <-filter(matches,team!=t1)
  # Power play
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,date,bowler,wicketPlayerOut)
  a3 <- a2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a4 <- a3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsPowerPlay=sum(wickets)) %>%
             arrange(desc(wicketsPowerPlay))

  
  
  # Middle overs I
  b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b2 <- select(b1,date,bowler,wicketPlayerOut)
  b3 <- b2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  b4 <- b3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsMiddleOvers=sum(wickets)) %>%
    arrange(desc(wicketsMiddleOvers))
  
  
  
  #Midle overs 2
  c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c2 <- select(c1,date,bowler,wicketPlayerOut)
  c3 <- c2 %>%  group_by(bowler,date) %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  c4 <- c3 %>% select(date,bowler,wickets) %>% distinct(date,bowler,wickets) %>% group_by(bowler) %>% summarise(wicketsDeathOvers=sum(wickets)) %>%
    arrange(desc(wicketsDeathOvers))
  
  val=min(dim(a4)[1],dim(b4)[1],dim(c4)[1])
  m=cbind(a4[1:val,],b4[1:val,],c4[1:val,])
  
  ####################
  # Filter the performance of team2
  a <-filter(match,team==t2)
  # Power play
  a11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a21 <- select(a11,team,wicketPlayerOut)
  a31 <- a21 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  a31wickets=ifelse(!is.na(a31$wickets[1]), a31$wickets[1], 0)
  a31$opposition=t1
  if(!is.na(a31$wickets[1]))
    a31$type="1-PowerPlay"
  
  b11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b21 <- select(b11,team,wicketPlayerOut)
  b31 <- b21 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  b31wickets=ifelse(!is.na(b31$wickets[1]), b31$wickets[1], 0)
  b31$opposition=t1
  if(!is.na(b31$wickets[1]))
    b31$type="2-Middle overs"
  
  #Midle overs 2
  c11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c21 <- select(c11,team,wicketPlayerOut)
  c31 <- c21 %>% filter(wicketPlayerOut != "nobody") %>% mutate(wickets =n())
  c31wickets=ifelse(!is.na(c31$wickets[1]), c31$wickets[1], 0)
  c31$opposition=t1
  if(!is.na(c31$wickets[1]))
    c31$type="3-Death overs"
  
  m=rbind(a3,b3,c3,a31,b31,c31)
  
  plot.title= paste("Wickets across 20 overs of ",t1, "and", t2, sep=" ")
  ggplot(data = m,mapping=aes(x=type, y=wickets, fill=opposition)) +
    geom_bar(stat="identity", position = "dodge") +
    ggtitle(bquote(atop(.(plot.title),
                        atop(italic("Data source:http://cricsheet.org/"),""))))
  
  # Plot both lines
  if(plot ==1){ #ggplot2
    ggplot() +
      geom_segment(aes(x=0,xend=20,y=0,yend=0)) +
      geom_segment(aes(x=0,xend=0,y=0,yend=15)) +
      geom_segment(aes(x=0,xend=6,y=a3count,yend=a3count,color="t1")) +
      geom_segment(aes(x=6,xend=12,y=b3count,yend=b3count,color="t1")) +
      geom_segment(aes(x=12,xend=17,y=c3count,yend=c3count,color="t1")) +
      geom_segment(aes(x=17,xend=20,y=d3count,yend=d3count,color="t1")) +
      
      geom_segment(aes(x=6,xend=6,y=a3count,yend=b3count,color="t1")) +
      geom_segment(aes(x=12,xend=12,y=b3count,yend=c3count,color="t1")) +
      geom_segment(aes(x=17,xend=17,y=c3count,yend=d3count,color="t1")) +
      #Team 2
      geom_segment(aes(x=0,xend=6,y=a31count,yend=a31count,color="t2")) +
      geom_segment(aes(x=6,xend=12,y=b31count,yend=b31count,color="t2")) +
      geom_segment(aes(x=12,xend=17,y=c31count,yend=c31count,color="t2")) +
      geom_segment(aes(x=17,xend=20,y=d31count,yend=d31count,color="t2")) +
      
      geom_segment(aes(x=6,xend=6,y=a31count,yend=b31count,color="t2")) +
      geom_segment(aes(x=12,xend=12,y=b31count,yend=c31count,color="t2")) +
      geom_segment(aes(x=17,xend=17,y=c31count,yend=d31count,color="t2"))+
      ggtitle(bquote(atop(.("Over performance"),
                          atop(italic("Data source:http://cricsheet.org/"),""))))
    
    
    
  }else { #ggplotly
    g <- ggplot() +
      geom_segment(aes(x=0,xend=20,y=0,yend=0)) +
      geom_segment(aes(x=0,xend=0,y=0,yend=10)) +
      geom_segment(aes(x=0,xend=6,y=a3count,yend=a3count,color="t1")) +
      geom_segment(aes(x=6,xend=12,y=b3count,yend=b3count,color="t1")) +
      geom_segment(aes(x=12,xend=17,y=c3count,yend=c3count,color="t1")) +
      geom_segment(aes(x=17,xend=20,y=d3count,yend=d3count,color="t1")) +
      
      geom_segment(aes(x=6,xend=6,y=a3count,yend=b3count,color="t1")) +
      geom_segment(aes(x=12,xend=12,y=b3count,yend=c3count,color="t1")) +
      geom_segment(aes(x=17,xend=17,y=c3count,yend=d3count,color="t1")) +
      #Team 2
      geom_segment(aes(x=0,xend=6,y=a31count,yend=a31count,color="t2")) +
      geom_segment(aes(x=6,xend=12,y=b31count,yend=b31count,color="t2")) +
      geom_segment(aes(x=12,xend=17,y=c31count,yend=c31count,color="t2")) +
      geom_segment(aes(x=17,xend=20,y=d31count,yend=d31count,color="t2")) +
      
      geom_segment(aes(x=6,xend=6,y=a31count,yend=b31count,color="t2")) +
      geom_segment(aes(x=12,xend=12,y=b31count,yend=c31count,color="t2")) +
      geom_segment(aes(x=17,xend=17,y=c31count,yend=d31count,color="t2")) +
      ggtitle("Over performance")
    
    
    ggplotly(g)
    
  }
  
  
}
