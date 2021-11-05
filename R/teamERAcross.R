matchWormGraph <- function(match,t1,t2,plot=1) {
  team=ball=totalRuns=total=NULL
  ggplotly=NULL
  
  # Filter the performance of team1
  a <-filter(matches,team==t1)
  # Power play
  a1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a2 <- select(a1,team,date,totalRuns)
  a3 <- a2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  a3$ER=a3$total/a3$count * 6
  a4 = a3 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  a4$opposition=t2
  a4$type="1-Powerplay"
  
  # Middle overs I
  b1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b2 <- select(b1,team,date,totalRuns)
  b3 <- b2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  b3$ER=b3$total/b3$count * 6
  b4 = b3 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  b4$opposition=t2
  b4$type="2-Middle overs"
  
  ##Death overs
  c1 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c2 <- select(c1,team,date,totalRuns)
  c3 <- c2 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  c3$ER=c3$total/c3$count * 6
  c4 = c3 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  c4$opposition=t2
  c4$type="3-Death overs"
  
  
  
  ####################
  # Filter the performance of team2
  a <-filter(matches,team==t2)
  # Power play
  a11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 0.1, 5.9))
  a21 <- select(a11,team,date,totalRuns)
  a31 <- a21 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  a31$ER=a31$total/a31$count * 6
  a41 = a31 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  a41$opposition=t1
  a41$type="1-PowerPlay"
  
  
  # Middle overs I
  b11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 6.1, 15.9))
  b21 <- select(b11,team,date,totalRuns)
  b31 <- b21 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  b31$ER=b31$total/b31$count * 6
  b41 = b31 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  b41$opposition=t1
  b41$type="2-Middle overs"
  
  ##Death overs
  c11 <- a %>% filter(between(as.numeric(str_extract(ball, "\\d+(\\.\\d+)?$")), 16.1, 20.0))
  c21 <- select(c11,team,date,totalRuns)
  c31 <- c21 %>% group_by(team,date) %>% summarise(total=sum(totalRuns),count=n())
  c31$ER=c31$total/c31$count * 6
  c41 = c31 %>% select(team,ER) %>% summarise(meanER=mean(ER))
  c41$opposition=t1
  c41$type="3-Death overs"
  
  m=rbind(a4,b4,c4,a41,b41,c41)
  
  ggplot(data = m,mapping=aes(x=type, y=meanER, fill=opposition)) +
    geom_bar(stat="identity", position = "dodge")
  
  
  # Plot both lines
  if(plot ==1){ #ggplot2
    ggplot() +
      geom_segment(aes(x=0,xend=20,y=0,yend=0)) +
      geom_segment(aes(x=0,xend=0,y=0,yend=36)) +
      geom_segment(aes(x=0,xend=6,y=a4ER,yend=a4ER,color="t1")) +
      geom_segment(aes(x=6,xend=12,y=b4ER,yend=b4ER,color="t1")) +
      geom_segment(aes(x=12,xend=17,y=c4ER,yend=c4ER,color="t1")) +
      geom_segment(aes(x=17,xend=20,y=d4ER,yend=d4ER,color="t1")) +
      
      geom_segment(aes(x=6,xend=6,y=a4ER,yend=b4ER,color="t1")) +
      geom_segment(aes(x=12,xend=12,y=b4ER,yend=c4ER,color="t1")) +
      geom_segment(aes(x=17,xend=17,y=c4ER,yend=d4ER,color="t1")) +
      #Team 2
      geom_segment(aes(x=0,xend=6,y=a41ER,yend=a41ER,color="t2")) +
      geom_segment(aes(x=6,xend=12,y=b41ER,yend=b41ER,color="t2")) +
      geom_segment(aes(x=12,xend=17,y=c41ER,yend=c41ER,color="t2")) +
      geom_segment(aes(x=17,xend=20,y=d41ER,yend=d41ER,color="t2")) +
      
      geom_segment(aes(x=6,xend=6,y=a41ER,yend=b41ER,color="t2")) +
      geom_segment(aes(x=12,xend=12,y=b41ER,yend=c41ER,color="t2")) +
      geom_segment(aes(x=17,xend=17,y=c41ER,yend=d41ER,color="t2")) +
      ggtitle(bquote(atop(.("Over ER performance"),
                          atop(italic("Data source:http://cricsheet.org/"),""))))
    
    
    
  }else { #ggplotly
    ggplot() +
      geom_segment(aes(x=0,xend=20,y=0,yend=0)) +
      geom_segment(aes(x=0,xend=0,y=0,yend=36)) +
      geom_segment(aes(x=0,xend=6,y=a4ER,yend=a4ER,color="t1")) +
      geom_segment(aes(x=6,xend=12,y=b4ER,yend=b4ER,color="t1")) +
      geom_segment(aes(x=12,xend=17,y=c4ER,yend=c4ER,color="t1")) +
      geom_segment(aes(x=17,xend=20,y=d4ER,yend=d4ER,color="t1")) +
      
      geom_segment(aes(x=6,xend=6,y=a4ER,yend=b4ER,color="t1")) +
      geom_segment(aes(x=12,xend=12,y=b4ER,yend=c4ER,color="t1")) +
      geom_segment(aes(x=17,xend=17,y=c4ER,yend=d4ER,color="t1")) +
      #Team 2
      geom_segment(aes(x=0,xend=6,y=a41ER,yend=a41ER,color="t2")) +
      geom_segment(aes(x=6,xend=12,y=b41ER,yend=b41ER,color="t2")) +
      geom_segment(aes(x=12,xend=17,y=c41ER,yend=c41ER,color="t2")) +
      geom_segment(aes(x=17,xend=20,y=d41ER,yend=d41ER,color="t2")) +
      
      geom_segment(aes(x=6,xend=6,y=a41ER,yend=b41ER,color="t2")) +
      geom_segment(aes(x=12,xend=12,y=b41ER,yend=c41ER,color="t2")) +
      geom_segment(aes(x=17,xend=17,y=c41ER,yend=d41ER,color="t2")) +
      ggtitle("Over performance")
    
    
    ggplotly(g)
    
  }
  
  
}
