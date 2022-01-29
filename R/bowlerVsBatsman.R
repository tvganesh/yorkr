bowlerVsBatsman <- function(batsman1,bowler1){
    a <- matches %>% filter(batsman==batsman1 & bowler==bowler1)
    b <- select(a,batsman,bowler,runs,wides,noballs,wicketKind,wicketPlayerOut)
    c <- summarise(group_by(a,bowler),balls=n(), runsConceded=sum(runs,wides,noballs))
    d <- c %>% mutate(ER=runsConceded*6/balls) %>% select(balls,runsConceded,ER)
    e <- b %>% select(bowler,batsman,wicketKind,wicketPlayerOut)
    f <- e %>% filter(wicketPlayerOut==batsman1) %>% summarise(wicketTaken=n())
    g <-cbind(bowler1,batsman1,d,f)

    g
}
