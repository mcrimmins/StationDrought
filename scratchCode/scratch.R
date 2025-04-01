# scratch code

startDate<-as.Date("2020-05-01")
start<-c(startDate)
end<-c(ymd(as.Date(startDate)) %m+% months(3))

for(l in 1:3){
  start<-c(start,startDate)
  end<-ymd(as.Date(startDate)) %m+% months(3)
}

ymd(as.Date('2020-05-01')) %m+% months(3)

begMo<-5
endMo<-7

lubridate::days_in_month(begMo)
