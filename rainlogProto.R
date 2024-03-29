# station-based drought monitor for Rainlog data
# adapted from obsForBen.R
# get Rainlog data for list of days
# MAC 03/24/21
# added multinetwork stations
# 4/30/21

#library(plyr)
library(RCurl)
library(jsonlite)
#library(spacetime)
library(ggplot2)
library(ggmap)
library(scales)
library(tidyverse)
library(SPEI)
library(htmlwidgets)
library(raster)
library(ff)
library(leaflet)

# load data
load("prismGrid.RData")

# functions
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format="%d")))
}

##### USE FF TO CREATE PRISM MATRIX FOR CLIMOS
pcpStack_az<-stack("~/RProjects/StationDrought/AZ_monthlyPRISM_prec_1895_2020")
# write matrix to disk
mat <- ff(vmode="double",dim=c(ncell(pcpStack_az),nlayers(pcpStack_az)),filename=paste0(getwd(),"/stack.ffdata"), overwrite = TRUE)
for(i in 1:nlayers(pcpStack_az)){
  mat[,i] <- pcpStack_az[[i]][]
}
save(mat,file=paste0(getwd(),"/data.RData"))
# end write

# set date ranges
#dateRangeStart<-"2021-03-22"
#dateRangeEnd<-"2021-03-22"

# 365-day SPI
dateRangeStart=format(Sys.Date()-367, "%Y-%m-%d")
dateRangeEnd=format(Sys.Date()-2, "%Y-%m-%d")
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# 90-day SPI
#dateRangeStart=format(Sys.Date()-92, "%Y-%m-%d")
#dateRangeEnd=format(Sys.Date()-2, "%Y-%m-%d")
#allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# 180-day SPI
#dateRangeStart=format(Sys.Date()-182, "%Y-%m-%d")
#dateRangeEnd=format(Sys.Date()-2, "%Y-%m-%d")
#allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)


##### DOWNLOAD DATA - Rainlog, RCC-ACIS, FCDs from monsoon API
# specify center and radius for search area
# AZ extent
#xmin       : -114.8154 
#xmax       : -109.0449 
#ymin       : 31.32917 
#ymax       : 37.00459 

##### NOAA RCC-ACIS DOWNLOAD
###### GET ACIS DATA --- GET SNOW, too
# ACIS query
# RCC ACIS bbox - Bounding box specified in decimal degrees (W,S,E,N) (e.g. [-90, 40, -88, 41])
#ACISbbox<-paste0(min(TucsonMap$data$lon),',',min(TucsonMap$data$lat),',',max(TucsonMap$data$lon),',',max(TucsonMap$data$lat))
ACISbbox<-paste0(-114.8154,',',31.32917,',',-109.0449,',',37.00459)

jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","elems":"pcpn","meta":"name,ll"}') # or uid
out<-postForm("http://data.rcc-acis.org/MultiStnData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
outACIS<-fromJSON(out)


# melt data
#meltData<-reshape::melt(summary, id=1:3)
#meltData$value<-as.numeric(as.character(meltData$value))
# change to common dataframe format
#colnames(meltData)<-c("position.lng","position.lat","gaugeId","readingDate","rainAmount")
#meltData$readingDate<-as.Date(meltData$readingDate)-1 # adjusting to match Rainlog

##### RAINLOG DATA DOWNLOAD
# while statement to loop through pages
limit<-1000
i<-0
done<-0

# using geographic center of US 39.828165, -98.579480
while (done==0) {
  #jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0}}')
  jsonQuery=paste0('{"quality":["Good"],"pagination":{"offset":',i,',"limit":',limit,'},"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region": {"type": "Rectangle","westLng": -114.8154,"eastLng": -109.0449,"northLat": 31.32917,"southLat": 37.00459}}')
  
  out<-postForm("https://rainlog.org/api/1.0/Reading/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE){
    if (i==0){
      dataStack<-out
    }else{
      dataStack<-rbind(dataStack, out)
    }
    if (out$readingDate[length(out$readingDate)]==dateRangeEnd){
      done<-1
      break
    }else{}
    
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# get gauges
# while statement to loop through gauges
limit<-1000
i<-0
done<-0

while (done==0) {
  #jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region":{"type":"Circle","center":{"lat":32.221551,"lng":-110.909479},"radius":25.0},"pagination":{"offset":',i,',"limit":',limit,'}}')
  jsonQuery=paste0('{"dateRangeStart":"',dateRangeStart,'","dateRangeEnd":"',dateRangeEnd,'","region": {"type": "Rectangle","westLng": -114.8154,"eastLng": -109.0449,"northLat": 31.32917,"southLat": 37.00459},"pagination":{"offset":',i,',"limit":',limit,'}}')
  out<-postForm("https://rainlog.org/api/1.0/GaugeRevision/getFiltered", 
                .opts = list(postfields = jsonQuery, 
                             httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
  out<-fromJSON(out)
  if(exists("out")==TRUE & is.data.frame(out)==TRUE){
    if (i==0){
      gaugeStack<-jsonlite::flatten(out, recursive=FALSE)
    }else{
      gaugeStack<-rbind(gaugeStack, jsonlite::flatten(out))
    }
  }else{
    break
    done<-1
  }
  
  i <-i+limit
  rm(out)
  print(i)
}

# # reverse geocode
# library(revgeo)
# test<-gaugeStack[1:20,]
# location<-revgeo(test$position.lng, test$position.lat, output = 'frame')

# join data frames
mergedData <- merge(dataStack,gaugeStack,by="gaugeId")
mergedData$readingDate <-as.Date(mergedData$readingDate, format="%Y-%m-%d")
#mergedData <- merge(dataStack,gaugeStack,by="gaugeRevisionId", all.x = TRUE)

# fix dates - raw table has observation date, rain most likely fell on previous day (Rainlog site subtracts one day)
# don't subtract if you want it to match the PRISM reporting
# mergedData$readingDate<-as.Date(mergedData$readingDate)-1
##### END DOWNLOAD DATA

# set time period, less 365 or less
# 30, 90, 180, 365
# loop through all time periods

period<-c(29,89,179,364)
periodNameList<-c("30-day","90-day","180-day","365-day")
perEnd<-allDates[length(allDates)]  

for(k in 1:length(period)){
  perBeg<-perEnd-period[k]
  periodName<-periodNameList[k]
  # subset ACIS
  # format into dataframe
  ll<-data.frame(matrix(unlist(outACIS$data$meta$ll), nrow=length(outACIS$data$meta$ll), byrow=T))
  meta<-outACIS$data$meta
  # get summary formatted
  summary<- data.frame(matrix(unlist(outACIS$data$data), nrow=nrow(outACIS$data), byrow=T))
  colnames(summary)<-allDates
  # trim to period range
  summary<-summary[,which(colnames(summary)==perBeg):ncol(summary)]
  
  # replace Traces to 0
  summary[summary == "T"] <- "0.00"
  
  # convert obs to numeric
  summary[1:ncol(summary)] <- sapply(summary[1:ncol(summary)],as.character)
  summary[1:ncol(summary)] <- sapply(summary[1:ncol(summary)],as.numeric)
  # add metadata
  summary<-cbind(ll,meta$name,rowSums(summary, na.rm = TRUE), apply(summary, 1, function (x) sum(is.na(x))),
                 apply(summary, 1, function (x) sum(is.na(x))/ncol(summary)), apply(summary, 1, function (x) max(x, na.rm = TRUE)))
  # colnames
  colnames(summary)<-c("lon","lat","gaugeID","sumPrecip","daysMiss","percMiss", "maxPrecip")
  sumACIS<-summary
  # subset Rainlog
  subMergedData<-mergedData[mergedData$readingDate>=perBeg & mergedData$readingDate<=perEnd,]
  subDates<-seq.Date(perBeg,perEnd,by=1)
  
##### SUMMARIZE DATA
# develop summed period dataframe
sumPeriod<- subMergedData %>%
              group_by(gaugeId) %>% #gaugeId.x 
              summarize(countObs = n(),
                        gaugeID= first(gaugeId), # gaugeRevisionId
                        lat = min(position.lat),
                        lon = min(position.lng),
                        maxPrecip = max(rainAmount, na.rm = TRUE),
                        snowAccum = sum(snowAccumulation, na.rm = TRUE),
                        sumPrecip = sum(rainAmount, na.rm = TRUE)
                    )

# delete duplicates in lat/lon locations?

# calculate percent missing
sumPeriod$percMiss<-(1-(sumPeriod$countObs/length(subDates)))
sumPeriod$daysMiss<-(length(subDates)-sumPeriod$countObs)

# Combine networks into dataframe
sumACIS$network<-"NOAA"
sumPeriod$network<-"RAINLOG"
sumPeriod<-rbind.data.frame(sumPeriod[,c(5,4,3,8,10,9,6,11)],sumACIS)

# subset less than 10% missing
sumPeriod<-subset(sumPeriod, percMiss>=0 & percMiss<=0.07)
##### END SUMMARIZE DATA

##### DEVELOP CLIMATOLOGIES USING LOCAL MONTHLY PRISM 1895-2020
# get climo from monthly PRISM using ff
# https://www.r-bloggers.com/2015/05/extract-values-from-numerous-rasters-in-less-time/
#library(raster)
#library(ff)
#load("~/RProjects/StationDrought/stackData.RData") # developed with StationDrought/processMonthlyPRISM.R
# pcpStack_az<-stack("~/RProjects/StationDrought/AZ_monthlyPRISM_prec_1895_2020")
# 
# # write matrix to disk
# mat <- ff(vmode="double",dim=c(ncell(pcpStack_az),nlayers(pcpStack_az)),filename=paste0(getwd(),"/stack.ffdata"))
# for(i in 1:nlayers(pcpStack_az)){
#   mat[,i] <- pcpStack_az[[i]][]
# }
# save(mat,file=paste0(getwd(),"/data.RData"))
# # end write

ID_Raster <- raster(pcpStack_az[[1]])
ID_Raster[] <- 1:ncell(pcpStack_az[[1]])

monthFirst<-as.numeric(format(perBeg, "%m"))
monthLast<-as.numeric(format(perEnd, "%m"))
dayFirst<-as.numeric(format(perBeg, "%d"))
dayLast<-as.numeric(format(perEnd, "%d"))

# monthFirst<-as.numeric(format(as.Date(dateRangeStart,"%Y-%m-%d"), "%m"))
# monthLast<-as.numeric(format(as.Date(dateRangeEnd,"%Y-%m-%d"), "%m"))
# dayFirst<-as.numeric(format(as.Date(dateRangeStart,"%Y-%m-%d"), "%d"))
# dayLast<-as.numeric(format(as.Date(dateRangeEnd,"%Y-%m-%d"), "%d"))

#monthFirst<-12
#monthLast<-3

if(monthFirst>monthLast){
  mos<-c(monthFirst, which((monthLast<seq(1,12,1))==FALSE))
}else{
  mos<-seq(monthFirst,monthLast,1)
}

# get days in month
f1<-function(x){numberOfDays(as.Date(paste0(x,"-01-2000"),"%m-%d-%Y"))}
daysMo<-sapply(mos, f1)
daysMoAdj<-daysMo
  daysMoAdj[1]<-daysMoAdj[1]-as.numeric(format(perBeg,"%d"))
  daysMoAdj[length(daysMoAdj)]<-as.numeric(format(perEnd,"%d"))
adjFactor<-daysMoAdj/daysMo
adjFactor<-cbind.data.frame(mos,adjFactor)
# weighting matrix
wgts<-cbind.data.frame(seq(1,12,1),rep(0,12))
  colnames(wgts)<-c("mos","adjFactor")
wgts<-merge(wgts,adjFactor,by="mos", all.x=TRUE)
  wgts[is.na(wgts)] <- 0
  wgts<-wgts[,c(1,3)]
# or sums
# adjFactor<-sum(daysMoAdj)/sum(daysMo)    

# create dates
datesMo<-as.data.frame(seq.Date(as.Date("1895-01-01","%Y-%m-%d"),as.Date("2020-12-01","%Y-%m-%d"),by="month"))
  colnames(datesMo)<-"date"
  datesMo$month<-as.numeric(format(datesMo$date,"%m"))

# add in columns for sumPeriod df
sumPeriod$avgPRISM<-NA
sumPeriod$diffAvg<-NA
sumPeriod$spi<-NA  

## FIX GRID TO GO PAST AZ BOUNDARY
for(i in 1:nrow(sumPeriod)){
  ext_ID <- raster::extract(ID_Raster,cellFromXY(pcpStack_az, c(sumPeriod$lon[i],sumPeriod$lat[i])))
  ppt <- mat[as.numeric(ext_ID),]
    ppt<-ppt/25.4
  ppt<-cbind.data.frame(datesMo,ppt)
  ppt<-merge(ppt,wgts, by.x="month", by.y="mos", all.x=TRUE) 
  ppt$adjPPT<-ppt$ppt*ppt$adjFactor.y
  ppt <- ppt[order(as.Date(ppt$date,"%Y-%m-%d")),]
  ppt$sum<- zoo::rollapply(ppt$adjPPT, length(mos), sum, fill=NA,align="right")
  # subset rollsum to last month
  ppt<-subset(ppt,month==last(mos))
  # prism avg
  sumPeriod$avgPRISM[i]<-mean(ppt$sum, na.rm=TRUE)
  # calculate spi
  pptTemp<-ppt$sum
    pptTemp<-c(pptTemp,sumPeriod$sumPrecip[i])
  expVec<-NULL
   for(j in 1:length(pptTemp)){
     temp<-c(pptTemp[j], rep(0,11))
     expVec<-c(expVec,temp)
   }
  spiFitted<-spi(expVec, 1, na.rm = TRUE)
  spiFitted<-as.data.frame(spiFitted$fitted)
  spiFitted<-na.omit(spiFitted)
  sumPeriod$spi[i]<-spiFitted[nrow(spiFitted),1]
}

sumPeriod$diffAvg<-sumPeriod$sumPrecip-sumPeriod$avgPRISM
##### END CLIMATOLOGIES

##### DEVELOP CLIMATOLOGIES USING DAILY PRISM RCC-ACIS -- TOO SLOW
# build get PRISM data - VERY SLOW ----
# # try both daily and scaled monthly
# prismPPT<-NULL
# yrs<-seq(1982,2020,1)
# prismStack<-list()
# 
# ptm <- proc.time()
#   for(i in 1:nrow(sumPeriod)){
#     for(k in 1:length(yrs)){
#       edate<-paste0(yrs[k],"-",format(as.Date(dateRangeEnd, "%Y-%m-%d"), "%m-%d"))
#       sdate<-as.Date(edate, "%Y-%m-%d")-90
#       jsonQuery<-paste0('{"loc":"',sumPeriod$lon[i],',',sumPeriod$lat[i],'","sdate":"',sdate,'","edate":"',edate,'","grid":"21","elems":[{"name":"pcpn","interval":"dly","smry":"sum","smry_only":"1"}]}')
#       out<-postForm("http://data.rcc-acis.org/GridData", 
#                     .opts = list(postfields = jsonQuery, 
#                                  httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
#       out<-fromJSON(out)
#       prismPPT[k]<-out$smry
#     }
#     prismStack[[i]] <- prismPPT
#     print(sumPeriod$gaugeId.x[i])  
#   }
# proc.time() - ptm
# 
# prismTEMP <- as.data.frame(do.call(rbind, prismStack))
# sumPeriod$avgPRISM<-rowMeans(prismTEMP, na.rm = TRUE, dims = 1)
# sumPeriod$diffAvg<-sumPeriod$sumPrecip-sumPeriod$avgPRISM
# 
# prismTEMP$curr <-  sumPeriod$sumPrecip
# # calculate SPI without monthly values
# # https://www.r-bloggers.com/2010/03/visualizing-drought/
# #  fit.cdf <- ecdf(prismTEMP[1,])
# #  cdfs <- sapply(prismTEMP[1,],fit.cdf)
# #  SPI <- qnorm(cdfs)
# 
# # expand dataframe to pad with emptyrows
# zeroPad<-as.data.frame(matrix(0L, nrow = nrow(prismTEMP), ncol = 11))
# expStack<-list()
# for(i in 1:ncol(prismTEMP)){
#   expStack[[i]]<-cbind(prismTEMP[,i], zeroPad)
# }
# expStack<- as.data.frame(do.call(cbind, expStack))
# # transpose
# expStack<-as.data.frame(t(expStack))
# 
# library(SPEI)
# spiFitted<-spi(expStack, 1)
# spiFitted<-as.data.frame(spiFitted$fitted)
# spiFitted<-na.omit(spiFitted)
# 
# # add back into sumPeriod df
# sumPeriod$spi<-t(spiFitted[nrow(spiFitted),])
# # -----
##### DAILY PRISM CLIMATOLOGIES


##### DEVELOP LEAFLET MAP
# plot on map
#library(leaflet)

#load("prismGrid.RData")

# station labs
labs <- lapply(seq(nrow(sumPeriod)), function(i) {
  paste0( '<p> <b> Observed Precip (in): ', round(sumPeriod[i, "sumPrecip"],2), '</b></p>', 
          '<p> PRISM Avg (in): ', round(sumPeriod[i, "avgPRISM"],2), '</p>',
          '<p> Diff from Avg (in): ', round(sumPeriod[i, "diffAvg"],2), '</p>',
          '<p> <font color="red"> SPI: ', round(sumPeriod[i, "spi"],2), '</font></p>',
          '<p> Days missing: ', sumPeriod[i,"daysMiss"], '</p>',
          '<p> Network: ', sumPeriod[i,"network"], '</p>') 
})

pal <- colorNumeric(
  palette = colorRampPalette(c('chocolate4','snow3','green'))(length(sumPeriod$spi)), 
  domain = c(-3,3))
mapTitle<-paste0(periodName," SPI: ", format(perBeg, "%b-%d-%y"),"<br>to",
       format(perEnd, "%b-%d-%y"))

#leafMap<-leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
leafMap<-leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  #setView(-111.839389, 33.178586, zoom = 8) %>%
  addPolygons(data=prismGrid, color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.1,
              highlightOptions = highlightOptions(color = "red", weight = 2,
                                                  bringToFront = FALSE),
             group = "grid") %>%
  addCircleMarkers(sumPeriod$lon, sumPeriod$lat,
                   radius = 7,
                   color = "grey66",
                   fillColor =  pal(sumPeriod$spi),
                   fillOpacity = 1,
                   label = lapply(labs, htmltools::HTML),
                   group = "points") %>%
  addLegend("bottomright", pal = pal, values = c(-3,3),
            title = mapTitle,
            opacity = 1) %>%
  addLayersControl(
    overlayGroups = c("grid", "points"),
    options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup("grid")

# save map
saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/StationDrought/maps/MultiNetwork_",periodName,"_SPI.html"), selfcontained = FALSE)
##### END LEAFLET MAP

print(periodName)

}
