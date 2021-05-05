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
library(leafem)

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

# USDM data
download.file("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip",
              destfile = "usdm.zip")
  unzip("usdm.zip", exdir = "USDMtemp",overwrite = TRUE)
  infoSHP<-rgdal::ogrListLayers("USDMtemp")
  usdmSHP<-rgdal::readOGR(dsn="USDMtemp",layer=infoSHP)
  # clip to smaller region
  usdmSHP <- crop(usdmSHP, extent(-118.3,-105.3, 30.5, 39))
  # color ramp
  USDMpal <- colorFactor("YlOrRd", c(0,1,2,3,4))
  # get USDM date
  temp<-substring(infoSHP, c(6,10,12), c(9,11,13))
  usdmLab<-paste0(temp[2],"-",temp[3],"-",temp[1])

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
currDate<-Sys.Date()
# or test date
#currDate<-as.Date("2020-07-15",format="%Y-%m-%d")
dateRangeStart=format(currDate-367, "%Y-%m-%d")
dateRangeEnd=format(currDate-2, "%Y-%m-%d")
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
sumACIS$network<-"NOAA-GHCN"
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

#monthFirst<-1
#monthLast<-5

# if(monthFirst>=monthLast){
#   mos<-c(monthFirst, which((monthLast>seq(1,12,1))==FALSE))
# }else{
#   mos<-seq(monthFirst,monthLast,1)
# }

# sequence of months
mos<-unique(as.numeric(format(seq(perBeg,perEnd, by="day"),"%m")))

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
sumPeriod$spi.sci<-NA
sumPeriod$perRank<-NA

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
  
  # SCI package SPI estimation
  spi.para <- SCI::fitSCI(expVec,first.mon=1,distr="gamma",time.scale=1,p0=TRUE)
  spi.sci <- SCI::transformSCI(expVec,first.mon=1,obj=spi.para,sci.limit=3)
  spi.sci<-na.omit(spi.sci)
  sumPeriod$spi.sci[i]<-spi.sci[length(spi.sci)]
  
  # percent rank
  sumPeriod$perRank[i]<-round(last(trunc(rank(pptTemp))/length(pptTemp))*100,0)
  
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

# alt-format station labs
labs <- lapply(seq(nrow(sumPeriod)), function(i) {
  paste0( '<b> Observed Precip (in): ', round(sumPeriod[i, "sumPrecip"],2), '</b><br>', 
          'PRISM Avg (in): ', round(sumPeriod[i, "avgPRISM"],2), '<br>',
          'Diff from Avg (in): ', round(sumPeriod[i, "diffAvg"],2), '<br>',
          '<b> <font color="red"> SPI: ', round(sumPeriod[i, "spi.sci"],2), '</font></b><br>',
          '%  rank: ', sumPeriod[i,"perRank"], '<br>',
          'Max 1-day Precip: ', sumPeriod[i,"maxPrecip"], '<br>',
          'Days missing: ', sumPeriod[i,"daysMiss"], '<br>',
          'Gauge: ', sumPeriod[i,"gaugeID"], '<br>',
          'Network: ', sumPeriod[i,"network"]) 
})

pal <- colorNumeric(
  palette = colorRampPalette(c('chocolate4','snow3','green'))(length(sumPeriod$spi)), 
  domain = c(-3,3))
mapTitle<-paste0(periodName," SPI:<br>", format(perBeg, "%b-%d-%y"),"<br>to ",
       format(perEnd, "%b-%d-%y"))

# network groups
networks = as.character(unique(sumPeriod$network))

#leafMap<-leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
leafMap<-leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group="basemap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group="satellite") %>%
  setView(-111.87672536355456, 34.1, zoom = 7) %>%  
  addPolygons(data=prismGrid, color = "#444444", weight = 0.5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.1,
              highlightOptions = highlightOptions(color = "red", weight = 2,
                                                  bringToFront = FALSE),
              group = "PRISM grid") %>%
  addPolygons(data=usdmSHP,
              fillColor = ~USDMpal(DM),
              weight = 1,
              opacity = 1,
              color = "black",
              dashArray = "3",
              fillOpacity = 0.2, label = paste0("D",usdmSHP$DM,": ",usdmLab), group = "USDM")
 # addWMSTiles(
 #    baseUrl = "http://ndmc-001.unl.edu:8080/cgi-bin/mapserv.exe?map=/ms4w/apps/usdm/service/usdm_current_wms.map&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=usdm_current&WIDTH=640&HEIGHT=480&crs=EPSG:3857&styles=default&format=image/png&bbox=-18367715.9809,1689200.13961,-6679169.4476,15538711.0963",
 #    layers = "usdm_current",
 #    options = WMSTileOptions(format = "image/png", transparent = T, version ="1.3.0", opacity=0.5, group="USDM", attribution="US Drought Monitor")
 #  )
  
for(n in networks){
  subNet<-sumPeriod[sumPeriod$network==n,]
  leafMap<- leafMap %>% addCircleMarkers(data=subNet, lng=~lon, lat=~lat,
                   radius = 6,
                   color = "black",
                   fillColor =  pal(subNet$spi.sci),
                   stroke = TRUE,
                   weight = 1,
                   fillOpacity = 0.9,
                   label = lapply(labs[which(sumPeriod$network==n)], htmltools::HTML),
                   group = n)
}
  
  
leafMap<-leafMap %>%
  #setView(-111.839389, 33.178586, zoom = 8) %>%
  addLegend("bottomright", pal = pal, values = c(-3,3),
            title = mapTitle,
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("basemap","satellite"),
    overlayGroups = c("PRISM grid", "RAINLOG", "NOAA-GHCN", "USDM"),
    options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(c("PRISM grid")) %>%
  addLogo("https://cals.arizona.edu/climate/misc/SPImaps/UA_CSAP_CLIMAS_logos_horiz.png",
          position = "bottomleft",
          offset.x = 20,
          offset.y = 20,
          width = 200,
          height = 36,
          src = "remote")

# save map
saveWidget(leafMap, file=paste0("/home/crimmins/RProjects/StationDrought/maps/MultiNetwork_",periodName,"_SPI.html"), selfcontained = FALSE)
##### END LEAFLET MAP
print(periodName)
}

# create Website with markdown ----
library(rmarkdown)
library(knitr)

render('/home/crimmins/RProjects/StationDrought/maps/AZdrought.Rmd', output_file='index.html',
       output_dir='/home/crimmins/RProjects/StationDrought/maps', clean=TRUE)

