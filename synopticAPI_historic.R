# build historic dataset from Synoptic Labs
# MAC 05/06/2021

library(httr)
library(jsonlite)
library(tidyr)
#library(lubridate)

# get keys
source("/home/crimmins/RProjects/StationDrought/apiKey.R")

# get mesonet ID
# networks<-GET(paste0("https://api.synopticdata.com/v2/networks?token=",token))
# networksRaw <- content(networks, as = "text", encoding = "UTF-8")
# networksDF <- fromJSON(networksRaw,flatten = TRUE)
# networkInfo<-networksDF$MNET
# #thin out to key vars
# networkInfo<-networkInfo[,c("ID","LONGNAME","SHORTNAME")]
# save(networkInfo, file = "./synopticData/networkNames.RData")
# 
# ##### Build initial dataset
# # 90 day date blocks to download
# startList<-as.Date(c("2020-05-01","2020-08-01","2020-11-01","2021-02-01"),"%Y-%m-%d")
# endList  <-as.Date(c("2020-07-31","2020-10-31","2021-01-31","2021-04-30"),"%Y-%m-%d")
# 
# outList<-list()
# out<-NULL
# 
# ptm <- proc.time()
# for(i in 1:length(startList)){
#   start<-paste0(format(startList[i], "%Y%m%d"),"0000")
#   end<-paste0(format(endList[i], "%Y%m%d"),"0000")
#   out<-GET(paste0("https://api.synopticdata.com/v2/stations/precip?state=az&start=",start,"&end=",end,"&pmode=intervals&interval=day&obtimezone=local&units=english&token=",token))
#   outList[[i]] <- out
#   print(endList[i])
# }
# proc.time() - ptm
# 
# save(outList, file = paste0("./synopticData/AZDailyPrecip_",startList[1],"_",endList[length(endList)],".RData"))
##### 

# # process into full data frame
# load("~/RProjects/StationDrought/synopticData/AZDailyPrecip_2020-05-01_2021-04-30.RData")
# load("~/RProjects/StationDrought/synopticData/networkNames.RData")
# 
# # combined dataframes
# combObs<-list()
# 
# for(i in 1:length(outList)){
#   # get JSON out of data list
#   out<-outList[[i]]
#   rawOut <- content(out, as = "text", encoding = "UTF-8")
#   dfOut <- fromJSON(rawOut, flatten = TRUE)
#   # process station data
#   stations<-dfOut$STATION
#   # extract all obs
#   allObs<-unnest(stations, OBSERVATIONS.precipitation)
#   # subset inactive stations
#   allObs<-subset(allObs, STATUS=="ACTIVE")
#   # get date field
#   # allObs$last_report<- as.POSIXct(allObs$last_report, format = "%Y-%m-%dT%H:%M:%S-0700")
#   allObs$last_report<-  format(as.POSIXct(allObs$last_report, format = "%Y-%m-%dT%H:%M:%S%z"), tz="America/Phoenix",usetz=TRUE)
#   allObs$precipDate<-as.Date(allObs$last_report)
#   
#   # merge network info
#   allObs<-merge(allObs, networkInfo,by.x="MNET_ID",by.y="ID")
#   
#   # combined list
#   combObs[[i]] <- allObs
# }
# # combine into dataframe
# combObs = do.call(rbind, combObs)
# save(combObs, file = paste0("./synopticData/AZDailyPrecip_working_DataFrame.RData"))


###### UPDATE dataframe
# update data in combined data frame
load("/home/crimmins/RProjects/StationDrought/synopticData/AZDailyPrecip_working_DataFrame.RData")
load("/home/crimmins/RProjects/StationDrought/synopticData/networkNames.RData")
# get needed dates based on dataframe
start<-paste0(format(max(combObs$precipDate,na.rm=TRUE), "%Y%m%d"),"0000")
end<-paste0(format(Sys.Date(), "%Y%m%d"),"0000")
# get needed data
out<-GET(paste0("https://api.synopticdata.com/v2/stations/precip?state=az&start=",start,"&end=",end,"&pmode=intervals&interval=day&obtimezone=local&units=english&token=",token))
  # process new data
  rawOut <- content(out, as = "text", encoding = "UTF-8")
  dfOut <- fromJSON(rawOut, flatten = TRUE)
  # process station data
  stations<-dfOut$STATION
  # extract all obs
  allObs<-unnest(stations, OBSERVATIONS.precipitation)
  # subset inactive stations
  allObs<-subset(allObs, STATUS=="ACTIVE")
  # get date field example format "2021-04-29T16:56:00-0700"
  #allObs$last_report_UTC<- as.POSIXct(allObs$last_report, format = "%Y-%m-%dT%H:%M:%S%z")
  allObs$last_report<-  format(as.POSIXct(allObs$last_report, format = "%Y-%m-%dT%H:%M:%S%z"), tz="America/Phoenix",usetz=TRUE)
  allObs$precipDate<-as.Date(allObs$last_report)
  # merge network info
  allObs<-merge(allObs, networkInfo,by.x="MNET_ID",by.y="ID")
  # remove new cols -- ADDED on 6/5/2022, updated 7/20/2022
  #colnames(allObs) %in% colnames(combObs)
  #allObs<-subset(allObs, select=-c(UNITS.position,UNITS.elevation,OBSERVATIONS.precip_accum_since_local_midnight,OBSERVATIONS.precip_accum,
  #                                 OBSERVATIONS.precip_accum_fifteen_minute,OBSERVATIONS.precip_accum_one_hour))
  allObs<-subset(allObs, select=-c(which(colnames(allObs) %in% colnames(combObs)==FALSE)))
                                   
  
# combine into working dataframe
combObs<-rbind.data.frame(combObs,allObs) 

# check for duplicates
combObs<-combObs %>% dplyr::distinct()

# trim data to last 400 days to maintain df size
combObs<-subset(combObs, precipDate>=(Sys.Date()-400))

# drop HADS stations
combObs<-subset(combObs, SHORTNAME!="HADS")

# save to working datafile
save(combObs, file = "/home/crimmins/RProjects/StationDrought/synopticData/AZDailyPrecip_working_DataFrame.RData")
#### end update
rm(list = ls())




               






