# process monthly PRISM - prec into AZ stack for calculating climatologies
# MAC 03/29/21

library(prism)
library(raster)
library(tidyr)

# update with new files (adapted from ~/RProjects/PRISMDownload/monthlyDownloadPRISM.R)
# updated to new version of prism R
prism_set_dl_dir("/scratch/crimmins/PRISM/monthly/precip")
# download tmean daily data
#get_prism_monthlys(type="ppt", year = 2019:2020, mon = 1:12, keepZip=F)
# clean archive
#prism_archive_clean("ppt", "monthly")

# set rasteroptions
rasterOptions(progress = 'text')

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")

# look at file order
# get date order, make sure in right order
#names<-prism_archive_ls()
names<-ls_prism_data(name=TRUE)
  temp<-as.data.frame(substr(names$product_name, 1,9))
  temp<-separate(data = temp, col = 1, into = c("month", "year"), sep = "  ")
  temp$date<-as.Date(paste0(temp$month,"-01-",temp$year),format="%b-%d-%Y")
#dates<-as.Date(strptime(substr(names$product_name, 1,9),"%b  %Y"))
dates<-temp$date
dateOrder<-as.data.frame(sort.int(dates, index.return=TRUE))

# precip
options(prism.path = "/scratch/crimmins/PRISM/monthly/precip") 
pcpStack <- prism_stack(names$files[dateOrder$ix])

# crop to AZ
  # get boundary
  #us<-getData('GADM', country='USA', level=2)
  #az<-subset(us, NAME_1=="Arizona")
  # crop DEM grid down to CWA
  #e <- extent(az)
  # slightly adjust eastern edge of crop, -109
  e <- extent(-114.8,-109,31.3,37)
  pcpStack_az <- crop(pcpStack, e)

  writeRaster(pcpStack_az,filename="/home/crimmins/RProjects/StationDrought/AZ_monthlyPRISM_prec_1895_2020.grd", overwrite=TRUE )  
  
# check time series
#test<-t(raster::extract(pcpStack_az[[(nlayers(pcpStack_az)-360):nlayers(pcpStack_az)]], cellFromXY(pcpStack_az, c(-110.9341,32.1169))))

# extract from prism package
#point <- c(-110.9341,32.1169)
# prism_archive_subset() will return prism data that matches the specified 
# variable, time step, years, months, days, etc.
#subsetPPT<-prism_archive_subset("ppt", "monthly", years = 1991:2020)
#p <- pd_plot_slice(subsetPPT, point)

# faster(?) extract... https://www.r-bloggers.com/2015/05/extract-values-from-numerous-rasters-in-less-time/
  pcpStack_az<-stack("~/RProjects/StationDrought/AZ_monthlyPRISM_prec_1895_2020")
  
library(ff)
mat <- ff(vmode="double",dim=c(ncell(pcpStack_az),nlayers(pcpStack_az)),filename=paste0(getwd(),"/stack.ffdata"), overwrite = TRUE)
for(i in 1:nlayers(pcpStack_az)){
  mat[,i] <- pcpStack_az[[i]][]
}
save(mat,file=paste0(getwd(),"/stackData.RData"))


ID_Raster <- raster(pcpStack_az[[1]])
ID_Raster[] <- 1:ncell(pcpStack_az[[1]])

ext_ID <- raster::extract(ID_Raster,cellFromXY(pcpStack_az, c(-110.9341,32.1169)))
ext2 <- mat[as.numeric(ext_ID),]

