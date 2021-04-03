# create background PRISM grid for leaflet map
# MAC 03/25/21

library(raster)

temp<-raster('CheckerBoardGrid.grd')
plot(temp)

# get boundary
us<-getData('GADM', country='USA', level=2)
  az<-subset(us, NAME_1=="Arizona")
  
  
  # crop DEM grid down to CWA
  e <- extent(az)
  temp <- crop(temp, az)
  #ndfdDEMsub<-projectRasterForLeaflet(ndfdDEMsub, method = "bilinear")
  # develop mesh grid
  temp<-rasterToPolygons(temp)
  prismGrid<- spTransform(temp, CRS("+init=epsg:4326")) # WGS
  
  save(prismGrid, file = "prismGrid.RData")
  
  
  # prep the PRISM grids
  
  