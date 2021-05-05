# create background PRISM grid for leaflet map
# MAC 03/25/21

library(raster)

temp<-raster('CheckerBoardGrid.grd')
#plot(temp)

# get boundary
#us<-getData('GADM', country='USA', level=2)
#  az<-subset(us, NAME_1=="Arizona")
  
  # crop DEM grid down to CWA
#  e <- extent(az)
  # slightly adjust eastern edge of crop, -109
  e <- extent(-114.8,-109,31.3,37)
  temp <- crop(temp, e)
  #ndfdDEMsub<-projectRasterForLeaflet(ndfdDEMsub, method = "bilinear")
  # develop mesh grid
  temp<-rasterToPolygons(temp)
  prismGrid<- spTransform(temp, CRS("+init=epsg:4326")) # WGS
  
  save(prismGrid, file = "prismGrid.RData")
  
  
  # leaflet map
  library(leaflet)
  
  #leafMap<-leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
  leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
    #setView(-111.839389, 33.178586, zoom = 8) %>%
    addPolygons(data=prismGrid, color = "#444444", weight = 0.5, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.1,
                highlightOptions = highlightOptions(color = "red", weight = 2,
                                                    bringToFront = FALSE),
                group = "grid")
  