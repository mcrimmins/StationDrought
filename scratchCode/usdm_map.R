
# USDM KMZ file
# download.file("https://droughtmonitor.unl.edu/data/kmz/usdm_current.kmz", destfile = "usdm_current.kmz")
# unzip("usdm_current.kmz",exdir="USDMtemp")
# infoKML<-rgdal::ogrListLayers("USDMtemp/doc.kml")
# test<-rgdal::readOGR(dsn="USDMtemp/doc.kml",layer=infoKML)


download.file("http://ndmc-001.unl.edu:8080/cgi-bin/mapserv.exe?map=/ms4w/apps/usdm/service/usdm_current_wms.map&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=usdm_current&WIDTH=1280&HEIGHT=960&crs=EPSG:4326&styles=default&format=image/png&bbox=30,-120,50,-90",
              destfile = "usdm.png")

usdmImg<-raster::raster("usdm.png")

# Give it lat/lon coords for 36-37°E, 3-2°S
extent(usdmImg) <- c(-120,-90,30,50)
# ... and assign a projection
projection(usdmImg) <- CRS("+init=epsg:4326")
plot(usdmImg)


pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                    na.color = "transparent")

leaflet() %>%  addProviderTiles(providers$CartoDB.Positron, group="basemap") %>%
  addRasterImage(usdmImg, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(r),
            title = "Surface temp")


# shapefile version...

download.file("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip",
              destfile = "usdm.zip")

unzip("usdm.zip", exdir = "USDMtemp",overwrite = TRUE)

infoSHP<-rgdal::ogrListLayers("USDMtemp")
test<-rgdal::readOGR(dsn="USDMtemp",layer=infoSHP)

leaflet() %>%  addProviderTiles(providers$CartoDB.Positron, group="basemap") %>%
  addPolygons(test)
  
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

factpal <- colorFactor("YlOrRd", test$DM)

leaflet(test) %>%  addProviderTiles(providers$CartoDB.Positron, group="basemap") %>% 
  addPolygons(data=
              fillColor = ~factpal(DM),
  weight = 1,
  opacity = 1,
  color = "black",
  dashArray = "3",
  fillOpacity = 0.7)  


  
