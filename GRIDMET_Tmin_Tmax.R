#GRIDMET data acquisition

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'jharr@mortonarb.org', drive=T)
path.google <- ("~/Google Drive/My Drive/")
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
GRIDMETsave <- "GRIDMET_data"
assetHome <- ee_get_assethome()

##################### 
# Chicago geometry
##################### 

Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg") 
ee_print(Chicago)

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)

##################### 
# Read in & Format GRIDMET data
##################### 
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET#bands

years <- seq(2001,2024)
#years <- ee$List$sequence(2001,2024)


#######
#tmin
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET")$select("tmmn")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("tmin_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("tmin_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

#######
#tmax
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET")$select("tmmx")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("tmax_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("tmax_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

##################### 

# tmin <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET")$select("tmmn")$filterDate("2001-01-01", "2002-01-01")$filterBounds(Chicago)$map(function(image){
#   return(image$clip(Chicago))})
# maximumTemperatureVis <- list( 
#   min= 250.0,
#   max= 300,
#   palette =c ('#d8d8d8', '#4addff', '#5affa3', '#f2ff89', '#ff725c')
# );
# 
# Map$addLayer(tmin$first()$select("tmmn"), maximumTemperatureVis )
# datemod <- ee$List(tmin$aggregate_array("system:id"))$distinct()
# datestring <- ee$List(paste0( "d", datemod$getInfo()))
# modtmin <- ee$ImageCollection$toBands(tmin$select("tmmn"))$rename(datestring)
# bandNames <- gridmet_tmintmax$aggregate_array("system:index")
# image <- array$arrayProject(list(0))$arrayFlatten(list(bandNames))
# GDALinfo()
# 
