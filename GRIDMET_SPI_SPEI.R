#GRIDMET data acquisition spi + spei

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
# Read in & Format GRIDMET data (spi & spei)
##################### 
#https://developers.google.com/earth-engine/datasets/catalog/GRIDMET_DROUGHT#bands

#NOTE: these variables have a 5 day cadence

years <- seq(2001,2024)

#######
#spi14d
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("GRIDMET/DROUGHT")$select("spi14d")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("spi14d_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("spi14d_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

#######
#spi30d
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("GRIDMET/DROUGHT")$select("spi30d")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("spi30d_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("spi30d_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

#######
#spi90d
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("GRIDMET/DROUGHT")$select("spi90d")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("spi90d_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("spi90d_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

#######
#spei14d
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("GRIDMET/DROUGHT")$select("spei14d")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("spei14d_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("spei14d_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

#######
#spei30d
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("GRIDMET/DROUGHT")$select("spei30d")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("spei30d_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("spei30d_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

#######
#spei90d
#######

for (yr in years){
  
  imgyr <- ee$ImageCollection("GRIDMET/DROUGHT")$select("spei90d")$filterDate(ee$Date$fromYMD(yr,1,1), ee$Date$fromYMD((yr+1),1,1))$filterBounds(Chicago)$map(function(image){
    return(image$clip(Chicago))})
  
  img_flat <- imgyr$toBands()
  export_tmin <- ee_image_to_drive(image=img_flat, description=paste0("spei90d_", yr), region=Chicago$geometry(), fileNamePrefix=paste0("spei90d_", yr), folder=GRIDMETsave, timePrefix=F)
  export_tmin$start()
  
}

##################### 