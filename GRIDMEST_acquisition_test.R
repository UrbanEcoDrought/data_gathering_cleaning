#GRIDMET data acquisition

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'jharr@mortonarb.org', drive=T)
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "~/Google Drive/Shared drives/Urban Ecological Drought/"
GRIDMETsave <- "GRIDMET_data"
assetHome <- ee_get_assethome()

##################### 
#helper functions
##################### 
addTime <- function(image){ 
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}

addYear = function(img) {
  d= ee$Date(ee$Number(img$get('system:time_start')));
  y= ee$Number(d$get('year'));
  return(img$set('year', y));
}

# Function for combining images with the same date
# 2nd response from here: https:#gis.stackexchange.com/questions/280156/mosaicking-image-collection-by-date-day-in-google-earth-engine 
mosaicByDate <- function(imcol, dayWindow){
  # imcol: An image collection
  # returns: An image collection
  imlist = imcol$toList(imcol$size())
  
  # Note: needed to specify the ee_utils_pyfunc since it's not an image collection
  unique_dates <- imlist$map(ee_utils_pyfunc(function(img){
    return(ee$Image(img)$date()$format("YYYY-MM-dd"))
  }))$distinct()
  
  # Same as above: what we're mappign through is a List, so need to call python
  mosaic_imlist = unique_dates$map(ee_utils_pyfunc(function(d){
    d = ee$Date(d)
    dy= d$get('day');    
    m= d$get('month');
    y= d$get('year');
    
    im = imcol$filterDate(d$advance(-dayWindow, "day"), d$advance(dayWindow, "day"))$reduce(ee$Reducer$median()) # shoudl influence the window for image aggregation
    
    return(im$set("system:time_start", d$millis(), 
                  "system:id", d$format("YYYY-MM-dd"),
                  'date', d, 'day', dy, 'month', m, 'year', y))
  }))
  
  # testOUT <- ee$ImageCollection(mosaic_imlist)
  # ee_print(testOUT)
  return (ee$ImageCollection(mosaic_imlist))
}
maskByLC <- function(imcol, MASK){
  imcol <- imcol$map(function(img){
    # Note: This is very slow, but I don't know how else to match the bands
    yrNow = ee$Number(img$get('year'))$format()$slice(0) # May be able to work around the slice, but I kept getting format issues
    yrStr = ee$String("YR")$cat(yrNow) # Need to figure out how to pull the right band
    
    maskNow = MASK$select(yrStr); # Very clunky, but it works!
    
    return(img$updateMask(maskNow))
    
  })
  return(imcol)
}
regionMean <- function(img){
  RedMn = img$select("tmmx","tmmn")$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
                                          scale=30, # hard-coded, but it's what has to happen to work
                                          maxPixels=1e13)
  return(ee$Feature(NULL, RedMn)$set('system:time_start', img$get('system:time_start'))$set('date', ee$Date(img$get('system:time_start'))$format("YYYY-MM-dd")))
}

# Function to extract things by landcover type; note: there are some not soft-coded options here, 
#   so you'll need to make sure that landcover and mask names match what Christy is using in this repo
extractByLC <- function(imcol, landcover, outfolder, fileNamePrefix, ...){
  lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")
  if(!landcover %in% lcnames){
    stop(paste("Invalid landcover type.  Must match one of the following:", paste(lcnames, collapse = ", ")))
  }
  if(landcover=="forest") lcMask = forMask
  if(landcover=="crop") lcMask = cropMask
  if(landcover=="grassland") lcMask = grassMask
  if(landcover=="urban-high") lcMask = urbHMask
  if(landcover=="urban-medium") lcMask = urbMMask
  if(landcover=="urban-low") lcMask = urbLMask
  if(landcover=="urban-open") lcMask = urbOMask
  
  ndviLCYear <- maskByLC(imcol, lcMask)
  
  # regionMean is a function defied above
  LCMeans = ee$FeatureCollection(ndviLCYear$map(regionMean))
  
  LCMeansSave <- ee_table_to_drive(collection=LCMeans,
                                   description=paste0("Save_", fileNamePrefix),
                                   folder=outfolder,
                                   fileNamePrefix=fileNamePrefix,
                                   timePrefix=T,
                                   fileFormat="CSV",
                                   selectors=c("date", "tmmx","tmmn"))
  LCMeansSave$start()
  
  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}

##################### 

##################### 
# Read in Landcover Masks ----
##################### 
Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg") 
ee_print(Chicago)

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)

# Landcover names and mask ----
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

forMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Forest')
# ee_print(forMask)
# Map$addLayer(forMask$select("YR2023"))

grassMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Grass')
# Map$addLayer(grassMask$select("YR2023"))

cropMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Crop')

urbOMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Urban-Open')

urbLMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Urban-Low')

urbMMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Urban-Medium')

urbHMask <- ee$Image('users/jharr/NLCD-Chicago_2000-2024_Urban-High')

# Map$addLayer(urbLMask$select("YR2023"))
# Map$addLayer(forMask$select("YR2023"))
##################### 

##################### 
# Read in & Format GRIDMET data
##################### 
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET#bands

gridmet <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET")$filterBounds(Chicago)$filterDate("2001-01-01", "2024-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  return(img$set('date',d, 'day',dy, 'month',m, 'year',y))})

tminmaxMosaic = mosaicByDate(gridmet, 7)$select(c('tmmn_median', 'tmmx_median'),c('tmmn','tmmx'))$sort("date")
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=tminmaxMosaic, landcover=LCTYPE, outfolder=GRIDMETsave, fileNamePrefix=paste0("tminmaxtest_", LCTYPE))
}

# $map(function(img){
#   d= ee$Date(img$get('system:time_start'));
#   dy= d$get('day');    
#   m= d$get('month');
#   y= d$get('year');
#   
#   return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
#   })$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
#   # # Add masks 
#   img <- applyLandsatBitMask(img)
#   
# 
# var maximumTemperature = dataset.select('tmmx');
# var maximumTemperatureVis = {
#   min: 290.0,
#   max: 314.0,
#   palette: ['d8d8d8', '4addff', '5affa3', 'f2ff89', 'ff725c'],
# };
# Map.setCenter(-115.356, 38.686, 5);
# Map.addLayer(maximumTemperature, maximumTemperatureVis, 'Maximum Temperature');






