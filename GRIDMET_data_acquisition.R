#GRIDMET data acquisition

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'jharr@mortonarb.org', drive=T)
path.google <- ("~/Google Drive/My Drive/")
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
#tmin and tmax

regionMean <- function(img){
  RedMn = img$select('tmmn','tmmx')$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
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
                                   selectors=c("date", "tmmn","tmmx"))
  LCMeansSave$start()
  
  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}

##################### 

gridmet_tmintmax <- ee$ImageCollection("IDAHO_EPSCOR/GRIDMET")$filterBounds(Chicago)$filterDate("2001-01-01", "2024-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  return(img$set('date',d, 'day',dy, 'month',m, 'year',y))})

tmintmaxMosaic = mosaicByDate(gridmet_tmintmax, 7)$select(c('tmmn_median','tmmx_median'),c('tmmn','tmmx'))$sort("date")
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=tmintmaxMosaic, landcover=LCTYPE, outfolder=GRIDMETsave, fileNamePrefix=paste0("tmintmax_", LCTYPE))
}

pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/data/GRIDMET_data")

ftmintmax <- dir(file.path(path.google, GRIDMETsave))

# Clunky code, but should pull the latest file
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

tmintmaxAll <- data.frame()
for(LCTYPE in lcnames){
  
  filetmintmax <- dir(file.path(path.google, GRIDMETsave), paste0("tmintmax_", LCTYPE))[length(dir(file.path(path.google, GRIDMETsave), paste0("tmintmax_", LCTYPE)))]
  
  if(!file.exists(file.path(pathShare, filetmintmax))) file.copy(from=file.path(path.google, GRIDMETsave, filetmintmax), to=file.path(pathShare, filetmintmax), overwrite=T, copy.mode=T)
  
  tmintmax <- read.csv(file.path(path.google, GRIDMETsave, filetmintmax))

  tmintmax$type <- LCTYPE
  
  tmintmaxAll <- rbind(tmintmaxAll, tmintmax)
}
summary(tmintmaxAll)

tmintmaxAll$date <- as.Date(tmintmaxAll$date)
tmintmaxAll$year <- lubridate::year(tmintmaxAll$date)
tmintmaxAll$yday <- lubridate::yday(tmintmaxAll$date)
tmintmaxAll$type <- factor(tmintmaxAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(tmintmaxAll)
summary(tmintmaxAll)
unique(tmintmaxAll$type)

write.csv(tmintmaxAll, file.path(pathShare, "GRIDMET_tmin_tmax_all.csv"), row.names=F)

##################### 
#repeat with other GRIDMET collection for SPI and SPEI
##################### 
#https://developers.google.com/earth-engine/datasets/catalog/GRIDMET_DROUGHT

# 14 day spi and spei

regionMean <- function(img){
  RedMn = img$select('spi14d','spei14d')$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
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
                                   selectors=c("date", "spi14d","spei14d"))
  LCMeansSave$start()
  
  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}



gridmet_spi_spei <- ee$ImageCollection('GRIDMET/DROUGHT')$filterBounds(Chicago)$filterDate("2001-01-01", "2024-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  return(img$set('date',d, 'day',dy, 'month',m, 'year',y))})

spi_spei14dMosaic = mosaicByDate(gridmet_spi_spei, 7)$select(c('spi14d_median','spei14d_median'),c('spi14d','spei14d'))$sort("date")
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=spi_spei14dMosaic, landcover=LCTYPE, outfolder=GRIDMETsave, fileNamePrefix=paste0("spi_spei14d_", LCTYPE))
}


spi_spei14dAll <- data.frame()
for(LCTYPE in lcnames){
  
  file14d <- dir(file.path(path.google, GRIDMETsave), paste0("spi_spei14d_", LCTYPE))[length(dir(file.path(path.google, GRIDMETsave), paste0("spi_spei14d_", LCTYPE)))]
  
  if(!file.exists(file.path(pathShare, file14d))) file.copy(from=file.path(path.google, GRIDMETsave, file14d), to=file.path(pathShare, file14d), overwrite=T, copy.mode=T)
  
  spi_spei14d <- read.csv(file.path(path.google, GRIDMETsave, file14d))
  
  spi_spei14d$type <- LCTYPE
  
  spi_spei14dAll <- rbind(spi_spei14dAll, spi_spei14d)
}
summary(spi_spei14dAll)

spi_spei14dAll$date <- as.Date(spi_spei14dAll$date)
spi_spei14dAll$year <- lubridate::year(spi_spei14dAll$date)
spi_spei14dAll$yday <- lubridate::yday(spi_spei14dAll$date)
spi_spei14dAll$type <- factor(spi_spei14dAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(spi_spei14dAll)
summary(spi_spei14dAll)
unique(spi_spei14dAll$type)

write.csv(spi_spei14dAll, file.path(pathShare, "GRIDMET_spi_spei_14d_all.csv"), row.names=F)

#####################
# 30 day spi and spei
#####################
regionMean <- function(img){
  RedMn = img$select('spi30d','spei30d')$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
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
                                   selectors=c("date", "spi30d","spei30d"))
  LCMeansSave$start()
  
  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}

spi_spei30dMosaic = mosaicByDate(gridmet_spi_spei, 7)$select(c('spi30d_median','spei30d_median'),c('spi30d','spei30d'))$sort("date")
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=spi_spei30dMosaic, landcover=LCTYPE, outfolder=GRIDMETsave, fileNamePrefix=paste0("spi_spei30d_", LCTYPE))
}


spi_spei30dAll <- data.frame()
for(LCTYPE in lcnames){
  
  file30d <- dir(file.path(path.google, GRIDMETsave), paste0("spi_spei30d_", LCTYPE))[length(dir(file.path(path.google, GRIDMETsave), paste0("spi_spei30d_", LCTYPE)))]
  
  if(!file.exists(file.path(pathShare, file30d))) file.copy(from=file.path(path.google, GRIDMETsave, file30d), to=file.path(pathShare, file30d), overwrite=T, copy.mode=T)
  
  spi_spei30d <- read.csv(file.path(path.google, GRIDMETsave, file30d))
  
  spi_spei30d$type <- LCTYPE
  
  spi_spei30dAll <- rbind(spi_spei30dAll, spi_spei30d)
}
summary(spi_spei30dAll)

spi_spei30dAll$date <- as.Date(spi_spei30dAll$date)
spi_spei30dAll$year <- lubridate::year(spi_spei30dAll$date)
spi_spei30dAll$yday <- lubridate::yday(spi_spei30dAll$date)
spi_spei30dAll$type <- factor(spi_spei30dAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(spi_spei30dAll)
summary(spi_spei30dAll)
unique(spi_spei30dAll$type)

write.csv(spi_spei30dAll, file.path(pathShare, "GRIDMET_spi_spei_30d_all.csv"), row.names=F)

#####################
# 90 day spi and spei
#####################

regionMean <- function(img){
  RedMn = img$select('spi90d','spei90d')$reduceRegion(reducer= ee$Reducer$mean(), geometry=Chicago$geometry(),
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
                                   selectors=c("date", "spi90d","spei90d"))
  LCMeansSave$start()
  
  return(print(paste0(fileNamePrefix, " processed! Check Earth Engine queue for status")))
}

spi_spei90dMosaic = mosaicByDate(gridmet_spi_spei, 7)$select(c('spi90d_median','spei90d_median'),c('spi90d','spei90d'))$sort("date")
for(LCTYPE in lcnames){
  # print(LCTYPE)
  extractByLC(imcol=spi_spei90dMosaic, landcover=LCTYPE, outfolder=GRIDMETsave, fileNamePrefix=paste0("spi_spei90d_", LCTYPE))
}


spi_spei90dAll <- data.frame()
for(LCTYPE in lcnames){
  
  file90d <- dir(file.path(path.google, GRIDMETsave), paste0("spi_spei90d_", LCTYPE))[length(dir(file.path(path.google, GRIDMETsave), paste0("spi_spei90d_", LCTYPE)))]
  
  if(!file.exists(file.path(pathShare, file90d))) file.copy(from=file.path(path.google, GRIDMETsave, file90d), to=file.path(pathShare, file90d), overwrite=T, copy.mode=T)
  
  spi_spei90d <- read.csv(file.path(path.google, GRIDMETsave, file90d))
  
  spi_spei90d$type <- LCTYPE
  
  spi_spei90dAll <- rbind(spi_spei90dAll, spi_spei90d)
}
summary(spi_spei90dAll)

spi_spei90dAll$date <- as.Date(spi_spei90dAll$date)
spi_spei90dAll$year <- lubridate::year(spi_spei90dAll$date)
spi_spei90dAll$yday <- lubridate::yday(spi_spei90dAll$date)
spi_spei90dAll$type <- factor(spi_spei90dAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(spi_spei90dAll)
summary(spi_spei90dAll)
unique(spi_spei90dAll$type)

write.csv(spi_spei90dAll, file.path(pathShare, "GRIDMET_spi_spei_90d_all.csv"), row.names=F)



