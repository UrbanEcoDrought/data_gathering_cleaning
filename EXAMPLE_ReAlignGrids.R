# Example script to align grids on GEE

# saving spatial NDVI data for machine learning exercises

library(rgee); library(raster); library(terra); library(sf); library(sp); library(stars)
ee_check() # For some reason, it's important to run this before initializing right now
# rgee::ee_Initialize(user = 'malexander@anl.gov', drive=T, project="nbs2023-malexander")
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T, gcs=T)

# path.google <- "~/Google Drive/Shared drives/Urban Ecological Drought/data/data_sets/example_met_geotiff/"
# path.google.share <- "G:/Shared drives/Urban Ecological Drought/data/data_sets/landsat_spatial/"
# NDVIsave <- "UrbanEcoDrought_NDVI_LocalExtract"
# GoogleFolderSave <- "UHI_Analysis_Output_Final_v2"
assetHome <- ee_get_assethome()

GoogleFolderSave <- "landsat_data-TEST"
##################### 
# 0. Read in helper functions ----
##################### 
source("Workflow_NDVI_Landsat_by_Landcover/00_EarthEngine_HelperFunctions.R")

Map$setCenter(-88.04526, 41.81513, 11);
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)
ndviVis = list(
  min= 0.0,
  max= 1,
  palette= c(
    '#FFFFFF', '#CE7E45', '#DF923D', '#F1B555', '#FCD163', '#99B718', '#74A901',
    '#66A000', '#529400', '#3E8601', '#207401', '#056201', '#004C00', '#023B01',
    '#012E01', '#011D01', '#011301'
  )
)

Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg") 
ee_print(Chicago)

chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)


##################### 


# It'll be easiest if we upload the sample raster to earth engine and work with it there
# I tried using raster_as_ee, but it requires Google Cloud Storage to work, which doesn't work for me
# So I just manually uploaded it
origGT <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/data/data_sets/example_met_geotiff/Sample_Tmax30day.tif")
origGT
plot(origGT)

sampleGT <- ee$Image(file.path(assetHome, "Sample_Tmax30day"))
ee_print(sampleGT)
Map$addLayer(sampleGT)
projExample = sampleGT$projection()
projCRS = projExample$crs()
projTransform <- unlist(projExample$getInfo()$transform)

sampleGT$projection()$getInfo()

sampleExtent <- sampleGT$geometry()
coordExt <- sampleExtent$coordinates
coordExt$getInfo()
sampleGT$geometry()$getInfo()


projExample$getInfo()
projCRS$getInfo()
projTransform

##################### 
# Read in & Format Landsat 9 ----
##################### 
# Function to extract the date from each image
get_date <- function(image) {
  date <- ee$Date(image$get("system:time_start"))$format("YYYY-MM-dd")
  ee$Feature(NULL, list(date = date))
}


# "LANDSAT/LC09/C02/T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2
# Loading for a specific time range and just doing the mean to test the concept
landsat9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$filterBounds(sampleGT$geometry())$filter(ee$Filter$date("2023-01-01", "2023-12-31"))$reduce("mean")$clip(sampleGT$geometry())
landsat9 <- landsat9$addBands(landsat9$normalizedDifference(c('SR_B5_mean','SR_B4_mean'))$rename('NDVI'))
ee_print(landsat9)
Map$addLayer(landsat9$select('NDVI'), ndviVis, "NDVI - First")


l9Agg = landsat9$reproject(projExample)$reduceResolution(reducer=ee$Reducer$mean())$reproject(projExample)
ee_print(l9Agg)
Map$addLayer(l9Agg$select('NDVI'), ndviVis, "NDVI - First")

# define the reducer
saveTest <- ee_image_to_drive(image=l9Agg$select("NDVI"), description="landsat9_NDVI4", fileNamePrefix="landsat9_NDVI-Agg4", folder=GoogleFolderSave, timePrefix = F, region = sampleGT$geometry(), maxPixels = 10e12, scale=4470.698, crs=projCRS, crsTransform=projTransform)
saveTest$start()


##################### 
# Check the test Landsat 9 vs. the test raster
##################### 
origGT <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/data/data_sets/example_met_geotiff/Sample_Tmax30day.tif")
origGT
plot(origGT)

# newL9b <- raster("~/Google Drive/My Drive/landsat_data-TEST/")
newL9 <- raster("~/Google Drive/My Drive/landsat_data-TEST/landsat9_NDVI-Agg4.tif")
newL9
plot(newL9)

dfOrigGT <- data.frame(coordinates(origGT), Tmax30d=getValues(origGT))
summary(dfOrigGT)

dfl9Agg <- data.frame(coordinates(newL9), NDVI=getValues(newL9))
summary(dfl9Agg)

library(ggplot2)
ggplot(data=dfOrigGT) +
  geom_tile(data=dfl9Agg, aes(x=x, y=y, fill=NDVI), color="blue", alpha=0.5) +
  geom_tile(aes(x=x, y=y), color="black", fill="black", alpha=0.3)
