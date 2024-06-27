# saving spatial NDVI data for machine learning exercises

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'malexander@anl.gov', drive=T, project="nbs2023-malexander")
path.google.CR <- "~/Google Drive/My Drive/UrbanEcoDrought/"
path.google.share <- "G:/Shared drives/Urban Ecological Drought/data/data_sets/landsat_spatial/"
NDVIsave <- "UrbanEcoDrought_NDVI_LocalExtract"
# GoogleFolderSave <- "UHI_Analysis_Output_Final_v2"
assetHome <- ee_get_assethome()


GoogleFolderSave <- "landsat_data"
##################### 
# 0. Read in helper functions ----
##################### 
source("../UrbanDrought_SpatialAnalysis_Chicago/Workflow_NDVI_Landsat_by_Landcover/00_EarthEngine_HelperFunctions.R")
##################### 


##################### 
# Color Palette etc. ----
##################### 
# Setting the center point for the Arb because I think we'll have more variation
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
#####################

##################### 
# Read in & Format Landsat 8 ----
##################### 
# "LANDSAT/LC08/C02/T1_RT"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC08_C02_T1_L2
landsat8 <- ee$ImageCollection("LANDSAT/LC08/C02/T1_L2")$filterBounds(chiBBox)$map(function(image){
  return(image$clip(chiBBox))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat8$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat8)
# Map$addLayer(landsat8$first()$select('NDVI'))

l8Mosaic = mosaicByDate(landsat8, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
ee_print(l8Mosaic, "landsat8-Mosaic")
l8Mosaic$getInfo
# Map$addLayer(l8Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# pulling out just ndvi from the collection
l8NDVI = l8Mosaic$select("NDVI")
# l8NDVI$getInfo

# test = l8NDVI
# test = test$filterDate("2022-05-01", "2022-05-20")
# test = test$filterBounds(chiBBox)

# Function to extract the date from each image
get_date <- function(image) {
  date <- ee$Date(image$get("system:time_start"))$format("YYYY-MM-dd")
  ee$Feature(NULL, list(date = date))
}

# Map the function over the image collection
dates_features <- l8NDVI$map(get_date)

# Convert the collection of features to a list of dates
dates_list <- dates_features$aggregate_array("date")$getInfo()
print(dates_list)


l8NDVI2 <- ee$ImageCollection$toBands(l8NDVI)$rename(dates_list)
ee_print(l8NDVI2)

# Map$addLayer(l8NDVI2$select('2022-05-08'), ndviVis, 'NDVI')

saveTest <- ee_image_to_drive(image=l8NDVI2, description="landsat8_NDVI", fileNamePrefix="landsat8_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=30, crs="EPSG:4326")
saveTest$start()



##################### 

##################### 
# Read in & Format Landsat 9 ----
##################### 
# "LANDSAT/LC09/C02/T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2
landsat9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$filterBounds(Chicago)$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B10')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7', 'ST_B10'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat9$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat9)
# Map$addLayer(landsat9$first()$select('NDVI'))

l9Mosaic = mosaicByDate(landsat9, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l9Mosaic, "landsat9-Mosaic")
# Map$addLayer(l9Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# pulling out just ndvi from the collection
l9NDVI = l9Mosaic$select("NDVI")
# l8NDVI$getInfo

# test = l8NDVI
# test = test$filterDate("2022-05-01", "2022-05-20")
# test = test$filterBounds(chiBBox)

# Function to extract the date from each image
get_date <- function(image) {
  date <- ee$Date(image$get("system:time_start"))$format("YYYY-MM-dd")
  ee$Feature(NULL, list(date = date))
}

# Map the function over the image collection
dates_features <- l9NDVI$map(get_date)

# Convert the collection of features to a list of dates
dates_list <- dates_features$aggregate_array("date")$getInfo()
print(dates_list)


l9NDVI2 <- ee$ImageCollection$toBands(l8NDVI)$rename(dates_list)
ee_print(l9NDVI2)

# Map$addLayer(l8NDVI2$select('2022-05-08'), ndviVis, 'NDVI')

saveTest <- ee_image_to_drive(image=l9NDVI2, description="landsat9_NDVI", fileNamePrefix="landsat9_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=30, crs="EPSG:4326")
saveTest$start()



##################### 

##################### 
# Read in & Format Landsat 7 ----
##################### 
# ""LANDSAT/LE07/C02/T1_L2""
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2
landsat7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$filterBounds(Chicago)$filterDate("2001-01-01", "2022-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat7$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat7)
# Map$addLayer(landsat7$first()$select('NDVI'))

l7Mosaic = mosaicByDate(landsat7, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l7Mosaic, "landsat7-Mosaic")
# Map$addLayer(l7Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# pulling out just ndvi from the collection
l7NDVI = l7Mosaic$select("NDVI")
# l7NDVI$getInfo

# test = l7NDVI
# test = test$filterDate("2022-05-01", "2022-05-20")
# test = test$filterBounds(chiBBox)

# Function to extract the date from each image
get_date <- function(image) {
  date <- ee$Date(image$get("system:time_start"))$format("YYYY-MM-dd")
  ee$Feature(NULL, list(date = date))
}

# Map the function over the image collection
dates_features <- l7NDVI$map(get_date)

# Convert the collection of features to a list of dates
dates_list <- dates_features$aggregate_array("date")$getInfo()
print(dates_list)


l7NDVI2 <- ee$ImageCollection$toBands(l7NDVI)$rename(dates_list)
ee_print(l7NDVI2)

# Map$addLayer(l7NDVI2$select('2022-05-08'), ndviVis, 'NDVI')

saveTest <- ee_image_to_drive(image=l7NDVI2, description="landsat7_NDVI", fileNamePrefix="landsat7_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=30, crs="EPSG:4326")
saveTest$start()


##################### 


##################### 
# Read in & Format Landsat 5 ----
##################### 
# "LANDSAT_LT05_C02_T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2
landsat5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")$filterBounds(Chicago)$filterDate("2001-01-01", "2022-12-31")$map(function(image){
  return(image$clip(Chicago))
})$map(function(img){
  d= ee$Date(img$get('system:time_start'));
  dy= d$get('day');    
  m= d$get('month');
  y= d$get('year');
  
  # # Add masks 
  img <- applyLandsatBitMask(img)
  
  # #scale correction; doing here & separating form NDVI so it gets saved on the image
  lAdj = img$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7'))$multiply(0.0000275)$add(-0.2);
  lst_k = img$select('ST_B6')$multiply(0.00341802)$add(149);
  
  # img3 = img2$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y)
  return(img$addBands(srcImg=lAdj, overwrite=T)$addBands(srcImg=lst_k, overwrite=T)$set('date',d, 'day',dy, 'month',m, 'year',y))
})$select(c('SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B7', 'ST_B6'),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K'))$map(addNDVI)
# Map$addLayer(landsat5$first()$select('NDVI'), ndviVis, "NDVI - First")
# ee_print(landsat5)
# Map$addLayer(landsat5$first()$select('NDVI'))

l5Mosaic = mosaicByDate(landsat5, 7)$select(c('blue_median', 'green_median', 'red_median', 'nir_median', 'swir1_median', 'swir2_median', 'LST_K_median', "NDVI_median"),c('blue', 'green', 'red', 'nir', 'swir1', 'swir2', 'LST_K', "NDVI"))$sort("date")
# ee_print(l5Mosaic, "landsat5-Mosaic")
# Map$addLayer(l5Mosaic$first()$select('NDVI'), ndviVis, "NDVI - First")

# pulling out just ndvi from the collection
l5NDVI = l5Mosaic$select("NDVI")
# l5NDVI$getInfo

# test = l5NDVI
# test = test$filterDate("2022-05-01", "2022-05-20")
# test = test$filterBounds(chiBBox)

# Function to extract the date from each image
get_date <- function(image) {
  date <- ee$Date(image$get("system:time_start"))$format("YYYY-MM-dd")
  ee$Feature(NULL, list(date = date))
}

# Map the function over the image collection
dates_features <- l5NDVI$map(get_date)

# Convert the collection of features to a list of dates
dates_list <- dates_features$aggregate_array("date")$getInfo()
print(dates_list)


l5NDVI2 <- ee$ImageCollection$toBands(l5NDVI)$rename(dates_list)
ee_print(l5NDVI2)

# Map$addLayer(l5NDVI2$select('2022-05-08'), ndviVis, 'NDVI')

saveTest <- ee_image_to_drive(image=l5NDVI2, description="landsat5_NDVI", fileNamePrefix="landsat5_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=30, crs="EPSG:4326")
saveTest$start()

##################### 

