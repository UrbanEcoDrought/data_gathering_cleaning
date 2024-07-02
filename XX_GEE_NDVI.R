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
source("Workflow_NDVI_Landsat_by_Landcover/00_EarthEngine_HelperFunctions.R")
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
l8Mosaic$getInfo()
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

# aggregating up to the resolution that the climate data are at.

target_projection <- ee$Projection("EPSG:3857")
# scale <- res(temp.dat)

l8NDVI_reproj <- l8NDVI2$reproject(
  crs = target_projection,
  scale = 5462.168
)

# define the reducer
reducer <- ee$Reducer$mean()

l8NDVI_agg <- l8NDVI_reproj$reduceResolution(
  reducer = reducer,
  bestEffort = TRUE
)
ee_print(l8NDVI_agg)

saveTest <- ee_image_to_drive(image=l8NDVI_agg, description="landsat8_NDVI", fileNamePrefix="landsat8_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=5462.168, crs="EPSG:3857")
saveTest$start()



##################### 

##################### 
# Read in & Format Landsat 9 ----
##################### 
# "LANDSAT/LC09/C02/T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LC09_C02_T1_L2
landsat9 <- ee$ImageCollection("LANDSAT/LC09/C02/T1_L2")$filterBounds(chiBBox)$map(function(image){
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


l9NDVI2 <- ee$ImageCollection$toBands(l9NDVI)$rename(dates_list)
ee_print(l9NDVI2)

# Map$addLayer(l8NDVI2$select('2022-05-08'), ndviVis, 'NDVI')
# aggregating up to the resolution that the climate data are at.

target_projection <- ee$Projection("EPSG:3857")
# scale <- res(temp.dat)

l9NDVI_reproj <- l9NDVI2$reproject(
  crs = target_projection,
  scale = 5462.168
)

# define the reducer
reducer <- ee$Reducer$mean()

l9NDVI_agg <- l8NDVI_reproj$reduceResolution(
  reducer = reducer,
  bestEffort = TRUE
)
ee_print(l9NDVI_agg)
saveTest <- ee_image_to_drive(image=l9NDVI_agg, description="landsat9_NDVI", fileNamePrefix="landsat9_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=5462.168, crs="EPSG:3857")
saveTest$start()



##################### 

##################### 
# Read in & Format Landsat 7 ----
##################### 
# ""LANDSAT/LE07/C02/T1_L2""
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LE07_C02_T1_L2
landsat7 <- ee$ImageCollection("LANDSAT/LE07/C02/T1_L2")$filterBounds(chiBBox)$filterDate("2001-01-01", "2022-12-31")$map(function(image){
  return(image$clip(chiBBox))
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
target_projection <- ee$Projection("EPSG:3857")
# scale <- res(temp.dat)

l7NDVI_reproj <- l7NDVI2$reproject(
  crs = target_projection,
  scale = 5462.168
)

# define the reducer
reducer <- ee$Reducer$mean()

l7NDVI_agg <- l7NDVI_reproj$reduceResolution(
  reducer = reducer,
  bestEffort = TRUE
)

saveTest <- ee_image_to_drive(image=l7NDVI_agg, description="landsat7_NDVI", fileNamePrefix="landsat7_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=5462.168, crs="EPSG:3857")
saveTest$start()


##################### 


##################### 
# Read in & Format Landsat 5 ----
##################### 
# "LANDSAT_LT05_C02_T1_L2"
# Load MODIS NDVI data; attach month & year
# https://developers.google.com/earth-engine/datasets/catalog/LANDSAT_LT05_C02_T1_L2
landsat5 <- ee$ImageCollection("LANDSAT/LT05/C02/T1_L2")$filterBounds(chiBBox)$filterDate("2001-01-01", "2022-12-31")$map(function(image){
  return(image$clip(chiBBox))
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
target_projection <- ee$Projection("EPSG:3857")
# scale <- res(temp.dat)

l5NDVI_reproj <- l5NDVI2$reproject(
  crs = target_projection,
  scale = 5462.168
)

# define the reducer
reducer <- ee$Reducer$mean()

l5NDVI_agg <- l5NDVI_reproj$reduceResolution(
  reducer = reducer,
  bestEffort = TRUE
)

saveTest <- ee_image_to_drive(image=l5NDVI_agg, description="landsat5_NDVI", fileNamePrefix="landsat5_NDVI", folder=GoogleFolderSave, timePrefix = F, region = chiBBox, maxPixels = 10e12, scale=5462.168, crs="EPSG:3857")
saveTest$start()

##################### 
# bringing in NLCD data and saving
# STOOOOPPPP!!!
# Need to figure out the percent cover aggregation with christy's masks first. NLCD Not valid at the moment.
##################### 
# 0. Read in helper functions ----
##################### 
source("Workflow_NDVI_Landsat_by_Landcover/00_EarthEngine_HelperFunctions.R")
##################### 


##################### 
# Color Palette etc. ----
##################### 
# Setting the center point for the Arb because I think we'll have more variation
Map$setCenter(-88.04526, 41.81513, 11);

# Adding Landcover Classes!
nlcdPalette = c(
  '#5475A8', # Open Water (11)
  # '#d1def8', # Ice/Snow (12)
  '#dec5c5', # Developed, Open Space (21)
  '#d99282', # Developed, Low Intensity (22)
  '#eb0000', # Developed, Medium Intensity (23)
  '#ab0000', # Developed High Intensity (24)
  '#b3ac9f', # Barren Land (31)
  '#68ab5f', # Deciduous Forest (41)
  '#1c5f2c', # Evergreen Forest (42)
  '#b5c58f', # Mixed Forest (43)
  # '#af963c', # Dwarf Shrub/Scrub (51); Alaska Only
  '#ccb879', # Shrub/Scrub (52)
  '#dfdfc2', # Grassland/Herbaceous (71)
  # '#d1d182', # Sedge/herbaceous (72); Alaska Only
  # '#a3cc51', # lichens (73); Alaska Only
  # '#82ba9e', # Moss (74); Alaska Only
  '#dcd939', # Pasture/Hay (81)
  '#ab6c28', # Cultivated Crops (82)
  '#b8d9eb', # Woody Wetlands (90)
  '#6c9fb8' # Emergent Herbaceous Wetlands (95)
);

nlcdvis = list(
  min= 0,
  max= 95,
  palette= nlcdPalette
);
##################### 

##################### 
# Read in base layers ----
##################### 
# Chicago = ee$FeatureCollection("projects/breidyee/assets/SevenCntyChiReg") 
# ee_print(Chicago)

# chiBounds <- Chicago$geometry()$bounds()
chiBBox <- ee$Geometry$BBox(-88.70738, 41.20155, -87.52453, 42.49575)

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2019_REL_NLCD
nlcdChi <- ee$ImageCollection('USGS/NLCD_RELEASES/2019_REL/NLCD')$select('landcover')$map(function(img){
  d <- ee$Date(ee$Number(img$get('system:time_start')));
  y <- ee$Number(d$get('year'));
  return(img$clip(chiBBox)$set('year', y))
})

# https://developers.google.com/earth-engine/datasets/catalog/USGS_NLCD_RELEASES_2021_REL_NLCD#description
nlcdChi21 <- ee$ImageCollection('USGS/NLCD_RELEASES/2021_REL/NLCD')$select('landcover')$map(function(img){
  d <- ee$Date(ee$Number(img$get('system:time_start')));
  y <- ee$Number(d$get('year'));
  return(img$clip(chiBBox)$set('year', y))
})

# ee_print(nlcdChi21)

lcVals = nlcdChi$first()$reduceRegion(ee$Reducer$frequencyHistogram(), chiBBox, maxPixels=1e12)
# ee_print(lcVals)
lcVals$getInfo()

# ee_print(nlcdChi) # Note: the nlcd is giving me a strsplit code error, but it does map!
Map$addLayer(nlcdChi$first()$select('landcover'), nlcdvis, 'NLCD Land Cover',);
Map$addLayer(nlcdChi21$first()$select('landcover'), nlcdvis, 'NLCD Land Cover',);


projNLCD = nlcdChi$select("landcover")$first()$projection()
projNLCD$getInfo() # Note that this is really messy
# projCRS = projNLCD$crs()
projCRS = "EPSG:4326" # This seems to be what works
projTransform <- unlist(projNLCD$getInfo()$transform) # I saved this, but using it in the export causes really weird things

# Chicago <- Chicago$map(function(FEAT){return(FEAT$reproject(projNLCD))})

# chiGeom <- Chicago$geometry()
# 
# chiTransform = c(1,0,0,0,1,0)



nlcdProj = "EPSG:4326"
nlcdTransform = c(30, 0, -2493045, 0, -30, 3310005)
##################### 

##################### 
# Create annually-resolved image ----
##################### 
lcChi2001 <- nlcdChi$filter(ee$Filter$eq('system:index', '2001'))$first();
# ee_print(lcChi2001)
# Map$addLayer(lcChi2001, nlcdvis, 'NLCD Land Cover');
lcChi2004 <- nlcdChi$filter(ee$Filter$eq('system:index', '2004'))$first();
lcChi2006 <- nlcdChi$filter(ee$Filter$eq('system:index', '2006'))$first();
lcChi2008 <- nlcdChi$filter(ee$Filter$eq('system:index', '2008'))$first();
lcChi2011 <- nlcdChi$filter(ee$Filter$eq('system:index', '2011'))$first();
lcChi2013 <- nlcdChi$filter(ee$Filter$eq('system:index', '2013'))$first();
lcChi2016 <- nlcdChi$filter(ee$Filter$eq('system:index', '2016'))$first();
lcChi2019 <- nlcdChi$filter(ee$Filter$eq('system:index', '2019'))$first();
lcChi2021 <- nlcdChi21$filter(ee$Filter$eq('system:index', '2021'))$first();
# ee_print(lcChi2019)
# Map$addLayer(lcChi2019, nlcdvis, 'NLCD Land Cover 2019');
# Map$addLayer(lcChi2021, nlcdvis, 'NLCD Land Cover 2021');

# Creating duped layers for each year for our sanity
lcChi2000 <- lcChi2001$set('system:time_start', ee$Date$fromYMD(2000, 1, 1))$set('year',2000);
# lcChi2001 <- nlcdChi$filter(ee$Filter$eq('system:index', '2001'))$first();
lcChi2002 <- lcChi2001$set('system:time_start', ee$Date$fromYMD(2002, 1, 1))$set('year',2002);
lcChi2003 <- lcChi2004$set('system:time_start', ee$Date$fromYMD(2003, 1, 1))$set('year',2003);
# lcChi2004 <- nlcdChi$filter(ee$Filter$eq('system:index', '2004'))$first();
lcChi2005 <- lcChi2006$set('system:time_start', ee$Date$fromYMD(2005, 1, 1))$set('year',2005);
# lcChi2006 <- nlcdChi$filter(ee$Filter$eq('system:index', '2006'))$first();
lcChi2007 <- lcChi2008$set('system:time_start', ee$Date$fromYMD(2007, 1, 1))$set('year',2007);
# lcChi2008 <- nlcdChi$filter(ee$Filter$eq('system:index', '2008'))$first();
lcChi2009 <- lcChi2008$set('system:time_start', ee$Date$fromYMD(2009, 1, 1))$set('year',2009);
lcChi2010 <- lcChi2011$set('system:time_start', ee$Date$fromYMD(2010, 1, 1))$set('year',2010);
# lcChi2011 <- nlcdChi$filter(ee$Filter$eq('system:index', '2011'))$first();
lcChi2012 <- lcChi2013$set('system:time_start', ee$Date$fromYMD(2012, 1, 1))$set('year',2012);
# lcChi2013 <- nlcdChi$filter(ee$Filter$eq('system:index', '2013'))$first();
lcChi2014 <- lcChi2013$set('system:time_start', ee$Date$fromYMD(2014, 1, 1))$set('year',2014);
lcChi2015 <- lcChi2016$set('system:time_start', ee$Date$fromYMD(2015, 1, 1))$set('year',2015);
# lcChi2016 <- nlcdChi$filter(ee$Filter$eq('system:index', '2016'))$first();
lcChi2017 <- lcChi2016$set('system:time_start', ee$Date$fromYMD(2017, 1, 1))$set('year',2017);
lcChi2018 <- lcChi2019$set('system:time_start', ee$Date$fromYMD(2018, 1, 1))$set('year',2018);
# lcChi2019 <- nlcdChi$filter(ee$Filter$eq('system:index', '2019'))$first();
lcChi2020 <- lcChi2019$set('system:time_start', ee$Date$fromYMD(2020, 1, 1))$set('year',2020);
lcChi2021 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2021, 1, 1))$set('year',2021);
lcChi2022 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2022, 1, 1))$set('year',2022);
lcChi2023 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2023, 1, 1))$set('year',2023);
lcChi2024 <- lcChi2021$set('system:time_start', ee$Date$fromYMD(2024, 1, 1))$set('year',2024);


collAnn <- ee$ImageCollection(c(lcChi2001, lcChi2002, lcChi2003, lcChi2004, lcChi2005, lcChi2006, lcChi2007, lcChi2008, lcChi2009, lcChi2010, lcChi2011, lcChi2012, lcChi2013, lcChi2014, lcChi2015, lcChi2016, lcChi2017, lcChi2018, lcChi2019, lcChi2020, lcChi2021, lcChi2022, lcChi2023, lcChi2024))
ee_print(collAnn)

# Saving will be much easier if it's a single year with multiple bands
yrLC <- ee$List(collAnn$aggregate_array("year"))$distinct()
# yrLC$getInfo()
yrString <- ee$List(paste0("YR", yrLC$getInfo()))

lcChiAnn <- ee$ImageCollection$toBands(collAnn)$rename(yrString)
# ee_print(lcChiAnn)
# Map$addLayer(lcChiAnn$select("YR2012"), nlcdvis, 'NLCD Land Cover');

# aggregating land cover and reporjecting to match the climate data
target_projection <- ee$Projection("EPSG:3857")
# scale <- res(temp.dat)

lcChiAnn_reproj <- lcChiAnn$reproject(
  crs = target_projection,
  scale = 5462.168
)

# define the reducer
reducer <- ee$Reducer$mean()

lcChiAnn_agg <- lcChiAnn_reproj$reduceResolution(
  reducer = reducer,
  bestEffort = TRUE
)

saveLandCover <- ee_image_to_drive(image=lcChiAnn, description="Save_NLCD-Chicago_AnnualDupe_2000-2024", fileNamePrefix="NLCD-Chicago_AnnualDupe_2000-2024", folder=GoogleFolderSave,timePrefix = F, maxPixels = 10e12, scale=5462.168, region = chiBBox, crs="EPSG:3857")
saveLandCover$start()
