# library(raster)
library(terra)
library(sf)
library(ggplot2)

# Loading in the sample temperature data from trent to see what the resolution and projection are
temp.dat <- rast("input_data/Sample_Tmax30day.tif")
temp.dat

plot(temp.dat)
res(temp.dat)

# changing projection to get things into a resolution that is in meters
temp.dat.reproj <- project(temp.dat, "EPSG:3857")
temp.dat.reproj

target_projection <- crs(temp.dat.reproj)
scale <- 5462.168

# loading in data from GEE to see how it looks
l8.test <- rast("input_data/landsat_data/landsat8_NDVI.tif")
l8.test
plot(l8.test)[1]


# Landsat Data Stacking----

# setting up a loop to read in landsat data and stack into a single data frame
file.dir <- dir("input_data/landsat_data/")

landsat.df <- NULL

for(i in unique(file.dir)) {
  
  landsat.name <- stringr::str_split(i, "_")[[1]][1]
  temp.rast <- rast(paste0("input_data/landsat_data/", i))
  
  temp.df <- terra::as.data.frame(temp.rast, xy=T)
  
  temp.df.stack <- stack(temp.df[,!names(temp.df) %in% c("x", "y")])
  names(temp.df.stack) <- c("ndvi_value", "date")
  temp.df.stack$landsat <- landsat.name
  temp.df.stack$x <- temp.df$x
  temp.df.stack$y <- temp.df$y
  temp.df.stack$date <- lubridate::as_date(temp.df.stack$date)
  
  if(is.null(landsat.df)) landsat.df <- temp.df.stack else landsat.df <- rbind(landsat.df,temp.df.stack)
  print(i)
}

head(landsat.df)
unique(landsat.df$landsat)

landsat.df$year <- lubridate::year(landsat.df$date)
landsat.df$month <- lubridate::month(landsat.df$date)

ggplot(data=landsat.df[landsat.df$year=="2020" & landsat.df$month=="6",]) + facet_wrap(~date) +
  geom_tile(aes(x=x, y=y, fill=ndvi_value)) +
  theme_bw()
  

################
# loading in NLCD to merge in the land cover classes together.

lc.dat <- rast(file.path("input_data/nlcd_2021_land_cover_l48_20230630.img"))
plot(lc.dat)

target_crs <- crs(temp.dat.reproj)
target_res <- res(temp.dat.reproj)
target_extent <- terra::ext(temp.dat.reproj)

head(lc.dat)

lc.dat.reproj <- project(lc.dat, target_crs)

# reading in NLCD data----
nlcd.rast <- rast("input_data/landsat_data/NLCD-Chicago_AnnualDupe_2000-2024.tif")
nlcd.rast

plot(nlcd.rast)
head(nlcd.rast)

# data are categorical. Need to come up wiht a crosswalk
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

# data are spotty. May want to take a couple of week average.
saveRDS(landsat.df, file = "G:/Shared drives/Urban Ecological Drought/data/r_files/processed_files/landsat_NDVI_spatial.RDS")
saveRDS(landsat.df, file = "processed_data/landsat_NDVI_spatial.RDS")
write.csv(landsat.df, file = "G:/Shared drives/Urban Ecological Drought/data/r_files/processed_files/landsat_NDVI_spatial.csv", row.names=F)

