# Update images of landsat-based NDVI with the latest information 
# To be run ~ weekly!

# Note: Down the road this may get moved especially if we do more formal modeling of NDVI to give quantitative assessment of change in NDVI etc.

# Steps:
# 1. Read in the all existing landsat data
# 2. Make and graphs 

library(ggplot2)
# path.google <- ("~/Google Drive/Shared drives/Urban Ecological Drought/data/landsat_NDVI")
path.google <- ("~/Google Drive/My Drive/")
# NDVIsave <- "UrbanEcoDrought_NDVI_LocalExtract"
NDVIsave <- "UrbanEcoDrought_NDVI_LocalExtract-RAW"
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/data/UrbanEcoDrought_NDVI_LocalExtract/")
# pathShare <- "~/Google Drive/Shared drives/Urban Ecological Drought/data/UrbanEcoDrought_NDVI_LocalExtract/"

fNDVI <- dir(file.path(path.google, NDVIsave))


day.labels <- data.frame(Date=seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels$Date))
day.labels
summary(day.labels)

# Clunky code, but should pull the latest file
lcnames <- c("forest", "forest-wet", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

ndviAll <- data.frame()
  fileL8For <- dir(file.path(path.google, NDVIsave), paste0("Landsat8_", "forest", "_"))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat8_", "forest", "_" )))]
  fileL8ForWet <- dir(file.path(path.google, NDVIsave), paste0("Landsat8_", "forest-wet", "_"))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat8_", "forest-wet", "_")))]
  
  landsat8For <- read.csv(file.path(path.google, NDVIsave, fileL8For))
  landsat8ForWet <- read.csv(file.path(path.google, NDVIsave, fileL8ForWet))
  
  landsat8$mission <- "landsat 8"

  landsatAll <- rbind(landsat8, landsat9, landsat7, landsat5)
  # landsatAll <- rbind(landsat8, landsat9)
  landsatAll$type <- LCTYPE
  
  ndviAll <- rbind(ndviAll, landsatAll)
}
