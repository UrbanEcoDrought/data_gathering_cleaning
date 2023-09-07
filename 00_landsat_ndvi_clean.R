# organizing the various NDVI products that Christy produced from GEE
# pulling in data from googledrive
library(ggplot2)

Sys.setenv(GOOGLE_DRIVE = "G:/Shared drives/Urban Ecological Drought")

google.drive <- Sys.getenv("GOOGLE_DRIVE")

file.names <- dir(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract"))
file.names

lc.types <- unique( stringr::str_split_i(file.names, "_", 2))

# test run to see what the file structure looks like.
test <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/Landsat5_crop_2023_08_24_10_14_09.csv"), header=T)
head(test)

# reading in each individual .csv file and compiling into a single object
ndvi.all <- NULL
for(i in 1:length(file.names)){
  ndvi.temp <- read.csv(file.path(google.drive, "data/UrbanEcoDrought_NDVI_LocalExtract/", file.names[i]), header=T)
  
  ndvi.temp$type <- as.factor(stringr::str_split_i(file.names[i], "_", 2)) # pulling in the land cover type from file name
  ndvi.temp$satellite <- as.factor(stringr::str_split_i(file.names[i], "_", 1)) # pulling in the collection satellite name
  if(is.null(ndvi.all)) ndvi.all <- ndvi.temp else ndvi.all <- rbind(ndvi.all, ndvi.temp) # compiling into a single object
  
}

head(ndvi.all)
summary(ndvi.all)

# setting date as date
ndvi.all$date <- lubridate::as_date(ndvi.all$date)

# creating a year variable for easier subsetting
ndvi.all$year <- lubridate::year(ndvi.all$date)

# creatign a day of year variable in case it is useful
ndvi.all$doy <- lubridate::yday(ndvi.all$date)
head(ndvi.all)

ggplot(data=ndvi.all) + facet_wrap(type~.) +
  geom_line(aes(x=date, y=NDVI, col=type))

saveRDS(ndvi.all, file.path(google.drive, "data/r_files/processed_files/landsat_ndvi_all.RDS"))
