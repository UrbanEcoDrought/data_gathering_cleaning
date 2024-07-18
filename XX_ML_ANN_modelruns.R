# Building Artificial neural network model to predict NDVI from climate and lag across space

library(neuralnet)
library(dplyr)
library(caret)
library(terra)
library(ggplot2)
# Specifying Paths----
google.drive <-  "G:/Shared drives/Urban Ecological Drought"

# loading in NDVI data----

ndvi.dat <- readRDS("processed_data/landsat_NDVI_spatial.RDS") # data also on Drought google drive but loaded Ross's local copy for speed
head(ndvi.dat)


# loading in climate data----
clim.dat <- rast("input_data/Sample_Tmax30day.tif")
clim.dat
clim.dat2 <- project(clim.dat, "EPSG:3857")


clim.df <- as.data.frame(clim.dat2, xy=T)
head(clim.df)
names(clim.df) <- c("x", "y", "temp.30day")

ggplot(data = ndvi.dat[ndvi.dat$date %in% "2011-08-07",]) +
  geom_raster(aes(x=x, y=y, fill=ndvi_value)) +
  geom_raster(data=clim.df, aes(x=x, y=y, fill=temp.30day),fill="red3")

# merging NDVI and climate data together----
coord.check <- ndvi.dat[ndvi.dat$x %in% clim.df$x,]

chi.dat <- merge(ndvi.dat, clim.df, by=c("x", "y"))
summary(chi.dat)
