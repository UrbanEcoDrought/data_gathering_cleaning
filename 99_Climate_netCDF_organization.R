# organizing climate data into a dataframe that we can work with
library(dplyr)
library(terra)
library(ggplot2)
library(ncdf4)
library(lubridate)
library(sf)

# loading in climate data----
# Specifying Paths
google.drive <-  "G:/Shared drives/Urban Ecological Drought"
clim.path <- file.path(google.drive, "data/data_sets/Daily Meteorological Data")
clim.files <- dir(clim.path)
clim.files.nc <- clim.files[grep(".nc", clim.files)]

climateData <- NULL

pb <- txtProgressBar(min=0, max=length(clim.files.nc), style=3)
pb.ind=1
for(i in clim.files.nc){
  temp <- nc_open(file.path(clim.path, i))
  varNow <- names(temp$var)
  varLabprep <- stringr::str_split_i(i,"_",2)
  varLab <- tolower(stringr::str_split_i(varLabprep, "[.]", 1))
  
  # checkign dimension indexing
  # summary(temp$dim)
  dimLat <- ncvar_get(temp, "lat")
  dimLon <- ncvar_get(temp, "lon")
  
  # temp$dim$time$units # we don't have unit information; PROBABLY days since 1900
  dimTime <- ncvar_get(temp, "time")
  # dimDate <- lubridate::ymd("0000-01-01") + c(dimTime)-1 # There's a way to say that dimTime are days, but I forget
  dimDate <- as_date(dimTime, origin = "0000-01-01")-1 # subtracting 1 to be sure that we are starting on Jan1
  head(dimDate)
  
  # # If you want ALL the data start here
  # tempData <- ncvar_get(temp, varNow) # dims are lat, lon, time
  # dim(tempData)
  
  tempRast <- rast(file.path(clim.path, i))
  base_crs <- crs(tempRast)
  tempRast.reproj <- project(tempRast, "EPSG:3857")
  
  tempDF <- terra::as.data.frame(tempRast,xy=T)
  tempDF2 <- stack(tempDF[,!names(tempDF) %in% c("x", "y")])
  names(tempDF2) <- c(varLab, "date")
  tempDF2$x <- tempDF$x
  tempDF2$y <-tempDF$y
  tempDF2$date <- dimDate
  
  ggplot(data=tempDF2[tempDF2$date=="2020-06-01",]) +
    geom_raster(aes(x=x, y=y, fill=spei14day))
  
  # We only have NDVI for 2000-present, so lets just pull a subset; data is in dims of c(lat,lon, time)
  indTimeStart <- which(dimDate=="2000-01-01")
  dimDateShort <- as_date(seq(from=ymd("2000-01-01"), to=max(dimDate), by="day"))
  
  tempData <- ncvar_get(temp, varNow, start=c(1,1,indTimeStart), count=c(length(dimLat), length(dimLon), length(dimTime)-indTimeStart+1)) # dims are lat, lon, time
  dim(tempData)
  dimnames(tempData) <- list(latitude=dimLat, longitude=dimLon, date=dimDateShort)
  
  # tempR1 <- rast(tempData, crs=base_crs, extent=ext(test))
  tempRast <- rast(aperm(tempData, c(2,1,3)), crs=base_crs, extent=ext(test))
 
  temp.reproj <- project(tempRast, "EPSG:3857")
  tempDF <- terra::as.data.frame(temp.reproj, xy=T)
  names(tempDF) <- c("x", "y", paste0(c(dimDateShort)))
  
  tempDF.stack <- stack(tempDF[,!names(tempDF) %in% c("x", "y")])
  names(tempDF.stack) <- c(varLab, "date")
  tempDF.stack$x <- tempDF$x
  tempDF.stack$y <- tempDF$y
  
  if(is.null(climateData)) climateData <- tempDF.stack else climateData <- merge(climateData, tempDF.stack, by=c("date", "x", "y"), all=T)
  saveRDS(climateData, file="processed_data/climate_spatial.RDS")
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}
