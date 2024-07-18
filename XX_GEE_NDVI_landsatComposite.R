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
landsat.df$doy <- lubridate::yday(landsat.df$date)

ggplot(data=landsat.df[landsat.df$year=="2020" & landsat.df$month=="6",]) + facet_grid(date~landsat) +
  geom_tile(aes(x=x, y=y, fill=ndvi_value)) +
  theme_bw()
  
ggplot(data=landsat.df[landsat.df$landsat=="landsat8",]) +
  geom_point(aes(x=doy, y=ndvi_value, col=landsat), alpha = 0.1) +
  stat_smooth(aes(x=doy, y=ndvi_value, col=landsat), method="gam",
              formula = y~s(x, bs="cs"))+
  theme_minimal()


# want to add in the lagged time series just like we do with the regional means
# see if we can keep it unique to individual satellites

# creating a variable for x.y combo
landsat.df$xy.coord <- paste(landsat.df$x, landsat.df$y, sep="[.]")

landsat.df2 <- NULL

pb <- txtProgressBar(min=0, max=length(land), style=3)
pb.ind=1

for(s in unique(landsat.df$landsat)){
  print(s)
  sat.temp <- landsat.df[landsat.df$landsat==s,]
  sat.temp <- sat.temp[order(sat.temp$date, decreasing = F),]
  
  
  pb <- txtProgressBar(min=0, max=length(unique(sat.temp$xy.coord)), style=3)
  pb.ind=1
  for(xy in unique(sat.temp$xy.coord)){
    coord.temp <- sat.temp[sat.temp$xy.coord==xy,]
    coord.temp <- coord.temp[order(coord.temp$date, decreasing = F),]
    
    coord.temp$ndvi.lag14d <- NA
    for(d in 1:nrow(coord.temp)){
      
      rowLag <- which(coord.temp$date>=(coord.temp$date[d]-14) & coord.temp$date<coord.temp$date[d])
      
      if(length(rowLag)<1) next
      if(length(rowLag)==1) coord.temp$ndvi.lag14d[d] <- coord.temp$ndvi_value[rowLag]
      if(length(rowLag)>1) coord.temp$ndvi.lag14d[d] <- mean(coord.temp$ndvi_value[rowLag], na.rm=T)
      
    }
   sat.temp2 <- merge(sat.temp, coord.temp, all.x=T)
   setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
  }
  if(is.null(landsat.df2)) landsat.df2 <- sat.temp2 else lansat.df2 <- rbind(landsat.df2, sat.temp2)
}




# data are spotty. May want to take a couple of week average.
saveRDS(landsat.df2, file = "G:/Shared drives/Urban Ecological Drought/data/r_files/processed_files/landsat_NDVI_spatial.RDS")
saveRDS(landsat.df2, file = "processed_data/landsat_NDVI_spatial.RDS")
write.csv(landsat.df2, file = "G:/Shared drives/Urban Ecological Drought/data/r_files/processed_files/landsat_NDVI_spatial.csv", row.names=F)



