library(ggplot2)
library(sf)
library(terra)
library(ggthemes)
# Specifying Paths----
google.drive <-  "G:/Shared drives/Urban Ecological Drought"

# loading in NDVI data----

ndvi.dat <- readRDS("processed_data/landsat_NDVI_spatial.RDS") # data also on Drought google drive but loaded Ross's local copy for speed
head(ndvi.dat)
summary(ndvi.dat)


ggplot(data=ndvi.dat[,c("ndvi.lag21d", "date")]) +
  geom_histogram(aes(x=date))

pdf(file="figures/NDVI_check.pdf", height = 8, width = 8)
for(i in unique(ndvi.dat$date[order(ndvi.dat$date, decreasing = F)])){
print(ggplot(data=ndvi.dat[ndvi.dat$date==i,]) + facet_grid(~landsat) +
  geom_raster(aes(x=x, y=y, fill=ndvi_value)) +
    scale_fill_gradient2(low = "#d8b365", mid = "#f5f5f5", high = "#5ab4ac", midpoint=median(ndvi.dat$ndvi_value, na.rm=T)) +
  labs(title = paste0(as.Date(i))) +
  theme_map())
}
dev.off()


# Something is really off in the lag assignment. Not sure what's going on there.

pdf(file="figures/NDVI_21dayLag_check.pdf", height = 8, width = 8)
for(i in unique(ndvi.dat$date[order(ndvi.dat$date, decreasing = F)])){
  print(ggplot(data=ndvi.dat[ndvi.dat$date==i,]) + facet_grid(~landsat) +
          geom_raster(aes(x=x, y=y, fill=ndvi.lag21d)) +
          scale_fill_gradient2(low = "#d8b365", mid = "#f5f5f5", high = "#5ab4ac", midpoint=median(ndvi.dat$ndvi.lag21d, na.rm=T)) +
          labs(title = paste0(as.Date(i))) +
          theme_map())
}
dev.off()