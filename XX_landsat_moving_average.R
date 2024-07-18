# want to create a moving average for each satellite for each pixel
library(ggplot2)
library(lubridate)
library(zoo)
library(dplyr)

# Loading in landsat data

landsat.df <- readRDS("processed_data/landsat_NDVI_spatial.RDS")

head(landsat.df)

landsat.roll <- landsat.df %>%
  arrange(date) %>%
  group_by(x,y,landsat) %>%  # Adjust this line if you have different spatial groups
  mutate(moving_avg_ndvi = rollmean(ndvi_value, k = 14, fill = NA, align = "right"))

head(landsat.roll)
summary(landsat.roll)

# plotting to check on completeness of data
ggplot(data=landsat.roll[landsat.roll$year=="2020" & landsat.df$month=="6",]) + facet_grid(date~landsat) +
  geom_tile(aes(x=x, y=y, fill=ndvi_value)) +
  theme_bw()

ggplot(data=landsat.roll[landsat.roll$year=="2020" & landsat.df$month=="6",]) + facet_grid(date~landsat) +
  geom_tile(aes(x=x, y=y, fill=moving_avg_ndvi)) +
  theme_bw()
