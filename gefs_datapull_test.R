library(gdalcubes)
library(gefs4cast)
# source("gefs_to_parquet.R")
# source("gefs_functino_suite.R")

gdalcubes_options(parallel=T) #optional
dir.create("meow")
# gefs_to_parquet(Sys.Date(), c(mean = "geavg", spr = "gespr"), "todays_forecast")

# This is downloading the 0.5degree data by default.
# Will want to see if we can get the 0.25 degree data
# may require tweaking some of the functions that the 'gefs_to_parquet' function below relies upon

# files I think we need to tweak are in the 'gdalcubes-methods.R fils of the gefs4cast function.
gefs_to_parquet(dates = as.Date("2023-03-01"), ensemble = c(mean = "geavg", spr = "gespr"), "meow")


df <- arrow::open_dataset("meow/")

meow <- dplyr::collect(df)
head(meow)

summary(meow$datetime)
summary(as.Date(meow$reference_datetime))

# getting unique sites.
unique(meow$site_id)

hf.dat <- meow[meow$site_id=="HARV",]

# looking at unique variables that are available in this data pull
# Variable explanations found here: https://www.nco.ncep.noaa.gov/pmb/products/gens/geavg.t00z.pgrb2a.0p50.f003.shtml
# 
# "PRES"= "band57", Surface Pressure
# "TMP" = "band63", 2m surface temperature K
# "RH" = "band64", 2m relative humidity %
# "UGRD" = "band67", 10m u component of wind [m/s]
# "VGRD" = "band68", 10m v component of wind [m/s]
# "APCP" = "band69", surface total precipitation [kg/m^2]
# "DSWRF" = "band78", surface downward shortwave radiation [W/m^2]
# "DLWRF" = "band79"), surface downward longwave radiation [W/m^2]

unique(meow$variable)
