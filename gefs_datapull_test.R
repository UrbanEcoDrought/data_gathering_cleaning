library(gdalcubes)
library(gefs4cast)
# source("gefs_to_parquet.R")
# source("00_NOAA_GEFS_functions.R")

gdalcubes_options(parallel=T) #optional
dir.create("meow")
# gefs_to_parquet(Sys.Date(), c(mean = "geavg", spr = "gespr"), "todays_forecast")

# This is downloading the 0.5degree data by default..
# Will want to see if we can get the 0.25 degree data
# may require tweaking some of the functions that the 'gefs_to_parquet' function below relies upon


# setting custom lat:lon for the morton
# example from neon_sites: st_as_sf(sites,coords=c("longitude", "latitude"),
#          crs = 4326)

morton <- data.frame(site_name = "Morton Arboretum",
                     site_id = "MARB",
                     latitude = 41.8150,
                     longitude = -88.0658)
morton.sf <- sf::st_as_sf(morton, coords = c("longitude", "latitude"),
                       crs=4326) |>
  tibble::rowid_to_column("FID")
# files I think we need to tweak are in the 'gdalcubes-methods.R fils of the gefs4cast function.
gefs_to_parquet(dates = as.Date("2023-03-04"), ensemble = c(mean = "geavg", spr = "gespr"), "meow", sites=morton.sf)


df <- arrow::open_dataset("meow/")

meow <- dplyr::collect(df)
head(meow)

summary(meow$datetime)
summary(as.Date(meow$reference_datetime))

# getting unique sites.
unique(meow$site_id)

arb.dat <- meow[meow$site_id=="MARB",]

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
head(arb.dat)
dim(arb.dat)


# this has produced 3 hrly data for all of the variables that were in the primary GEFS data
# we will need to aggregate up from the 3hrly data to daily data
# need to determine the best way for that here.