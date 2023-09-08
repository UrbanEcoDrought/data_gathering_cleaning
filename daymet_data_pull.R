# Downloading Daymet data for the morton VC.

library(daymetr)


lat <- 41.813613
lon <- -88.071797


temp.daymet <- download_daymet(
  lat = lat,
  lon = lon,
  start= 2021,
  end = 2023
)

str(temp.daymet)

daymet.df <- temp.daymet$data
head(daymet.df)

# calculating vpd from max temperature
daymet.df$tmax.rankine <- daymet.df$tmax..deg.c.*(9/5) + 491.67

A <- -1.0440397e4
B <- -11.29465
C <- -2.7022355e-2
D <- 1.289036e-5
E <- -2.4780681e-9 
FF <- 6.5459673
Temp <- daymet.df$tmax.rankine

daymet.df$vp.sat <- exp((A/Temp)+B+(C*Temp)+(D*Temp^2)+(E*Temp^3)+(FF*log(Temp)))*6.89476 # converts to kPA


daymet.df$vp.kpa <- daymet.df$vp..Pa./1000
head(daymet.df)
daymet.df$vpd.kpa <- daymet.df$vp.sat - daymet.df$vp.kpa

hist(daymet.df$vpd.kpa)

saveRDS(daymet.df, "processed_data/morton_daymet.RDS")
