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
NDVIsave <- "UrbanEcoDrought_NDVI_LocalExtract"
pathShare <- file.path(path.google, "Shared drives/Urban Ecological Drought/data/UrbanEcoDrought_NDVI_LocalExtract/")


fNDVI <- dir(file.path(path.google, NDVIsave))


day.labels <- data.frame(Date=seq.Date(as.Date("2023-01-01"), as.Date("2023-12-01"), by="month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label=T), lubridate::day(day.labels2$Date))
day.labels
summary(day.labels)

# Clunky code, but should pull the latest file
lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

ndviAll <- data.frame()
for(LCTYPE in lcnames){
  landsat8 <- read.csv(file.path(path.google, NDVIsave, dir(file.path(path.google, NDVIsave), paste0("Landsat8_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat8_", LCTYPE)))]))
  landsat9 <- read.csv(file.path(path.google, NDVIsave, dir(file.path(path.google, NDVIsave), paste0("Landsat9_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat9_", LCTYPE)))]))
  landsat7 <- read.csv(file.path(path.google, NDVIsave, dir(file.path(path.google, NDVIsave), paste0("Landsat7_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat7_", LCTYPE)))]))
  landsat5 <- read.csv(file.path(path.google, NDVIsave, dir(file.path(path.google, NDVIsave), paste0("Landsat5_", LCTYPE))[length(dir(file.path(path.google, NDVIsave), paste0("Landsat5_", LCTYPE)))]))
  
  landsat8$mission <- "landsat 8"
  landsat9$mission <- "landsat 9"
  landsat7$mission <- "landsat 7"
  landsat5$mission <- "landsat 5"
  
  landsatAll <- rbind(landsat8, landsat9, landsat7, landsat5)
  # landsatAll <- rbind(landsat8, landsat9)
  landsatAll$type <- LCTYPE
  
  ndviAll <- rbind(ndviAll, landsatAll)
}
summary(ndviAll)
unique(ndviAll$mission)

ndviAll$date <- as.Date(ndviAll$date)
ndviAll$year <- lubridate::year(ndviAll$date)
ndviAll$yday <- lubridate::yday(ndviAll$date)
ndviAll$type <- factor(ndviAll$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-medium", "urban-high")))
head(ndviAll)
summary(ndviAll)
unique(ndviAll$type)

png("~/Google Drive/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/NDVI_Landsat_NLCD_latest.png", height=8, width=11, units="in", res=320)
ggplot(data=ndviAll[,], aes(x=yday, y=NDVI)) +
  ggtitle(paste0("Landsat 5,7,8,9 NDVI, last image: ", max(ndviAll$date))) +
  facet_wrap(~type) +
  # stat_smooth(color="black", fill=NA, size=0.5) +
  geom_line(aes(group=year, color="historical"), alpha=0.5, size=0.1)+
  geom_line(data=ndviAll[ndviAll$year==2005, ], aes(color="2005"), size=0.25) +
  geom_line(data=ndviAll[ndviAll$year==2012, ], aes(color="2012"), size=0.25) +
  geom_line(data=ndviAll[ndviAll$year==2023, ], aes(color="2023"), size=0.5) +
  stat_smooth(data=ndviAll[!ndviAll$year %in% c(2005, 2012, 2023), ],  aes(color="historical", fill="historical"), size=1.5, alpha=0.5, method="gam") +
  # stat_smooth(data=ndviAll[ndviAll$yday<=max(ndviAll$yday[ndviAll$year==2023]) & ndviAll$year!=2023, ], aes(color="historical", fill="historical"), size=1.5, alpha=0.8) +
  stat_smooth(data=ndviAll[ndviAll$year==2005, ], aes(color="2005", fill="2005"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndviAll[ndviAll$year==2012, ], aes(color="2012", fill="2012"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndviAll[ndviAll$year==2023, ], aes(color="2023", fill="2023"), size=1.25, alpha=0.5, method="gam") +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])  +
  scale_color_manual(values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  scale_fill_manual(values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Day of Year")  +
  guides(fill=F) +
  theme_bw()
dev.off()
 

png("~/Google Drive/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/NDVI_Landsat_NLCD_latest_NoGrassland.png", height=6, width=12, units="in", res=320)
ggplot(data=ndviAll[ndviAll$type!="grassland",], aes(x=yday, y=NDVI)) +
  # ggtitle(paste0("Landsat 5,7,8,9 NDVI, last image: ", max(ndviAll$date))) +
  facet_wrap(~type) +
  # stat_smooth(color="black", fill=NA, size=0.5) +
  geom_line(aes(group=year, color="historical"), alpha=0.5, size=0.1)+
  geom_line(data=ndviAll[ndviAll$year==2005 & ndviAll$type!="grassland", ], aes(color="2005"), size=0.25) +
  geom_line(data=ndviAll[ndviAll$year==2012 & ndviAll$type!="grassland", ], aes(color="2012"), size=0.25) +
  geom_line(data=ndviAll[ndviAll$year==2023 & ndviAll$type!="grassland", ], aes(color="2023"), size=0.5) +
  stat_smooth(data=ndviAll[!ndviAll$year %in% c(2005, 2012, 2023) & ndviAll$type!="grassland", ],  aes(color="historical", fill="historical"), size=1.5, alpha=0.5, method="gam") +
  # stat_smooth(data=ndviAll[ndviAll$yday<=max(ndviAll$yday[ndviAll$year==2023]) & ndviAll$year!=2023, ], aes(color="historical", fill="historical"), size=1.5, alpha=0.8) +
  stat_smooth(data=ndviAll[ndviAll$year==2005 & ndviAll$type!="grassland", ], aes(color="2005", fill="2005"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndviAll[ndviAll$year==2012 & ndviAll$type!="grassland", ], aes(color="2012", fill="2012"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndviAll[ndviAll$year==2023 & ndviAll$type!="grassland", ], aes(color="2023", fill="2023"), size=1.25, alpha=0.5, method="gam") +
  scale_x_continuous(name="Day of Year", expand=c(0,0), breaks=day.labels$yday[seq(2, 12, by=3)], labels=day.labels$Text[seq(2, 12, by=3)])  +
  scale_color_manual(name="year", values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  scale_fill_manual(name = "year", values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Day of Year")  +
  guides(fill=F) +
  theme_bw() + 
  theme(strip.text = element_text(size=rel(1.25), face="bold"))

dev.off()

