# walking through the SPI, SPEI, and EDDI, and VPD data that Trent staged in the GoogleDrive
library(ggplot2)
library(forecast)
# loading in .csv

dat <- read.csv("input_data/Morton_Drought_Datasets - Morton_Drought_Datasets.csv", header=T, na.strings = "NaN")

head(dat)

names(dat) <- tolower(names(dat))

dat$date <- as.Date(dat$date)
summary(dat)

dat.stack <- stack(dat[,!names(dat) %in% "date"])
head(dat.stack)

names(dat.stack) <- c("values", "index")
dat.stack$date <- dat$date

dat.stack.mean <- data.frame(names(dat)[!names(dat) %in% "date"])
names(dat.stack.mean) <- "index"

for(i in unique(dat.stack.mean$index)){
  temp <- dat[,names(dat) %in% i]
  
  dat.stack[dat.stack$index==i, "mean.value"] <- mean(temp, na.rm=T)
}

head(dat.stack)

# exploratory plots
ggplot(data=dat.stack) + facet_grid(index~.) +
  # geom_hline(yintercept=0, linetype="dashed") +
  geom_line(aes(x=date, y = values, col=index, group=index)) +
  geom_line(aes(x=date, y=mean.value, group=index), col="black", linetype="dashed")




# some stats on the various indices

# correlation analysis just to see who aligns with whom
dat.cor <- cor(dat[,!names(dat) %in% "date"], use="complete.obs") # using complete obs becasue dec 2022 has some missing data
dat.cor

# covariance
dat.cov <- cov(dat[,!names(dat) %in% "date"], use="complete.obs")
dat.cov


# Autocorrelation Analysis
acf(dat[,2], plot=T, demean=T)
acf(dat[,3], plot=T, demean=T)
acf(dat[,4], plot=T, demean=T)
acf(dat[,5], plot=T, demean=T) # missing data
acf(dat[,6], plot=T, demean=T) # missing data
acf(dat[,7], plot=T, demean=T)
acf(dat[,8], plot=T, demean=T) # missing data
acf(dat[,9], plot=T, demean=T)
acf(dat[,10], plot=T, demean=T)
acf(dat[,11], plot=T, demean=T)



library(lubridate)


# making a month variable and a year variable
dat$year <- year(dat$date)
dat$month <- month(dat$date)
dat$month.name <- month(dat$date, label= T, abbr=T)
head(dat)

###########################
# VPD index calculation----

vpd.dat <- dat[,c("date", "year", "month", "vpd..kpa.")]
head(vpd.dat)


vpd.1990.2010 <- vpd.dat[vpd.dat$date >= "1990-01-01" & vpd.dat$date <="2010-12-31",]

vpd.1990.2010.monthly <- aggregate(vpd..kpa. ~ month, data=vpd.1990.2010, FUN="mean")
vpd.1990.2010.monthly$sd <- aggregate(vpd..kpa. ~ month, data=vpd.1990.2010, FUN="sd")$vpd..kpa
head(vpd.1990.2010.monthly)
names(vpd.1990.2010.monthly) <- c("month", "vpd.month.index.mean", "vpd.month.index.sd")

head(vpd.dat)

vpd.dat2 <- merge(vpd.dat, vpd.1990.2010.monthly)
head(vpd.dat2)

vpd.dat2$vpd.index.value <- (vpd.dat2$vpd..kpa.-vpd.dat2$vpd.month.index.mean)/vpd.dat2$vpd.month.index.sd
head(vpd.dat2)

vpd.dat2.short <- vpd.dat2[,c("date", "vpd.index.value")]

dat <- merge(dat,vpd.dat2.short)
head(dat)

write.csv(dat, "processed_data/Morton_Drought_Datasets - Morton_Drought_Datasets_mra.csv", row.names=F)
#######################
# Monthly Average Calcs----
# making monthly averages of each index and identifying a 'normal' as the interquartile range
# can make plots to show when years fall outside of the 'normal'


# Creating monthly aggregations
dat.month <- NULL

index.list <- names(dat)[!names(dat) %in% c("month", "year", "month.name", "date")]

for(i in index.list){
  temp.mean <- aggregate(dat[,i] ~ month + year, data=dat, FUN="mean")
  names(temp.mean) <- c("month", "year", "mean.month")
  temp.ub <- aggregate(dat[,i] ~ month, data=dat, FUN="quantile", prob=0.75)
  names(temp.ub) <- c("month", "ub.75")
  temp.lb <- aggregate(dat[,i] ~ month, data=dat, FUN="quantile", prob=0.25)
  names(temp.lb) <- c("month", "lb.25")
  
  temp.df <- temp.mean
  temp.df2<- merge(temp.mean, temp.ub)
  temp.df3 <- merge(temp.df2, temp.lb)
  temp.df3$index <- as.factor(i)
  
 if(is.null(dat.month)) dat.month <- temp.df3 else dat.month <- rbind(dat.month, temp.df3) 
}

summary(dat.month)

dat.month$month.name <- month(dat.month$month, label=T, abbr=T)
dat.month$month <- month(dat.month$month)

head(dat.month)

# making some seasonal plots

diag <- ggplot(data=dat.month[dat.month$year >= 2000,]) + facet_wrap(index~.) +
  #geom_ribbon(aes(x=month, ymin=lb, ymax=ub, group=index), col="grey50")+
  geom_ribbon(aes(x=month.name, ymin= lb.25, ymax=ub.75, group=as.factor(year)), fill="grey80") +
  geom_line(aes(x=month.name, y=mean.month, col=as.factor(year), group=as.factor(year)))+
  theme_bw()

png(filename="figures/drought_index_inner_quartile_diagnostics.png", height=8, width = 15, res=300, unit="in")
  diag
dev.off()


# saving the stacked dataframe for others to use
write.csv(dat.month, file = "processed_data/morton_stacked_drought_indices.csv", row.names=F)

