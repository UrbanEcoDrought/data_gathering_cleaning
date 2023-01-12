# code for unpacking the .json file from the ECOSTRESS point run

library(httr)
library(jsonlite)
library(readxl)
library(ggplot2)


dat <- read.csv("input_data/test/API-Test-ECO2LSTE-001-results.csv", header=T)

summary(dat)

dat.short <- dat[,c("Date", "ECO2LSTE_001_SDS_LST", "ECO2LSTE_001_SDS_LST_err")]
names(dat.short) <- c("date.time", "lst_K", "lst_err_K")

dat.short$date <- as.Date(substr(dat.short$date.time, 1, 10), format="%Y-%m-%d")
dat.short$lst_F <- ((dat.short$lst_K - 273.15) * 9/5 + 32)


ggplot(data=dat.short) +
  geom_ribbon(aes(x=date, ymin=lst_K-lst_err_K, ymax=lst_K+lst_err_K, fill="Error"), alpha=0.25) +
  geom_line(aes(x=date, y=lst_K), size=1.25) +
  scale_fill_manual(values=c("Error" ="#E69F00")) +
  labs(x="Date", y="LST (K)")+
  theme_bw() +
  scale_x_date(date_labels = "%m-%d-%Y")
