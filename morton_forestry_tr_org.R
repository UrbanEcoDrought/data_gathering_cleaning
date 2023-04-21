# Morton Forestry plot Tree ring organization
library(dplR)

# reading in tree-ring data
files <- list.files("input_data/morton_forestry_plot_tr_data/")
combo.files <- files[grep("Combined",files)]


fp.chrons <- NULL

for(i in combo.files){
temp <- read.tucson(paste0("input_data/morton_forestry_plot_tr_data/", i))

detrend <- detrend(temp, method="Spline")
temp.chron <- chron(detrend, biweight = T)

temp.chron2 <- data.frame(year=row.names(temp.chron),
                           temp.name = temp.chron$std)

chron.name <- sapply(strsplit(i, "_"), `[`, 1)
names(temp.chron2) <- c("year", chron.name)

if(is.null(fp.chrons)) fp.chrons <- temp.chron2 else fp.chrons <- merge(fp.chrons, temp.chron2, by="year", all=T)
}

# some flags where the splines didn't necessarily works well. For any exploratory analysis 
head(fp.chrons)


write.csv(fp.chrons, "processed_data/morton_forestry_two_thirds_chrons.csv", row.names=F)
