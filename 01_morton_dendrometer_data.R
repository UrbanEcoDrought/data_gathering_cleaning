# organizing the TOMST dendrometer data from Luke
# Morton Arboretum Forestry plots

#path for data from shared google drive
data.path <- "G:/Shared drives/Urban Ecological Drought/Stem Diameter Variations and Drought/Morton Arb Dendrometer Data/TOMST point dendrometers"

# loading in the metadata from Luke's googlesheet
googlesheets4::gs4_auth("malexander@anl.gov")
meta.data <- googlesheets4::read_sheet(ss="17Po9WFMhxpzfGGLINy2--5C7-EsM88GJZ2qHPUGonu0", sheet="Serial number IDs")
head(meta.data)

# listing out all fo the files in the directory
group.files <- list.files(data.path)
group.files <- group.files[!group.files %in% "morton_dendrometer_cleaned.csv"]



# reading in raw dendrometer data and organizing into a single data frame
# outside for loop
mort.dendro.dat <- NULL

pb <- txtProgressBar(min=0, max=length(group.files), style=3)
pb.ind=1
for(i in group.files) {
  
  temp <- read.csv(file.path(data.path, i), sep=";", header=F)
  
  if(temp$V1[1]=="File is empty") next # had one empty file that was causing issues when reading in all data.
  
  names(temp) <- c("index", "date.time", "time.zone", "t1", "t2", "t3", "soil.moisture.count", "shake", "errFlag")
  temp <- temp[,c(1:9)] # some files have a mystery 10th variable that I am dropping because not all have this
  
  temp$id.num <- as.factor(unique(sapply(strsplit(i, "_"), `[`, 2))) # getting sensor ID number from filename to cross reference to meta data for species
  
  if(is.null(mort.dendro.dat)) mort.dendro.dat <- temp else mort.dendro.dat <- rbind(mort.dendro.dat, temp)
  
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}

summary(mort.dendro.dat)

# some measurements had a , as a whole vs decimal delimiter
# need to sub that out for t1 and t2

mort.dendro.dat$t1 <- as.numeric(gsub(",", ".", mort.dendro.dat$t1))
mort.dendro.dat$t2 <- as.numeric(gsub(",", ".", mort.dendro.dat$t2))

# Setting timezone as a factor
mort.dendro.dat$time.zone <- as.factor(mort.dendro.dat$time.zone)

# setting errFlag as a factor
mort.dendro.dat$errFlag <- as.factor(mort.dendro.dat$errFlag)

# setting date.time
mort.dendro.dat$date.time <- gsub("[.]", "-", mort.dendro.dat$date.time)

mort.dendro.dat$date.time <- lubridate::ymd_hm(mort.dendro.dat$date.time)
summary(mort.dendro.dat)

head(mort.dendro.dat)
mort.dendro.dat$date <- lubridate::date(mort.dendro.dat$date.time)


# matching id.num with plot ID to look at different species
head(meta.data)
meta.data$`Serial#` <- as.factor(meta.data$`Serial#`)
# making starting DBH a character for now
meta.data$`DBH (cm) at installation` <- as.character(meta.data$`DBH (cm) at installation`)

mort.dendro.dat$plotID <- meta.data$Plot[match(mort.dendro.dat$id.num, meta.data$`Serial#`)]
mort.dendro.dat$spp <- meta.data$Species[match(mort.dendro.dat$id.num, meta.data$`Serial#`)]


# have an issue where sensor ID 92210128 being uninstalled and re-installed, giving it 2 starting DBH's
# going to pull this sensor from the data set as we have others

mort.dendro.dat2 <- mort.dendro.dat[!mort.dendro.dat$id.num %in% "92210128",] 

# pulling in the starting dbh
mort.dendro.dat2$start.dbh.cm <- meta.data$`DBH (cm) at installation`[match(mort.dendro.dat2$id.num, meta.data$`Serial#`)]
head(mort.dendro.dat2)

# saving dendrometer band data.
saveRDS(mort.dendro.dat2, file = "processed_data/morton_dendrometer_cleaned.rds")
write.csv(mort.dendro.dat2, file.path(data.path, "morton_dendrometer_cleaned.csv"), row.names=F)


library(ggplot2)
ggplot(data=mort.dendro.dat[mort.dendro.dat$id.num=="92210128",]) +
  geom_line(aes(x=date.time, y=t1)) +
  scale_x_datetime(date_labels="%b-%Y", date_breaks="3 month")
