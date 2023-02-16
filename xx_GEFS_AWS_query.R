# Querying AWS for the GEFS data
library(aws.s3)
library(geojsonio)
library(arrow)

gefs <- "noaa-gefs-pds"

dats <- get_bucket_df(gefs)
save.path <- "processed_data/gefs"
today <- Sys.Date()
today.path <- gsub("-", "_", today)
dir.create(today.path)

download.file("s3:/",gefs, "noaa-gefs-pds/gefs.",gsub("-","",Sys.Date()),"/00/pgrb2a/")



pb <- txtProgressBar(min=0, max=length(grep(".csv", dats$Key)), style=3)
pb.ind=1

for(i in grep(".csv", dats$Key)){
  
  dat.now <- s3read_using(read.csv, object = file.path("s3:/", gefs, dats$Key[1]))
  
  dir.create(file.path(save.path, strsplit(dats$Key[i],"/")[[1]][1]))
  
  write.csv(dat.now, file.path(save.path, dats$Key[i]), row.names=F)
  
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}

