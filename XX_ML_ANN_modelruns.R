# Building Artificial neural network model to predict NDVI from climate and lag across space

library(neuralnet)
library(dplyr)
library(caret)
library(terra)
library(ggplot2)
library(ncdf4)
library(lubridate)
library(CAST)

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
# Specifying Paths----
google.drive <-  "G:/Shared drives/Urban Ecological Drought"

# loading in NDVI data----

#ndvi.dat <- readRDS("processed_data/landsat_NDVI_spatial.RDS") # data also on Drought google drive but loaded Ross's local copy for speed
ndvi.dat <- readRDS(file.path(google.drive,"data/r_files/processed_files/landsat_NDVI_spatial.RDS"))
head(ndvi.dat)
summary(ndvi.dat)

ndvi.dat.clean <- ndvi.dat[!is.na(ndvi.dat$ndvi_value),]
summary(ndvi.dat.clean)

# loading in climate data----
clim.dat <- readRDS("processed_data/climate_spatial.RDS") # climate and landsat data should be in the same projection
head(clim.dat)

# removing the 90day variables as they ahve too many missing values
clim.dat2 <- clim.dat[,!names(clim.dat) %in% names(clim.dat)[grep("90", names(clim.dat))]]
summary(clim.dat2)



# merging the datasets in together
dim(ndvi.dat.clean)
dim(clim.dat2)

chi.dat <- merge(ndvi.dat.clean, clim.dat2, by=c("date", "x", "y"))
dim(chi.dat)
summary(chi.dat)

chi.dat$week.num <- lubridate::week(chi.dat$date)

# parsing down data to a clean set with no missing values
# chi.dat2 <- chi.dat[!is.na(chi.dat$tmin14), !names(chi.dat) %in% "ndvi.lag21d"]
chi.dat2 <- chi.dat[complete.cases(chi.dat),]

# chi.dat1.5 <- chi.dat[,!names(chi.dat) %in% "ndvi.lag14d"]
# chi.dat2 <- chi.dat1.5[complete.cases(chi.dat1.5),]

summary(chi.dat2)

dim(chi.dat2)

# have satellite as a factor which the ann will not like, so making it a numeric for now
class(chi.dat2$landsat)
chi.dat2$landsat <- as.factor(chi.dat2$landsat)
# chi.dat2$landsat <- as.factor(chi.dat2$landsat)
chi.dat2$landsat.num <- as.numeric(stringr::str_sub(as.character(chi.dat2$landsat),-1,-1))

# limiting to march-oct
chi.dat3 <- chi.dat2[!chi.dat2$month %in% c(1,2,11,12),]
head(chi.dat3)
# adding in a week variable to see if that helps with model stablility


# ANN Model----
# splitting into training and validation data sets
set.seed(08082024)
train.index <- createDataPartition(chi.dat3$ndvi_value, p=0.8, list=F)
#train.index <- sample(1:nrow(chi.dat3), size=(nrow(chi.dat3)*0.8), replace=F)

train.dat <- chi.dat3[train.index[,1],]
test.dat <- chi.dat3[-train.index[,1],]

# setting the formula for the NN
# ran the RF model and bulling out landsat as it was not a significant variable.
names(chi.dat3)
formula <- ndvi_value ~ week.num + ndvi.lag21d + spi14day + spi30day + spi60day + spei14day + spei30day + spei60day +tmax14 + tmax30 + tmax60 + tmin14 + tmin30 + tmin60

# formula <- ndvi_value ~ week.num + x + y + ndvi.lag21d
neural_net <- neuralnet(formula, data = train.dat, hidden = c(5,3), linear.output = T)

#------------------------------
# Cast Predictive Model----
# need to set the model up with rasters for the predictors

summary(chi.dat3)

formula <- ndvi_value ~ week.num + ndvi.lag21d + xy.coord + spi14day + spi30day + spi60day + 
  spei14day + spei30day + spei60day + tmax14 + tmax30 + tmax60 + 
  tmin14 + tmin30 + tmin60

# Spatial or temporal resampling, e.g., spatial leave-location-out cross-validation (SLOO)
folds <- CreateSpacetimeFolds(x=chi.dat, spacevar = "xy.coord", k=5)

# Set up control for train function
train_control <- trainControl(method = "cv", index = folds$index, indexOut = folds$indexOut)

model <- train(formula, 
               data = chi.dat, 
               method = "ranger",  # Random Forest algorithm
               trControl = train_control,
               ntrees=100,
               na.action="na.omit")

# Print the results
print(model)
saveRDS(model, "processed_data/cast_model.RDS")
# Print the results and check performance metrics
print(model$results)

# Predict NDVI on new data or using the same data
chi.dat3$cast.pred <- predict(model, newdata = chi.dat3)

CAST::global_validation(model)

ggplot(data=chi.dat3) +
  geom_point(aes(x=ndvi_value, y=cast.pred))

ggplot(data=chi.dat3) + facet_wrap(year~.) +
  geom_point(aes(x=doy, y=ndvi_value), col="black") +
  geom_point(aes(x=doy, y=cast.pred), col="orange2")


# Compare predicted vs actual NDVI values
head(predictions)


# Random Forest run----
#Play with NDVI predictions with random forest
#By Lindsay Darling

#Load libraries-----------

library(tidyverse)
library(tidylog)
library(randomForest)  #Random forests
library(units)         #Change units in spatial work
library(pdp)           #Partial dependence plots
library(vip)           #Variable importance plots
library(rpart)
library(rpart.plot)


vars.rf <- c("week.num", "ndvi_value", "ndvi.lag21d", "landsat", "xy.coord", "spei14day",
             "spei30day", "spei60day", "spi14day", "spi30day", "spi60day", "tmax14", "tmax30",
             "tmax60", "tmin14", "tmin30", "tmin60")
set.seed(123)
randoAll<-randomForest(ndvi_value~., data=train.dat[,names(train.dat) %in% vars.rf], mtry = 6, ntree = 500)
randoAll #90% variance explained

# saving model because it took 18hrs to run
saveRDS(randoAll, file="processed_data/randomForestmodel.RDS")

AllVIP<- vip(randoAll, include_type = TRUE, horizontal = TRUE, 
             aesthetics = list(fill = '#2a2e38'), num_features = 17) +
  theme_bw() +
  theme(axis.title.x = element_blank())

AllVIP
