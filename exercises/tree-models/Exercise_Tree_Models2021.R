# Tree Mased Models Exercise 

# setwd()
# getwd()
# .libPaths(c("C:/R/library", "C:/Program Files/R/R-3.5.1/library"))
# .libPaths()


# load require packages ----
library(ggplot2)  # graphing
library(sf)       # spatial data
library(mapview)  # maps
library(corrplot) # graphical display of correlation matrix
library(rpart)    # recursive partitioning
library(rpart.plot) # plots for part models
library(randomForest) # random forest
library(ranger)   # faster random forest
library(caret)    # classification and regression trees
library(ipred)    # resampled error predictions
library(caTools)  # utility fxns and model eval
library(raster)   # raster data analysis tools



# Part 1 - Explore data ----
## Get Data from West Virginia and plot ----
url <- 'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/wv_transect_editedforR.csv'
download.file(url, "soildata.csv")
soildata <- read.csv("soildata.csv", header=TRUE, sep = ",")

soildata <- st_as_sf(
  soildata,
  # set the coordinates
  coords = c("x", "y"),
  # set the projection
  crs = 4326
)

# plot points
mapview(soildata)

# Write a shapefile in your working directory for viewing in GIS
write_sf(soildata, dsn = "soildata.shp") 

# Create a map of the data with a GIS


## EDA for WV dataset ----
# view the data
View(soildata)

# examine the internal data structure
soildata <- read.csv("soildata.csv", header = TRUE, sep = ",")
str(soildata) 

soildata$spodint  <- as.factor(soildata$spodint)
soildata$spodint  <- ordered(soildata$spodint)
soildata$tipmound <- as.factor(soildata$tipmound)
soildata$tipmound <- ordered(soildata$tipmound)


# does solar radiation affect spodic intensity?
ggplot(soildata, aes(x = spodint, y = solar)) +
  geom_boxplot() +
  xlab("spodic intensity") + ylab("solar")


# how about aspect?
ggplot(soildata, aes(x = spodint, y = northwestn)) +
  geom_boxplot() +
  xlab("spodic intensity") + ylab("northwestness")


# distribution of O horizon thickness among soil orders
ggplot(soildata, aes(x = Otot)) +
  geom_density() +
  facet_wrap(~ order)


# combine numeric columns into a new data frame
numeric <- soildata |>
  dplyr::select(where(is.numeric)) 
names(numeric) 


# calculate correlation matrix
cormatrix <- cor(numeric) 

# plot correlation matrix
corrplot(cormatrix, method = "circle")


## Questions ----
# 1. What does the correlation matrix tell you about the enironmental convariate data?
# 2. Which predictors are highly correlated?



# Part 2 - Build a regression tree model, prune using cp, compare model accuracy automate the CP step further?? ----

idx <- createDataPartition(soildata$depth_cm, p = 0.85, list = FALSE)
val <- soildata[idx * -1, ]
cal <- soildata[idx, ]
nrow(cal)
nrow(val)


# build a regression tree model for soil depth
tree_reg_model <- rpart(depth_cm ~ rainfall + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = cal, method = "anova")


# examine model outputs
tree_reg_model


# plot the regression tree model
par(mar = c(3,6,3,6)) 
plot(tree_reg_model)
text(tree_reg_model, cex=0.85)

prp(tree_reg_model, type = 1, extra = 1, branch = 1)


# inspect the CP values
print(tree_reg_model$cptable)
plotcp(tree_reg_model)


# prune model based on the best CP value
tree_prune <- prune(tree_reg_model, cp = 0.08561859)

printcp(tree_prune)
prp(tree_prune, type = 1, extra = 1, branch = 1)


# predict soil depth for the validation dataset using the regression tree model
class_prediction <- predict(object = tree_reg_model, newdata = val)  


# Compute the RMSE
RMSE(pred = class_prediction, obs = val$depth_cm)


# predict soil depth for the validation dataset using the pruned regression tree model
class_prediction <- predict(object = tree_prune, newdata = val)  


# Compute the RMSE
RMSE(pred = class_prediction, obs = val$depth_cm)



# Part 3 - Build a classification tree model and evaluate model accuracy ----

# import training data 
soildata <- read.csv("soildata.csv", header=TRUE, sep=",")

# wrangle the training data
soildata$spodint  <- as.factor(soildata$spodint)
soildata$spodint  <- ordered(soildata$spodint)
soildata$tipmound <- as.factor(soildata$tipmound)
soildata$tipmound <- ordered(soildata$tipmound)


# index for lookup table
index <- c(0, 0.5, 1, 1.5, 2) 

# assigning corresponding categories to look up values
values <- c("nonspodic", "nonspodic", "spodic", "spodic", "spodic") 

# match spodint to index and assign values
soildata$spo_index <- values[match(soildata$spodint, index)] 

# convert new column from character to factor
soildata$spo_index <- as.factor(soildata$spo_index) 

# Splitting into cal/val with 15% split
idx <- createDataPartition(soildata$spo_index, p = 0.85, list = FALSE)
val <- soildata[idx * -1, ]
cal <- soildata[idx, ]
nrow(cal)
nrow(val)


# build a classification tree model using Gini index
tree_class_model_gini <- rpart(soildata$spo_index ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = soildata, method = "class", parms = list(split = "gini"))


# inspect model outputs
tree_class_model_gini

plot(tree_class_model_gini)
text(tree_class_model_gini, cex=0.8) #cex is text size

par(mar = c(3,6,3,6)) 
plot(tree_class_model_gini)
text(tree_class_model_gini, cex = 0.6)

rpart.plot(tree_class_model_gini, extra = 3)
rpart.plot(tree_class_model_gini, extra = 103)


# predict landcover for the validation dataset
tree_pred_gini <- predict(tree_class_model_gini, newdata = val, type = "class", datatype = "INT1U", progress = "text")


# Evaluate model results using a confusion matrix
results <- data.frame(val$spo_index, tree_pred_gini)
cm1 <- confusionMatrix(val$spo_index, tree_pred_gini, positive = NULL, prevalence = NULL, mode = "everything")
cm1
print(cm1$overall[1])


# import training data - Eval by Observed Class
soildata <- read.csv("soildata.csv", header=TRUE, sep=",")


# wrangle the training data
soildata$spodint <- as.factor(soildata$spodint)
soildata$spodint <- ordered(soildata$spodint)
soildata$tipmound <- as.factor(soildata$tipmound)
soildata$tipmound <- ordered(soildata$tipmound)


# index for lookup table
index <- c(0, 0.5, 1, 1.5, 2)

# assigning corresponding categories to look up values
values <- c("nonspodic", "nonspodic", "spodic", "spodic", "spodic") 

# match spodint to index and assign values
soildata$spo_index <- values[match(soildata$spodint, index)] 

# convert new column from character to factor
soildata$spo_index <- as.factor(soildata$spo_index) 


# Splitting into cal/val with 15% split
idx <- createDataParition(soildata$spo_index, p = 0.85, list = FALSE)
val <- soildata[idx * -1, ]
cal <- soildata[idx, ]
nrow(cal)
nrow(val)


# build a classification tree model using Information index
tree_class_model_info <- rpart(soildata$spo_index ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = soildata, method = "class", parms = list(split = "information"))


# inspect model outputs
tree_class_model_info


# predict landcover for the validation dataset
tree_pred_info <- predict(tree_class_model_info, newdata = val, type = "class", datatype = "INT1U", progress = "text")


# Evaluate model results using a confusion matrix
results <- data.frame(val$spo_index, tree_pred_info)
cm2 <- confusionMatrix(val$spo_index, tree_pred_info, positive = NULL, prevalence = NULL, mode = "everything")
cm2
cm2$overall[1]


## Question ----
# which modeling method (gini or information) had a higher overall accuracy? kappa?



# Part 4 - Build a Random Forest Model for classification with parameter tuning using gradient descent ----

# download csv from github
url <- 'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/archive/chapters/8_Tree_models/IAD16_train.csv'
download.file(url, "LCdata.csv")


# import and wrangle training data
LCtrain <- read.csv("LCdata.csv", header=TRUE)
names(LCtrain)
train <- LCtrain[, c(3:7,10)]
str(train)
train$Observed <- as.factor(train$Observed)
table(train$Observed)

## build RF model from training data
rf <- randomForest(as.factor(Observed) ~ ., data = train, ntree = 500, mtry = 2, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
rf

# Grab OOB error matrix & take a look
err <- rf$err.rate
head(err)

# examine model outputs
plot(rf)
legend(x = "right", legend = colnames(err), fill = 1:ncol(err))

#plot variable importance
varImpPlot(rf)

# Look at final OOB error rate (last row in err matrix)
oob_err <- err[nrow(err), "OOB"]
print(oob_err)


## Questions ----
# 1. What is the OOB Error
# 2. What was the predictor with the greatest variable importance?
# 3. What random forest parameters were used? ntrree? mtry?


# Establish a list of possible values for mtry, nodesize and ntree
mtry <- seq(2, 4, 1)
nodesize <- seq(1, 8, 2)
ntree <- seq(200, 800, 100)


# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, ntree = ntree)

# Create an empty vector to store OOB error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = Observed ~ ., 
                        data = train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        ntree = hyper_grid$ntree[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}


# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])


# Use this info to build a second random forest model and compare OOB 
# complete the code in the line below with the values from your grid search
rf.opt <- randomForest(Observed ~ ., data = train, ntree = 100, mtry = 1, nodesize= 1, importance = TRUE, proximity = TRUE, keep.forest = TRUE)

rf
rf.opt


## Questions ----
# 1. How many models were testing using the gradient descent?
# 2. What are the optimal parameters for ntree, mtry and nodesize?
# 3. What was the net result on our out of bag error?



# Part 5 - fast Random Forest with ranger ----

# using system time for benchmarking to compare model speed
# benchmark the random forest modeling process
system.time({
  rf.opt <- randomForest(Observed ~ ., data=train, ntree = 500, mtry = 4, nodesize= 5, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
  })

# benchmark the ranger implementation of the random forest modeling process
system.time({
  ranger.rf <- ranger(Observed ~ ., data=train)
})

# benchmark the random forest modeling process with gradient descent
system.time({for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = Observed ~ ., 
                        data = train,
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        ntree = hyper_grid$ntree[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}})


# benchmark the ranger implementation random forest modeling process with gradient descent

# Establish a list of possible values for mtry, nodesize and ntree
mtry <- seq(2, 4, 1)
min.node.size <- seq(1, 8, 2)
num.trees <- seq(100, 500, 100)


# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, min.node.size = min.node.size, num.trees = num.trees)


# Create an empty vector to store OOB error values
oob_err <- c()

system.time({
  for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- ranger(formula = Observed ~ ., 
                        data = train,
                        mtry = hyper_grid$mtry[i],
                        min.node.size = hyper_grid$min.node.size[i],
                        num.trees = hyper_grid$num.trees[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}
  })


## Questions ----
# 1. HOW MUCH FASTER IS THE RANGER IMPLEMENTATION for modeling??
# 2. HOW MUCH FASTER IS THE RANGER IMPLEMENTATION for tuning with gradient descent??

# Part 6 - Predict using random forest model to map landcover from Imagery ----

# get Sample Image from github and unzip
url <- 'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/archive/chapters/8_Tree_models/NAIP_5m.zip'
download.file(url, "NAIP_5m.zip")
unzip("NAIP_5m.zip")


# read and inspect imagery
x1 <- brick("m_3807706_nw_5m.tif")
names(x1)


# build a function to calculate NDVI
vi <- function(img, i, k){
  bi <- img[[i]]
  bk <- img[[k]]
  vi <- (bk-bi)/(bk+bi)
  return (vi)
}

# run NDVI with the new function and plot
ndvi1 <- vi(x1,1,4)
plot(ndvi1)


# Add NDVI back to the original image to create a 5-band image for predicting landcover
x <- stack(x1, ndvi1)
names(x) <- c('red', 'green', 'blue', 'nir', 'ndvi')


# predict landcover for the image using the optimum random forest model from part 5
LC.class <- predict(x, rf.opt, type = "response", datatype = "INT1U", progress = "text")


# plot output landcover map and write output to disc
plot(LC.class)
writeRaster(LC.class, "LC_Class_map.tif", overwrite = T, progress = "text")


# generate a probability surface for the predictions
LC.prob <- predict(x, rf.opt, type = "prob", datatype = "INT1U", options = c("COMPRESS=LZW"), progress = "text")


# plot the surface of probabilities
plot(LC.prob)


# pull out the raster attribute table for ArcGIS
attrib <- as.data.frame(LC.class@data@attributes)
colnames(attrib) <- c("Value", "Class")


# view the RAT
attrib


# write output table
write.csv(attrib, "LC.class_rat.csv")


# Question -----
# 1. Use the data to make a landcover map and attach a screenshot to the project worksheet
