library(lattice) #graphing
library(sp) #spatial data
library(maps) #maps
library(rgdal) #spatial import
library(corrplot) #graphical display of correlation matrix

file <-'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/wv_transect_editedforR.csv'
download.file(file, destfile = "soildata.csv")
soildata <- read.csv("soildata.csv", header=TRUE, sep=",")
coordinates(soildata) <- ~ x + y #set the coordinates; converting dataframe to a spatial object
proj4string(soildata) <- CRS("+init=EPSG:4269") #set the projection; https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf 

map("county", "west virginia") 
points(soildata) #plot points


## #convert soildata into a shapefile
## writeOGR(soildata, dsn = "C:/workspace", "soildata", driver = "ESRI Shapefile")
## 

#since we converted the soildata dataframe to a spatial object to export as a shapefile, we will need to convert it back to a dataframe to plot and further examine the data in R

#reimporting the data and overwriting the soildata object is just one way to achieve this
file <-'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/wv_transect_editedforR.csv'
download.file(file, destfile = "soildata.csv")
soildata <- read.csv("soildata.csv", header=TRUE, sep=",")
View(soildata) #view the data
str(soildata) #examine the internal data structure


set.seed(250)
soildata$spodint <- as.factor(soildata$spodint)
soildata$spodint <- ordered(soildata$spodint)
soildata$tipmound <- as.factor(soildata$tipmound)
soildata$tipmound <- ordered(soildata$tipmound)


boxplot(solar~spodint, data=soildata, xlab="spodic intensity", ylab="solar") #does solar radiation affect spodic intensity?
boxplot(northwestn~spodint, data=soildata, xlab="spodic intensity", ylab="northwestness") #how about aspect?
densityplot(~ Otot|order, data=soildata) #distribution of O horizon thickness among soil orders
numeric <- data.frame(soildata[, c(8, 25, 27:50)]) #combine numeric columns into a new data frame
names(numeric) 
cormatrix <- cor(numeric) #calculate correlation matrix
corrplot(cormatrix, method = "circle") #plot correlation matrix


library(rpart) #CART models
library(randomForest) #random forest
library(rpart.plot) #rpart plot graphics
library(caret) #confusion matrix

spodintmodel <- rpart(spodint ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = soildata, method = "class")

spodintmodel

plot(spodintmodel)
text(spodintmodel, cex=0.8) #cex is text size


## #if you are having trouble viewing the text in the plot window, click zoom to open a bigger window
## #you may also need to adjust the plot margins or text size; for this example, try:
## par(mar=c(3,6,3,6))
## plot(spodintmodel)
## text(spodintmodel, cex=0.6)

rpart.plot(spodintmodel, extra=3) #extra=3 displays the misclassification rate at the node, expressed as the number of incorrect classifications divided by the total observations in the node; there are many options under the extra setting for classification models

rpart.plot(spodintmodel, extra=103) #adding 100 to the extra setting displays the percentage observations in the node

prp(spodintmodel,type=1,extra=1,branch=1) #prp is another function in the rpart.plot package that has numerous plot customization options


index <- c(0, 0.5, 1, 1.5, 2) #index for lookup table
values <- c("nonspodic", "nonspodic", "spodic", "spodic", "spodic") #assigning corresponding categories to look up values
soildata$newcolumn <- values[match(soildata$spodint, index)] #match spodint to index and assign values
soildata$newcolumn <- as.factor(soildata$newcolumn) #convert new column from character to factor

spodintmodel2 <- rpart(newcolumn ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = soildata, method = "class")

spodintmodel2

plot(spodintmodel2)
text(spodintmodel2, cex=0.8)


printcp(spodintmodel)
printcp(spodintmodel2)


plotcp(spodintmodel)
plotcp(spodintmodel2)


pruned <- prune(spodintmodel, cp=0.029321)
printcp(pruned)
rpart.plot(pruned, extra=3)
pruned2 <- prune(spodintmodel2, cp=0.050459)
printcp(pruned2)
rpart.plot(pruned2, extra=3)


## splits 70% of the data selected randomly into training set and the remaining 30% sample into test set
datasplit <- sort(sample(nrow(soildata), nrow(soildata)*.7)) 
train <- soildata[datasplit,]
test <- soildata[-datasplit,]

spodintmodel <- rpart(spodint ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = train, method = "class")
printcp(spodintmodel)
pruned <- prune(spodintmodel, cp=0.070175)
pred <- predict(pruned, newdata=test, type="class") #predicting class test data using the pruned model
confusionMatrix(pred, test$spodint) #computes confusion matrix and summary statistics
#sensitivity=producer's accuracy and specificity=user's accuracy

spodintmodel2 <- rpart(newcolumn ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = train, method = "class")
printcp(spodintmodel2)
pruned2 <- prune(spodintmodel2, cp=0.050459)
pred2 <- predict(pruned2, newdata=test, type="class") #predicting class of test data using the pruned model
confusionMatrix(pred2, test$newcolumn) #computes confusion matrix and summary statistics


rf <- randomForest(Otot ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = soildata, importance=TRUE, ntree=1000, mtry=10) #importance=TRUE will allow the generation of a variable importance plot

rf #statistical summary

plot(rf)  #out of bag (OOB) error rate versus number of trees; this will help us tune the ntree parameter


hist(soildata$Otot)


file <- 'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/wv_transect_editedforR.csv'
download.file(file, destfile = "soildata.csv")
soildata <- read.csv("soildata.csv", header=TRUE, sep=",")
soildata2 <- droplevels(subset(soildata, order!="histosol")) #remove Histosol observation

rf2 <- randomForest(Otot ~ rainfall + geology + aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + landsatb3 +landsatb7 + maxc100 + maxent + minc100 + mirref + ndvi+ northeastn + northness + northwestn + planc100 + proc100 + protection + relpos11 + slp50 + solar + tanc75, data = soildata2, importance=TRUE, ntree=1000, mtry=9) 
#importance=TRUE will allow the generation of a variable importance plot

rf2 # statistical summary


varImpPlot(rf2)
imp <- as.data.frame(sort(importance(rf2)[,1],decreasing = TRUE),optional = T)
names(imp) <- "% Inc MSE"
imp # sorted tabular summary


soildata$epipedon2 <- soildata$epipedon
levels(soildata$epipedon2)[levels(soildata$epipedon2)=="ochric"] <- "nonfolistic"
levels(soildata$epipedon2)[levels(soildata$epipedon2)=="umbric"] <- "nonfolistic"


## library(raster)
## rasters <- stack(list.files(getwd(),pattern="img$",full.names=FALSE)) #combines rasters with a .img file extension stored in the working directory
## 
## rasters
## 
## model <- randomForest(Otot~landsatb7+maxent+protection+northwestn+solar, data=soildata2)
## 
## predict(rasters,model,progress="window",overwrite=TRUE,filename="rfpredict.img")
## #type not specified=vector of predicted values, "response" for predicted class, "prob" for probabilities, or "vote" for matrix of vote counts (one column for each class and one row for each new input); either in raw counts or in fractions (if norm.votes=TRUE)
## 
## #options for predicting rpart model: type= "vector" for mean response at the node, "prob" for matrix of class probabilities, or "class" for a factor of classifications based on the responses
## 

## rfpredict <- raster("rfpredict.img")
## plot(rfpredict, xlab="Easting (m)", ylab="Northing (m)", main="Total O Horizon Thickness (cm)")
## 
