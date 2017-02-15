library(randomForest)
library(raster)
library(ggplot2)


Dillon_Cr_WSH_SoilDepth_TA <- read.csv("G:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/Dillon_Creek/Dillon_Cr_WSH_SoilDepth_TA.txt")
 View(Dillon_Cr_WSH_SoilDepth_TA)
 setwd("G:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/Dillon_Creek")
 # getwd()

sdepth <-read.table("G:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/Dillon_Creek/Dillon_Cr_WSH_SoilDepth_TA.txt", header = TRUE, sep = "")

# Import Dataset using option 'From Text File' 
Dillon_Cr_WSH_SoilDepth_TA <- read.csv("G:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/Dillon_Creek/Dillon_Cr_WSH_SoilDepth_TA.txt", header=TRUE)

# View(Dillon_Cr_WSH_SoilDepth_TA)


# Exploratory Data Analysis
pairs(~twi_5m+slope_5m+mrvbf_5m+mrrtf_5m+aachn_5m+elev_5m, data = Dillon_Cr_WSH_SoilDepth_TA, main="Simple Scatterplot Matrix")
summary(Dillon_Cr_WSH_SoilDepth_TA)

# You can repeat by substituting terrain attributes with sources of soil depth data
pairs(~Measured+SSURGO_RV+ArcSIE_SSU+SS_Manual+SDM+OSD, data = Dillon_Cr_WSH_SoilDepth_TA, main="Simple Scatterplot Matrix")
summary(Dillon_Cr_WSH_SoilDepth_TA)

# explicitly define categorical predictors with factors
Dillon_Cr_WSH_SoilDepth_TA$LS_Pos <- factor(Dillon_Cr_WSH_SoilDepth_TA$LS_Pos)

# check data types to assure that the right types are represented
str(Dillon_Cr_WSH_SoilDepth_TA)
View(Dillon_Cr_WSH_SoilDepth_TA)

# fit RF model
fitMeasuredrf <- randomForest(Measured ~ LS_Pos + twi_5m + slope_5m + mrvbf_5m + mrrtf_5m + aachn_5m + elev_5m, data = Dillon_Cr_WSH_SoilDepth_TA, importance=TRUE, ntree=100)

# You could Repeat the RF model by only substituting the Source of Depth measurement 'Measured' from the data frame with 'SSURGO_RV', 'ArSIE_SSU', 'SS_Manual', 'SDM', and 'OSD' 
# like the example below.
fitSSURGO_RVrf <- randomForest(SSURGO_RV ~ LS_Pos + twi_5m + slope_5m + mrvbf_5m + mrrtf_5m + aachn_5m + elev_5m, data = Dillon_Cr_WSH_SoilDepth_TA, importance=TRUE, ntree=100) 

# You could also, Repeat the RF model by only substituting the predictors Source of Depth measurement from the data frame 'twi_90m', 'slope_90m', 'mrvbf_90m', 'mrrtf_90m', 'aachn_90m', and 'elev_90m

fitMeasuredrf <- randomForest(SSURGO_RV ~ LS_Pos + twi_90m + slope_90m + mrvbf_90m + mrrtf_90m + aachn_90m + elev_90m, data = Dillon_Cr_WSH_SoilDepth_TA, importance=TRUE, ntree=100) 


# Check the results (repeated these comands after each run)
plot(fitMeasuredrf)
importance (fitMeasuredrf)
plot(importance (fitMeasuredrf))
print(fitMeasuredrf)

# prediction from original data, save back to original Date Frame
Dillon_Cr_WSH_SoilDepth_TA$Measured.pred.rf5m <- predict(fitMeasuredrf, newdata=Dillon_Cr_WSH_SoilDepth_TA)
# repeat these after each run
Dillon_Cr_WSH_SoilDepth_TA$Measured.pred.rf90m <- predict(fitMeasuredrf, newdata=Dillon_Cr_WSH_SoilDepth_TA)

# get model residuals
hist(predict(fitMeasuredrf, newdata=Dillon_Cr_WSH_SoilDepth_TA) - Dillon_Cr_WSH_SoilDepth_TA$Measured)

# check by ploting original vs. predictions
plot(Measured.pred.rf5m ~ Measured, data=Dillon_Cr_WSH_SoilDepth_TA)
plot(Measured.pred.rf90m ~ Measured, data=Dillon_Cr_WSH_SoilDepth_TA)

# add 1:1 line
abline(a = 0, b=1, col='red')

# add basic regression line
abline(lm(Measured.pred.rf5m ~ Measured, data=Dillon_Cr_WSH_SoilDepth_TA), col='blue', lty=2)

# write to CSV name a new file between quotations marks ' '
write.csv(Dillon_Cr_WSH_SoilDepth_TA, file='Dillon_Cr_WSH_SoilDepth_TArf')
