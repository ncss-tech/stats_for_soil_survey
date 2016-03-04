library(randomForest)
library(raster)
library(ggplot2)

# Import Dataset using option 'From Text File' 
pH.Data_PTF <- read.csv("G:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/pH/pH Data_PTF.csv")
 View(pH.Data_PTF)
 setwd("G:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/pH")
# getwd()

# Exploratory Data Analysis
pairs(~pH.1.5.W+pH.1.5.CaCl2+pH.1.1.W+pH.1.2.CaCl2, data = pH.Data_PTF, main="Simple Scatterplot Matrix")
summary(pH.Data_PTF)

# explicitly define categorical predictors with factors
pH.Data_PTF$Soil.Order <- factor(pH.Data_PTF$Soil.Order)
pH.Data_PTF$Mineralogy.Class <- factor(pH.Data_PTF$Mineralogy.Class)
pH.Data_PTF$Texture.Class <- factor(pH.Data_PTF$Texture.Class)
pH.Data_PTF$Genetic.Horizons <- factor(pH.Data_PTF$Genetic.Horizons)

# check data types to assure that the right types are represented
str(pH.Data_PTF)
View(pH.Data_PTF)

# fit Regresson (Linear model lm)
fitpH15W <- lm(pH.1.5.W ~ pH.1.1.W, data = pH.Data_PTF)
# Sumarize the lm results
summary(fitpH15W)
# plot data
plot(pH.1.5.W ~ pH.1.1.W, data = pH.Data_PTF, main='pH.1:5W vs pH.1:1W')
# add model fit, only works with single variable regression
abline(fitpH15W, col='red')
# making predictions from the new linear model (regression)
predict(fitpH15W)

# prediction from original data, save back to original Date Frame
pH.Data_PTF$pH15W.pred.pH11W <- predict(fitpH15W, newdata=pH.Data_PTF)

# get model residuals
#hist(predict(fitpH15W, newdata=pH.Data_PTF) - pH.Data_PTF$pH15W.pred.pH11W)
#save residuals back to the original data frame
pH.Data_PTF$pH15W.res <- predict(fitpH15W, newdata=pH.Data_PTF - pH.Data_PTF$pH15W.pred.pH11W)
#Plot residuals
plot(pH15W.res ~ pH15W.pred.pH11W, data=pH.Data_PTF )
plot(fitpH15W, newdata=pH.Data_PTF - pH.Data_PTF$pH15W.pred.pH11W)

# check by ploting original vs. predictions
plot(pH.1.5.W ~ pH15W.pred.pH11W, data=pH.Data_PTF)

# add 1:1 line
abline(a = 0, b=1, col='red')

# add basic regression line
abline(lm(pH15W.pred.pH11W ~ pH.1.1.W, data=pH.Data_PTF), col='blue', lty=2)

# write to CSV name a new file between quotations marks ' '
write.csv(pH.Data_PTF, file='pH.Data_PTF_ph15W')
