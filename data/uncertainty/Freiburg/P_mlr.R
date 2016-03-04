library(randomForest)
library(raster)
library(ggplot2)
library(car) # ask for this car package and the dependencies
library(DAAG)
library(bootstrap)
library(MASS)
library(leaps)

# Import Dataset using option 'From csv or Text File' 
FRIBO_PvsTA <- read.csv("I:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/Freiburg/FRIBO_PvsTA.csv")
View(FRIBO_PvsTA)

setwd("I:/NCSC_Lincoln/Traning/Statistics for Soil Survey/2016_Course/Validation_Data_Lectures/Freiburg")
# getwd()

# Exploratory Data Analysis
# Terrain attribute colinearity
pairs(~elevation+slope+graddist+slopelength+midsolpos+curvature+plancurv+profcurv+stdheight+normheight+twi+vtr+tri, data = FRIBO_PvsTA, main="Simple Scatterplot Matrix")
summary(FRIBO_PvsTA)

# P forms Colinearity
pairs(~Pt+Pi+Po+PAAE+Polsen+PDS+E1min, data = FRIBO_PvsTA, main="Simple Scatterplot Matrix")
summary(FRIBO_PvsTA)

# explicitly define categorical predictors with factors
FRIBO_PvsTA$Soiltype <- factor(FRIBO_PvsTA$Soiltype)
FRIBO_PvsTA$Landuse <- factor(FRIBO_PvsTA$Landuse)


# check data types to assure that the right types are represented
str(FRIBO_PvsTA)
View(FRIBO_PvsTA)

# fit Multiple Regresson (Linear model lm)
fitPt <- lm(Pt ~ elevation + slope + graddist + slopelength + slopelength + midsolpos + curvature + plancurv + profcurv + stdheight + normheight + twi + vtr + tri, data = FRIBO_PvsTA)

# Sumarize the lm results
summary(fitPt)
coefficients(fitPt) # model coefficients
confint(fitPt, level=0.95) # CIs for model parameters 
fitted(fitPt) # predicted values
residuals(fitPt) # residuals
anova(fitPt) # anova table 
vcov(fitPt) # covariance matrix for model parameters 
influence(fitPt) # regression diagnostics 

# Regression Diagnostics
# Diagnostic Plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fitPt)

# Assessing Outliers (these are part of car package tha is not working)
outlierTest(fitPt) # Bonferonni p-value for most extreme obs
qqPlot(fitPt, main="QQ Plot") #qq plot for studentized resid 
leveragePlots(fitPt) # leverage plots



# plot data
# plot(pH.1.5.W ~ pH.1.1.W, data = pH.Data_PTF, main='pH.1:5W vs pH.1:1W')
# add model fit, only works with single variable regression
# abline(fitpH15W, col='red')
# making predictions from the new linear model (regression)
predict(fitPt)

# prediction from original data, save back to original Data Frame
FRIBO_PvsTA$Ptpred <- predict(fitPt, newdata=FRIBO_PvsTA)
# save residuals in a new comuns back to the original Data Frame
FRIBO_PvsTA$Ptres <- residuals(fitPt, newdata=FRIBO_PvsTA)
# Error in `$<-.data.frame`(`*tmp*`, "Ptres", value = c(56.0196997283275,: 
# replacement has 242 rows, data has 245


# write to CSV name a new file between quotations marks ' '
write.csv(FRIBO_PvsTA, file='FRIBO_PvsTA_Pt')

# K-fold Cross Validation
cv.lm(df = 228, fitPt, m=3) # 3 fold cross-validation

# Assessing R2 shrinkage using 10-Fold Cross-Validation
# Define Functions 
theta.fitPt <- function(x,y){lsfitPt(x,y)}
theta.predict <- function(fitPt,x){cbind(1,x)%*%fitPt$coef}

# Matrix of Predictions
na.omit(FRIBO_PvsTA)
FRIBO_PvsTA[complete.cases(FRIBO_PvsTA),]
X < - as.matrix(FRIBO_PvsTA[c("elevation","slope","graddist","slopelength","midsolpos","curvature","plancurv","profcurv","stdheight","normheight","twi","vtr","tri")])
# Error: object 'X' not found
ls(X)
exists("X")

# vector of predicted values
y <- as.matrix(FRIBO_PvsTA[c("FitPt")])
results <- crossval(X,y,theta.fitPt,theta.predict,ngroup=10)
cor(y, fitPt$fitted.values)**2 # raw R2
cor(y,results$cv.fitPt)**2 # cross-validated R2



# Variable Selection in Stepwise Regression
step <- stepAIC(fitPt, direction="both")
step$anova # display results

# All Subsets Regressions
# All Subsets Regression
# library(leaps)
attach(FRIBO_PvsTA)
leaps<-regsubsets(Pt ~ elevation + slope + graddist + slopelength + slopelength + midsolpos + curvature + plancurv + profcurv + stdheight + normheight + twi + vtr + tri, data=FRIBO_PvsTA,nbest=10)

# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")

# plot statistic by subset size (requires car package)
# subsets(leaps, statistic="rsq")

# Relative Importance
#Calculate Relative Importance for Each Predictor

library(relaimpo)
calc.relimp(fitPt,type=c("lmg","last","first","pratt"), rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fitPt, b = 1000, type = c("lmg", "last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
# print result
booteval.relimp(boot)
# plot result
plot(booteval.relimp(boot,sort=TRUE))

