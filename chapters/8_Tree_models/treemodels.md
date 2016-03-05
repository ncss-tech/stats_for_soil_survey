# Chapter 8 - Tree-based Models
Katey Yoast  
Thursday, January 14, 2016  


![Statistics for pedologists course banner image](figure/logo.jpg)


## 8.0  Introduction
Tree-based models are a supervised machine learning method commonly used in soil survey and ecology for exploratory data analysis and prediction due to their simplistic nonparametric design. Instead of fitting a model to the data, tree-based models recursively partition the data into increasingly homogenous groups based on values that minimize a loss function (such as Sum of Squared Errors (SSE) for regression or Gini Index for classification) ([McBratney et al.,2013](http://www.clw.csiro.au/aclep/documents/McBratney%20et%20al.%202003.pdf)). The two most common packages for generating tree-based models in R are rpart and randomForest. The rpart package creates a regression or classification tree based on binary splits that maximize homogeneity and minimize impurity. The output is a single decision tree that can be further "pruned" or trimmed back using the cross-validation error statistic to reduce over-fitting. The randomForest package is similar to rpart, but is double random in that each node is split using a random subset of predictors AND observations at each node and this process is repeated hundreds of times (as specified by the user). Unlike rpart, random forests do not produce a graphical decision tree since the predictions are averaged across hundreds or thousands of trees. Instead, random forests produce a variable importance plot and a tabular statistical summary. 


## 8.1 Exploratory Data Analysis

The data that we will be working with in this chapter were collected in support of a MLRA 127 soil survey update project to tabularly and spatially update SSRUGO map units for spodic properties in the Monongahela National Forest. Soils that were historically covered by Eastern Hemlock and Red Spruce exhibit spodic morphology on shale, siltstone, and sandstone bedrocks at elevations typically >3,200 ft in West Virginia ([Nauman et al., 2015](http://www.sciencedirect.com/science/article/pii/S0016706115000427)). The landscape and vegetative communities were greatly altered by fire and logging in the early 1900s, complicating the identification of spodic morphology. It is of particular interest to the project leader and the U.S. Forest Service that spatial maps be developed to depict the location of spodic morphology and folistic epipedons in the Monongahela National Forest. Folistic epipedons provide habitat for the Northern Flying Squirrel, just recently removed from the endangered species list.  

### 8.1.1 Examining Data in R

Before we dive in to model building, let's first import and examine the dataset in R.  


```r
library(lattice) #graphing
library(sp) #spatial data
library(maps) #maps
library(rgdal) #spatial import

file<-'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/wv_transect_editedforR.csv'
download.file(file, destfile = "soildata.csv")
soildata<-read.csv("soildata.csv", header=TRUE, sep=",")
View(soildata) #view the data
str(soildata) #examine the internal data structure
```

```
## 'data.frame':	250 obs. of  50 variables:
##  $ x           : num  -79.7 -79.7 -79.7 -79.8 -79.7 ...
##  $ y           : num  38.6 38.6 38.6 38.6 38.6 ...
##  $ Overtype    : Factor w/ 3 levels "Conifer","Hardwood",..: 2 2 2 2 3 3 3 2 3 2 ...
##  $ Underconifer: Factor w/ 2 levels "n","y": 1 2 1 2 2 2 2 1 2 2 ...
##  $ Oi          : num  3 4 3 4 3 2 2 2 4 3 ...
##  $ Oe          : num  0 0 2 4 4 6 4 2 2 4 ...
##  $ Oa          : num  0 3 0 0 8 0 0 0 7 4 ...
##  $ Otot        : num  3 7 5 8 15 8 6 4 13 7 ...
##  $ epipedon    : Factor w/ 3 levels "folistic","ochric",..: 2 2 2 2 1 2 2 2 2 2 ...
##  $ spodint     : num  0 0 0 0 1 1 1 0.5 1 0 ...
##  $ subgroup    : Factor w/ 5 levels "aeric","aquic",..: 4 4 4 4 3 3 3 4 3 4 ...
##  $ order       : Factor w/ 4 levels "histosol","inceptisol",..: 4 2 4 2 4 4 2 2 2 4 ...
##  $ ps          : Factor w/ 5 levels "cl","fl","fragm",..: 2 5 2 1 2 2 1 2 2 2 ...
##  $ drainage    : Factor w/ 4 levels "exd","mwd","swp",..: 4 4 4 4 4 4 4 4 4 4 ...
##  $ series      : Factor w/ 16 levels "Blandburg","Carrollton",..: 2 9 2 9 2 2 9 9 9 2 ...
##  $ taxon       : Factor w/ 3 levels "family","series",..: 2 2 2 3 2 2 3 3 3 2 ...
##  $ slope       : int  45 54 39 25 38 30 38 36 16 36 ...
##  $ surfacetex  : Factor w/ 21 levels "cnhosil","cnl",..: 19 19 2 19 19 2 19 20 20 20 ...
##  $ stoniness   : Factor w/ 5 levels "ns","rb","st",..: 5 4 5 4 4 4 4 1 1 1 ...
##  $ depthclass  : Factor w/ 4 levels "d","md","sh",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ bedrockdepth: int  200 200 200 200 200 200 200 200 200 200 ...
##  $ depth_cm    : int  67 69 52 95 93 81 86 90 75 92 ...
##  $ hillslope   : Factor w/ 4 levels "backslope","footslope",..: 1 1 1 1 1 1 1 1 3 1 ...
##  $ tipmound    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ rainfall    : int  51 51 51 51 51 51 51 51 51 51 ...
##  $ geology     : Factor w/ 2 levels "Dch","Dhs": 1 2 2 2 1 2 1 1 1 2 ...
##  $ aachn       : num  27.46 81.7 2.59 43.36 70.75 ...
##  $ dem10m      : num  1123 1239 1155 1240 1211 ...
##  $ downslpgra  : num  25.9 25.1 78 37 26.1 ...
##  $ eastness    : num  -0.53 0.972 1 0.9 -0.474 ...
##  $ greenrefl   : num  0.1035 0.0425 0.0351 0.0776 0.0721 ...
##  $ landsatb1   : num  0.0726 0.0742 0.0742 0.0775 0.0694 ...
##  $ landsatb2   : num  0.062 0.0583 0.0547 0.0583 0.0528 ...
##  $ landsatb3   : num  0.0335 0.0351 0.0301 0.0419 0.0318 ...
##  $ landsatb7   : num  0.0494 0.0516 0.043 0.0451 0.0302 ...
##  $ maxc100     : num  -0.0845 -0.0963 -0.0848 0.2386 0.1368 ...
##  $ maxent      : num  20.26 30.21 27.44 7.88 73.85 ...
##  $ minc100     : num  -0.1925 -0.1457 -0.8324 0.069 0.0998 ...
##  $ mirref      : num  0.1608 0.0827 0.1109 0.1477 0.0762 ...
##  $ ndvi        : num  0.866 0.822 0.842 0.8 0.77 ...
##  $ northeastn  : num  -0.974 0.854 0.728 0.944 -0.958 ...
##  $ northness   : num  -0.848 0.237 0.03 0.435 -0.88 ...
##  $ northwestn  : num  -0.225 -0.52 -0.686 -0.329 -0.287 ...
##  $ planc100    : num  -0.1177 -0.1077 -0.2173 0.1321 0.0476 ...
##  $ proc100     : num  -0.231 -0.178 -0.735 0.221 0.161 ...
##  $ protection  : num  0.237 0.154 0.222 0.114 0.138 ...
##  $ relpos11    : num  0.352 0.507 0.233 0.544 0.585 ...
##  $ slp50       : num  42.4 42.3 26.3 24.5 36.3 ...
##  $ solar       : int  1481510 1280610 1348160 1322630 1532160 1536340 1488440 1155820 1440500 1474730 ...
##  $ tanc75      : num  0.0511 0.0279 0.0553 -0.0187 -0.0346 ...
```

The example dataset, **soildata**, consists of 250 observations and 58 variables that were collected in the field or derived from geospatial data to identify spodic soil properties in West Virginia. Of particular interest is determining best splits for spodic intensity (relative spodicity index). As you can see in the data structure, R interpreted the spodint field as numeric. Since spodint is an index, it will need to be changed to a factor and then to an ordered factor. The same will need to be done for tipmound (a tip and mound microtopography index).


```r
soildata$spodint<-as.factor(soildata$spodint)
soildata$spodint<-ordered(soildata$spodint)
soildata$tipmound<-as.factor(soildata$tipmound)
soildata$tipmound<-ordered(soildata$tipmound)
```

Next, let's explore the tabular data:


```r
boxplot(solar~spodint, data=soildata, xlab="spodic intensity", ylab="solar") #does solar radiation affect spodic intensity?
```

![](treemodels_files/figure-html/unnamed-chunk-3-1.png)

```r
boxplot(northwestn~spodint, data=soildata, xlab="spodic intensity", ylab="northwestness") #how about aspect?
```

![](treemodels_files/figure-html/unnamed-chunk-3-2.png)

```r
densityplot(~ Otot|order, data=soildata) #distribution of O horizon thickness among soil orders
```

![](treemodels_files/figure-html/unnamed-chunk-3-3.png)

```r
numeric<-data.frame(soildata[, c(8, 25, 27:50)]) #combine numeric columns into a new data frame
names(numeric) 
```

```
##  [1] "Otot"       "rainfall"   "aachn"      "dem10m"     "downslpgra"
##  [6] "eastness"   "greenrefl"  "landsatb1"  "landsatb2"  "landsatb3" 
## [11] "landsatb7"  "maxc100"    "maxent"     "minc100"    "mirref"    
## [16] "ndvi"       "northeastn" "northness"  "northwestn" "planc100"  
## [21] "proc100"    "protection" "relpos11"   "slp50"      "solar"     
## [26] "tanc75"
```

```r
round(cor(numeric, use = "pairwise"), 2) #look at what variables are highly correlated to Otot
```

```
##             Otot rainfall aachn dem10m downslpgra eastness greenrefl
## Otot        1.00    -0.24 -0.21   0.01      -0.12    -0.26     -0.13
## rainfall   -0.24     1.00  0.11  -0.21       0.21     0.09      0.01
## aachn      -0.21     0.11  1.00   0.35      -0.07     0.20      0.04
## dem10m      0.01    -0.21  0.35   1.00       0.07     0.07     -0.07
## downslpgra -0.12     0.21 -0.07   0.07       1.00    -0.11     -0.01
## eastness   -0.26     0.09  0.20   0.07      -0.11     1.00     -0.06
## greenrefl  -0.13     0.01  0.04  -0.07      -0.01    -0.06      1.00
## landsatb1  -0.07     0.19  0.07  -0.20       0.08     0.16      0.21
## landsatb2  -0.27     0.19  0.12  -0.09       0.00     0.15      0.33
## landsatb3  -0.15     0.08  0.14   0.05       0.05     0.18      0.25
## landsatb7  -0.36     0.19  0.23   0.05       0.04     0.43      0.27
## maxc100    -0.02     0.18  0.26  -0.04      -0.15     0.13     -0.11
## maxent      0.34    -0.22 -0.03   0.42       0.11    -0.23     -0.04
## minc100     0.09    -0.03  0.34   0.19      -0.23    -0.05     -0.14
## mirref     -0.25     0.07  0.12  -0.04       0.04     0.16      0.82
## ndvi       -0.11     0.09  0.01  -0.03       0.01     0.05      0.01
## northeastn -0.05     0.03  0.10   0.02      -0.13     0.69     -0.15
## northness   0.19    -0.05 -0.07  -0.04      -0.07    -0.06     -0.15
## northwestn  0.31    -0.09 -0.18  -0.08       0.02    -0.73     -0.06
## planc100    0.12     0.03  0.13   0.02      -0.18    -0.02     -0.11
## proc100    -0.05     0.11  0.48   0.13      -0.18     0.13     -0.15
## protection  0.13    -0.26 -0.60  -0.30      -0.37     0.05      0.08
## relpos11   -0.18     0.26  0.70   0.23       0.03     0.15     -0.08
## slp50       0.13    -0.30 -0.19  -0.14      -0.72     0.12      0.01
## solar      -0.18     0.06  0.16   0.25       0.26     0.01      0.14
## tanc75     -0.14     0.04  0.03  -0.02       0.20     0.03      0.07
##            landsatb1 landsatb2 landsatb3 landsatb7 maxc100 maxent minc100
## Otot           -0.07     -0.27     -0.15     -0.36   -0.02   0.34    0.09
## rainfall        0.19      0.19      0.08      0.19    0.18  -0.22   -0.03
## aachn           0.07      0.12      0.14      0.23    0.26  -0.03    0.34
## dem10m         -0.20     -0.09      0.05      0.05   -0.04   0.42    0.19
## downslpgra      0.08      0.00      0.05      0.04   -0.15   0.11   -0.23
## eastness        0.16      0.15      0.18      0.43    0.13  -0.23   -0.05
## greenrefl       0.21      0.33      0.25      0.27   -0.11  -0.04   -0.14
## landsatb1       1.00      0.43      0.40      0.39    0.01  -0.20   -0.13
## landsatb2       0.43      1.00      0.62      0.64   -0.01  -0.21   -0.19
## landsatb3       0.40      0.62      1.00      0.65    0.01  -0.09   -0.04
## landsatb7       0.39      0.64      0.65      1.00   -0.09  -0.32   -0.17
## maxc100         0.01     -0.01      0.01     -0.09    1.00   0.03    0.57
## maxent         -0.20     -0.21     -0.09     -0.32    0.03   1.00    0.17
## minc100        -0.13     -0.19     -0.04     -0.17    0.57   0.17    1.00
## mirref          0.32      0.44      0.40      0.59   -0.15  -0.20   -0.22
## ndvi            0.00      0.16     -0.09      0.07   -0.05  -0.25   -0.12
## northeastn     -0.07     -0.17     -0.10      0.04    0.07  -0.25   -0.02
## northness      -0.25     -0.39     -0.31     -0.37   -0.03  -0.12    0.02
## northwestn     -0.28     -0.38     -0.33     -0.55   -0.11   0.08    0.05
## planc100       -0.08     -0.13     -0.04     -0.21    0.77   0.15    0.78
## proc100        -0.05     -0.05      0.00     -0.02    0.72   0.04    0.66
## protection      0.00      0.04     -0.07     -0.05   -0.39  -0.11   -0.49
## relpos11       -0.03      0.04      0.06      0.11    0.61   0.01    0.54
## slp50          -0.08     -0.02     -0.09     -0.07   -0.18  -0.13   -0.01
## solar           0.22      0.34      0.32      0.37    0.04   0.21    0.04
## tanc75          0.08      0.12      0.06      0.23   -0.65  -0.16   -0.68
##            mirref  ndvi northeastn northness northwestn planc100 proc100
## Otot        -0.25 -0.11      -0.05      0.19       0.31     0.12   -0.05
## rainfall     0.07  0.09       0.03     -0.05      -0.09     0.03    0.11
## aachn        0.12  0.01       0.10     -0.07      -0.18     0.13    0.48
## dem10m      -0.04 -0.03       0.02     -0.04      -0.08     0.02    0.13
## downslpgra   0.04  0.01      -0.13     -0.07       0.02    -0.18   -0.18
## eastness     0.16  0.05       0.69     -0.06      -0.73    -0.02    0.13
## greenrefl    0.82  0.01      -0.15     -0.15      -0.06    -0.11   -0.15
## landsatb1    0.32  0.00      -0.07     -0.25      -0.28    -0.08   -0.05
## landsatb2    0.44  0.16      -0.17     -0.39      -0.38    -0.13   -0.05
## landsatb3    0.40 -0.09      -0.10     -0.31      -0.33    -0.04    0.00
## landsatb7    0.59  0.07       0.04     -0.37      -0.55    -0.21   -0.02
## maxc100     -0.15 -0.05       0.07     -0.03      -0.11     0.77    0.72
## maxent      -0.20 -0.25      -0.25     -0.12       0.08     0.15    0.04
## minc100     -0.22 -0.12      -0.02      0.02       0.05     0.78    0.66
## mirref       1.00  0.06      -0.09     -0.29      -0.31    -0.19   -0.18
## ndvi         0.06  1.00      -0.04     -0.11      -0.11    -0.08   -0.07
## northeastn  -0.09 -0.04       1.00      0.69       0.00    -0.01    0.08
## northness   -0.29 -0.11       0.69      1.00       0.73     0.01   -0.02
## northwestn  -0.31 -0.11       0.00      0.73       1.00     0.02   -0.11
## planc100    -0.19 -0.08      -0.01      0.01       0.02     1.00    0.39
## proc100     -0.18 -0.07       0.08     -0.02      -0.11     0.39    1.00
## protection   0.06  0.03       0.05      0.02      -0.03    -0.31   -0.54
## relpos11    -0.07  0.03       0.05     -0.08      -0.15     0.38    0.78
## slp50       -0.03  0.02       0.17      0.11      -0.01    -0.02   -0.18
## solar        0.27  0.06      -0.60     -0.83      -0.58    -0.01    0.09
## tanc75       0.16  0.05      -0.01     -0.04      -0.05    -0.88   -0.30
##            protection relpos11 slp50 solar tanc75
## Otot             0.13    -0.18  0.13 -0.18  -0.14
## rainfall        -0.26     0.26 -0.30  0.06   0.04
## aachn           -0.60     0.70 -0.19  0.16   0.03
## dem10m          -0.30     0.23 -0.14  0.25  -0.02
## downslpgra      -0.37     0.03 -0.72  0.26   0.20
## eastness         0.05     0.15  0.12  0.01   0.03
## greenrefl        0.08    -0.08  0.01  0.14   0.07
## landsatb1        0.00    -0.03 -0.08  0.22   0.08
## landsatb2        0.04     0.04 -0.02  0.34   0.12
## landsatb3       -0.07     0.06 -0.09  0.32   0.06
## landsatb7       -0.05     0.11 -0.07  0.37   0.23
## maxc100         -0.39     0.61 -0.18  0.04  -0.65
## maxent          -0.11     0.01 -0.13  0.21  -0.16
## minc100         -0.49     0.54 -0.01  0.04  -0.68
## mirref           0.06    -0.07 -0.03  0.27   0.16
## ndvi             0.03     0.03  0.02  0.06   0.05
## northeastn       0.05     0.05  0.17 -0.60  -0.01
## northness        0.02    -0.08  0.11 -0.83  -0.04
## northwestn      -0.03    -0.15 -0.01 -0.58  -0.05
## planc100        -0.31     0.38 -0.02 -0.01  -0.88
## proc100         -0.54     0.78 -0.18  0.09  -0.30
## protection       1.00    -0.75  0.75 -0.34   0.14
## relpos11        -0.75     1.00 -0.39  0.20  -0.16
## slp50            0.75    -0.39  1.00 -0.45  -0.12
## solar           -0.34     0.20 -0.45  1.00   0.09
## tanc75           0.14    -0.16 -0.12  0.09   1.00
```


### 8.1.2 Plotting Data

```r
coordinates(soildata) <- ~ x + y #set the coordinates
proj4string(soildata) <- CRS("+init=EPSG:4269") #set the projection; https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf 

map("county", "west virginia") 
points(soildata) #plot points
```

![](treemodels_files/figure-html/unnamed-chunk-4-1.png)


```r
#convert soildata into a shapefile
writeOGR(soildata, dsn = "C:/workspace", "soildata", driver = "ESRI Shapefile") 
```

Conveniently, environmental covariate values were previously extracted for all of the observations in the **soildata** dataset. **How would you extract raster data to points in R?**  ([Hint](https://github.com/ncss-tech/stats_for_soil_survey/blob/master/chapters/6_linear_models/6_Linear_models.md))


### 8.1.3 Exercise 1
Examine the **soildata** shapefile and environmental covariate data in ArcGIS. Come up with a theory of which possible covariates might be useful for predicting spodic morphology and folistic epipedons. Also, think of the different possible ways to model these features given the **soildata** dataset. 


## 8.2 Classification and Regression Trees (CART)
The basic function for all CART models is (y ~ x), where y is the dependent variable to be predicted from x, a set of independent variables. If the dependent variable (y) is numeric, the resulting tree will be a regression tree. Conversely, if the dependent variable (y) is categorical, the resulting tree will be a classification tree. The rpart package allows all data types to be used as independent variables, regardless of whether the model is a classification or regression tree. The rpart algorithm ignores missing values when determining the quality of a split and uses surrogate splits to determine if observation(s) with missing data is best split left or right. If an observation is missing all surrogate splits, then the observation(s) is sent to the child node with the largest relative frequency (Feelders, 1999).

Assuming that the rpart and randomForest packages are already installed on your machine, simply load the packages using the `library()` function.


```r
library(rpart)
library(randomForest)
library(rpart.plot)
```


If you wanted to create a classification tree for spodint using all of the variables, you would simply type: `rpart(spodint ~ ., data=soildata)`. Since our goal is to generate a spatial prediction model, we only want to use the variables for which we have spatial coverage--our environmental covariate rasters. 


```r
spodintmodel<-rpart(spodint~rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, method = "class")

spodintmodel
```

```
## n= 250 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##   1) root 250 162 0 (0.35 0.084 0.26 0.024 0.28)  
##     2) northwestn< 0.55138 148  80 0 (0.46 0.081 0.29 0.027 0.14)  
##       4) maxc100< 0.10341 57  20 0 (0.65 0.088 0.14 0.018 0.11) *
##       5) maxc100>=0.10341 91  56 1 (0.34 0.077 0.38 0.033 0.16)  
##        10) solar< 1504355 83  52 0 (0.37 0.084 0.33 0.036 0.18)  
##          20) minc100< -0.197089 9   1 0 (0.89 0 0 0 0.11) *
##          21) minc100>=-0.197089 74  47 1 (0.31 0.095 0.36 0.041 0.19)  
##            42) maxent< 12.9024 23  10 0 (0.57 0.13 0.26 0.043 0)  
##              84) solar< 1460005 16   5 0 (0.69 0.19 0.12 0 0) *
##              85) solar>=1460005 7   3 1 (0.29 0 0.57 0.14 0) *
##            43) maxent>=12.9024 51  30 1 (0.2 0.078 0.41 0.039 0.27)  
##              86) greenrefl>=0.01846795 40  20 1 (0.22 0.075 0.5 0.025 0.17)  
##               172) dem10m>=1012.96 33  14 1 (0.27 0.061 0.58 0 0.091)  
##                 344) minc100< 0.06651935 24   7 1 (0.21 0.083 0.71 0 0) *
##                 345) minc100>=0.06651935 9   5 0 (0.44 0 0.22 0 0.33) *
##               173) dem10m< 1012.96 7   3 2 (0 0.14 0.14 0.14 0.57) *
##              87) greenrefl< 0.01846795 11   4 2 (0.091 0.091 0.091 0.091 0.64) *
##        11) solar>=1504355 8   0 1 (0 0 1 0 0) *
##     3) northwestn>=0.55138 102  53 2 (0.2 0.088 0.22 0.02 0.48)  
##       6) protection< 0.0988242 25  15 1 (0.2 0.16 0.4 0.04 0.2)  
##        12) proc100< 0.1502945 14  10 0 (0.29 0.29 0.14 0 0.29) *
##        13) proc100>=0.1502945 11   3 1 (0.091 0 0.73 0.091 0.091) *
##       7) protection>=0.0988242 77  33 2 (0.19 0.065 0.16 0.013 0.57)  
##        14) maxent< 9.5806 30  19 0 (0.37 0.1 0.17 0.033 0.33)  
##          28) rainfall< 52 9   2 0 (0.78 0 0 0 0.22) *
##          29) rainfall>=52 21  13 2 (0.19 0.14 0.24 0.048 0.38)  
##            58) landsatb7>=0.0398039 12   7 1 (0.17 0.25 0.42 0 0.17) *
##            59) landsatb7< 0.0398039 9   3 2 (0.22 0 0 0.11 0.67) *
##        15) maxent>=9.5806 47  13 2 (0.085 0.043 0.15 0 0.72) *
```

```r
plot(spodintmodel)
text(spodintmodel, cex=0.8) #cex is text size
```

![](treemodels_files/figure-html/unnamed-chunk-7-1.png)

Notice that the terminal nodes display the different spodic intensity classes, ranging from 0 to 2. **Can you think of another way that we could model spodic expression?** 

Could we treat spodic intesity (an ordered factor) as numeric, ranging from 0 to 1 to develop a regression tree? Does this make sense? what would a mean of 1 tell you about the observations in the terminal node? 

What if we considered everything with a spodic intensity of <= 0.5 to be non-spodic and everything >0.5 to be spodic? A binary probability approach to predicting spodic morphology, similar to Nauman et al., 2015.


```r
index <- c(0, 0.5, 1, 1.5, 2) #similar to lookup table
values <- c("nonspodic", "nonspodic", "spodic", "spodic", "spodic") #similar to lookup table
soildata$newcolumn <- values[match(soildata$spodint, index)] #similar to lookup table
soildata$newcolumn<-as.factor(soildata$newcolumn) #convert new column from character to factor

spodintmodel2<-rpart(newcolumn~rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, method = "class")

spodintmodel2
```

```
## n= 250 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 250 109 spodic (0.4360000 0.5640000)  
##    2) eastness>=-0.1275925 100  36 nonspodic (0.6400000 0.3600000)  
##      4) tanc75>=-0.0268239 64  16 nonspodic (0.7500000 0.2500000) *
##      5) tanc75< -0.0268239 36  16 spodic (0.4444444 0.5555556)  
##       10) northeastn>=0.7433605 13   3 nonspodic (0.7692308 0.2307692) *
##       11) northeastn< 0.7433605 23   6 spodic (0.2608696 0.7391304) *
##    3) eastness< -0.1275925 150  45 spodic (0.3000000 0.7000000)  
##      6) minc100< -0.1576455 47  23 spodic (0.4893617 0.5106383)  
##       12) dem10m>=1054.305 15   2 nonspodic (0.8666667 0.1333333) *
##       13) dem10m< 1054.305 32  10 spodic (0.3125000 0.6875000)  
##         26) protection>=0.178847 13   5 nonspodic (0.6153846 0.3846154) *
##         27) protection< 0.178847 19   2 spodic (0.1052632 0.8947368) *
##      7) minc100>=-0.1576455 103  22 spodic (0.2135922 0.7864078)  
##       14) landsatb7>=0.0440802 37  15 spodic (0.4054054 0.5945946)  
##         28) planc100< -0.00902005 8   1 nonspodic (0.8750000 0.1250000) *
##         29) planc100>=-0.00902005 29   8 spodic (0.2758621 0.7241379) *
##       15) landsatb7< 0.0440802 66   7 spodic (0.1060606 0.8939394) *
```

```r
plot(spodintmodel2)
text(spodintmodel2, cex=0.8)
```

![](treemodels_files/figure-html/unnamed-chunk-8-1.png)

Notice that ..... One way to compare the two models is to use the function `printcp()`:


```r
printcp(spodintmodel)
```

```
## 
## Classification tree:
## rpart(formula = spodint ~ rainfall + geology + aachn + dem10m + 
##     downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + 
##     landsatb3 + landsatb7 + maxc100 + maxent + minc100 + mirref + 
##     ndvi + northeastn + northness + northwestn + planc100 + proc100 + 
##     protection + relpos11 + slp50 + solar + tanc75, data = soildata, 
##     method = "class")
## 
## Variables actually used in tree construction:
##  [1] dem10m     greenrefl  landsatb7  maxc100    maxent     minc100   
##  [7] northwestn proc100    protection rainfall   solar     
## 
## Root node error: 162/250 = 0.648
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.179012      0   1.00000 1.00000 0.046614
## 2 0.030864      1   0.82099 0.90123 0.048107
## 3 0.029321      2   0.79012 1.01852 0.046235
## 4 0.018519      7   0.63580 0.97531 0.047069
## 5 0.015432      8   0.61728 0.96296 0.047276
## 6 0.012346     11   0.56790 0.96296 0.047276
## 7 0.010000     14   0.53086 0.95679 0.047374
```

```r
printcp(spodintmodel2)
```

```
## 
## Classification tree:
## rpart(formula = newcolumn ~ rainfall + geology + aachn + dem10m + 
##     downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + 
##     landsatb3 + landsatb7 + maxc100 + maxent + minc100 + mirref + 
##     ndvi + northeastn + northness + northwestn + planc100 + proc100 + 
##     protection + relpos11 + slp50 + solar + tanc75, data = soildata, 
##     method = "class")
## 
## Variables actually used in tree construction:
## [1] dem10m     eastness   landsatb7  minc100    northeastn planc100  
## [7] protection tanc75    
## 
## Root node error: 109/250 = 0.436
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.256881      0   1.00000 1.00000 0.071933
## 2 0.050459      1   0.74312 0.74312 0.067887
## 3 0.027523      5   0.54128 0.83486 0.069795
## 4 0.010000      8   0.45872 0.85321 0.070112
```

The `printcp()` funtion generates a cost complexity parameter table that provides the complexity parameter value (CP), relative model error (1 - relative error = ~variance explained), error estimated from a 10-fold cross validation (xerror), and the standard error of the xerror (xstd). The CP values control the size of the tree; the greater the CP value, the fewer the number of splits in the tree. To determine the optimal CP value, rpart automatically performs a 10-fold cross validation. The optimal size of the tree is generally the row in the CP table that minimizes all error with the fewest branches. Another way to determine the optimal tree size is to use the `plotcp()` function. This will plot the xerror versus cp value and tree size. 


```r
plotcp(spodintmodel)
```

![](treemodels_files/figure-html/unnamed-chunk-10-1.png)

```r
plotcp(spodintmodel2)
```

![](treemodels_files/figure-html/unnamed-chunk-10-2.png)

The optimal CP value is 0.099 for spodintmodel and 0.034 for spodintmodel2. Since both spodic intensity models overfit that data, they will need to be pruned using the `prune()` function.


```r
pruned<-prune(spodintmodel, cp=0.099)
printcp(pruned)
```

```
## 
## Classification tree:
## rpart(formula = spodint ~ rainfall + geology + aachn + dem10m + 
##     downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + 
##     landsatb3 + landsatb7 + maxc100 + maxent + minc100 + mirref + 
##     ndvi + northeastn + northness + northwestn + planc100 + proc100 + 
##     protection + relpos11 + slp50 + solar + tanc75, data = soildata, 
##     method = "class")
## 
## Variables actually used in tree construction:
## [1] northwestn
## 
## Root node error: 162/250 = 0.648
## 
## n= 250 
## 
##        CP nsplit rel error  xerror     xstd
## 1 0.17901      0   1.00000 1.00000 0.046614
## 2 0.09900      1   0.82099 0.90123 0.048107
```

```r
plot(pruned)
text(pruned, cex=0.8)
```

![](treemodels_files/figure-html/unnamed-chunk-11-1.png)

```r
pruned2<-prune(spodintmodel2, cp=0.034)
printcp(pruned2)
```

```
## 
## Classification tree:
## rpart(formula = newcolumn ~ rainfall + geology + aachn + dem10m + 
##     downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + 
##     landsatb3 + landsatb7 + maxc100 + maxent + minc100 + mirref + 
##     ndvi + northeastn + northness + northwestn + planc100 + proc100 + 
##     protection + relpos11 + slp50 + solar + tanc75, data = soildata, 
##     method = "class")
## 
## Variables actually used in tree construction:
## [1] dem10m     eastness   minc100    northeastn tanc75    
## 
## Root node error: 109/250 = 0.436
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.256881      0   1.00000 1.00000 0.071933
## 2 0.050459      1   0.74312 0.74312 0.067887
## 3 0.034000      5   0.54128 0.83486 0.069795
```

```r
plot(pruned2)
text(pruned2, cex=0.8)
```

![](treemodels_files/figure-html/unnamed-chunk-11-2.png)

Spodintmodel explained approximately 83% of the variance in the data while spodintmodel2 only explained 41%. It is evident that soil order and subgroup greatly influence spodic intensity, but is that useful for prediction? 

Another way to visualize rpart's decison tree and model performance is to use the rpart.plot package. 


```r
rpart.plot(pruned2, extra=3) #extra=3 displays the misclassification rate at the node, expressed as the number of incorrect classifications divided by the total observations in the node; there are many options under the extra setting for classification models
```

![](treemodels_files/figure-html/unnamed-chunk-12-1.png)

```r
rpart.plot(pruned2, extra=103) #adding 100 to the extra setting displays the percentage observations in the node
```

![](treemodels_files/figure-html/unnamed-chunk-12-2.png)

The rpart.plot package is recommended for plotting rpart trees. The examples above all deal with classification trees which result in categorical terminal nodes determined by majority votes. In a regression tree model, terminal nodes reflect the mean of the observations in that node. Let's build a regression tree using the soildata dataset to predict total O horizon thickness. 


```r
spodintmodel3<-rpart(Otot~x+y+Overtype+Underconifer+spodint+ps+drainage+slope+surfacetex+stoniness+depthclass+bedrockdepth+hillslope+tipmound+rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, method = "anova")

printcp(spodintmodel3)
```

```
## 
## Regression tree:
## rpart(formula = Otot ~ x + y + Overtype + Underconifer + spodint + 
##     ps + drainage + slope + surfacetex + stoniness + depthclass + 
##     bedrockdepth + hillslope + tipmound + rainfall + geology + 
##     aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + 
##     landsatb2 + landsatb3 + landsatb7 + maxc100 + maxent + minc100 + 
##     mirref + ndvi + northeastn + northness + northwestn + planc100 + 
##     proc100 + protection + relpos11 + slp50 + solar + tanc75, 
##     data = soildata, method = "anova")
## 
## Variables actually used in tree construction:
## [1] aachn      greenrefl  landsatb3  maxent     spodint    stoniness 
## [7] surfacetex y         
## 
## Root node error: 8990/250 = 35.96
## 
## n= 250 
## 
##          CP nsplit rel error  xerror     xstd
## 1  0.318266      0   1.00000 1.00522 0.164919
## 2  0.111721      1   0.68173 0.69790 0.115889
## 3  0.050419      2   0.57001 0.64985 0.095762
## 4  0.043957      3   0.51959 0.66264 0.106504
## 5  0.031130      4   0.47564 0.64953 0.108236
## 6  0.019235      5   0.44451 0.66186 0.115040
## 7  0.013928      6   0.42527 0.68578 0.115700
## 8  0.010851      7   0.41134 0.69221 0.114970
## 9  0.010394      8   0.40049 0.70219 0.114660
## 10 0.010000      9   0.39010 0.70022 0.114666
```

```r
pruned3<-prune(spodintmodel3, cp=0.050419)
prp(pruned3,type=1,extra=1,branch=1) #prp is another function in the rpart.plot package that has numerous plot customization options
```

![](treemodels_files/figure-html/unnamed-chunk-13-1.png)

```r
printcp(pruned3)
```

```
## 
## Regression tree:
## rpart(formula = Otot ~ x + y + Overtype + Underconifer + spodint + 
##     ps + drainage + slope + surfacetex + stoniness + depthclass + 
##     bedrockdepth + hillslope + tipmound + rainfall + geology + 
##     aachn + dem10m + downslpgra + eastness + greenrefl + landsatb1 + 
##     landsatb2 + landsatb3 + landsatb7 + maxc100 + maxent + minc100 + 
##     mirref + ndvi + northeastn + northness + northwestn + planc100 + 
##     proc100 + protection + relpos11 + slp50 + solar + tanc75, 
##     data = soildata, method = "anova")
## 
## Variables actually used in tree construction:
## [1] spodint    surfacetex
## 
## Root node error: 8990/250 = 35.96
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.318266      0   1.00000 1.00522 0.164919
## 2 0.111721      1   0.68173 0.69790 0.115889
## 3 0.050419      2   0.57001 0.64985 0.095762
```

**Was the majority of the variance in total O horizon thickness captured with the rpart model?**


### Exercise 1: rpart
Using the soildata dataset, construct a rpart model to predict tipmound. Prune the model if necessary and answer the following questions:

1) What are the most important variables for separating tip and mound ratings?
2) What is the cross-validation error of the pruned model?
3) How could you improve this model?

## 8.2 Random Forest
The randomForest algorithm fits hundreds to thousands of CART models to random subsets of input data and combines the trees for prediction. Similarly to rpart, randomForest allows all data types to be used as independent variables, regardless of whether the model is a classification or regression tree. Unlike rpart, the randomForest algorithm does not straight forwardly handle missing values with surrogate splits. There is a function called `rfImpute()` that uses a proximity matrix from the randomForest to populate missing values with either the weighted average of the non-missing observations (weighted by the proximities) for continuous predictors or the category with the largest average proximity for categorical predictors. 

Going back to the soildata dataset, it is also of particular interest to determine what properties best predict total O horizon thickness (Otot). Just like rpart, randomForest has the same basic model function: (y ~ x).


```r
rf<-randomForest(Otot~x+y+Overtype+Underconifer+spodint+ps+drainage+slope+surfacetex+stoniness+depthclass+bedrockdepth+hillslope+tipmound+rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, importance=TRUE) 
#Oi, Oe, Oa, order, subgroup, and epipedon were omitted as independent variables
#importance=TRUE will allow the generation of a variable importance plot

rf # statistical summary
```

```
## 
## Call:
##  randomForest(formula = Otot ~ x + y + Overtype + Underconifer +      spodint + ps + drainage + slope + surfacetex + stoniness +      depthclass + bedrockdepth + hillslope + tipmound + rainfall +      geology + aachn + dem10m + downslpgra + eastness + greenrefl +      landsatb1 + landsatb2 + landsatb3 + landsatb7 + maxc100 +      maxent + minc100 + mirref + ndvi + northeastn + northness +      northwestn + planc100 + proc100 + protection + relpos11 +      slp50 + solar + tanc75, data = soildata, importance = TRUE) 
##                Type of random forest: regression
##                      Number of trees: 500
## No. of variables tried at each split: 13
## 
##           Mean of squared residuals: 21.19538
##                     % Var explained: 41.06
```

```r
plot(rf)  #out of bag (OOB) error rate versus number of trees
```

![](treemodels_files/figure-html/unnamed-chunk-14-1.png)

The rf model, generated using the default number of trees and number of variables tried at each split, explained approximately 40% of the variance and produced a mean square error (sum of squared residuals divided by n) of 21 cm2. If you were to run this same model again, the % variance explained and MSE would change slightly due to the random subsetting and averaging in the randomForest algorithm. 

Recall that the rpart model that we constructed earlier for Otot produced a relative error of ~57%, meaning that the rpart model explained approximately 43% of the variance. In this example, the pruned rpart model performed slightly better (meaning that it was able to explain more variance) than the rf model. 

The defaults for the number of trees (`ntree`) and number of variables tried at each split (`mtry`) may need to be adjusted in the `randomForest` command to explain more variance in the data and to reduce model over-fitting. For most datasets, manually tweaking these parameters and examining the statistical summary is often sufficient. The `tuneRF()` function can be used to determine the optimal `mtry` value, but some users have claimed that this algorithm leads to bias. Feel free to manually tweak the `ntree` and `mtry` settings to see how they effect the overall model performance. 

Another way to assess the rf model is to look at the variable importance plot. 


```r
varImpPlot(rf)
```

![](treemodels_files/figure-html/unnamed-chunk-15-1.png)

```r
importance(rf) #tabular summary
```

```
##                  %IncMSE IncNodePurity
## x             2.99437033     225.63241
## y             2.70123449     352.65839
## Overtype      5.73889583     438.82936
## Underconifer  3.35731786      33.11964
## spodint      19.01618154    1577.31691
## ps            0.44745426      60.51363
## drainage      0.24165434      13.23985
## slope         1.03631886     118.81148
## surfacetex    5.93693023    1028.22926
## stoniness     1.01476242      65.00905
## depthclass   -1.05298237      22.19743
## bedrockdepth  1.50490552      11.31094
## hillslope     4.37570471      58.14830
## tipmound     -0.60217206      32.74371
## rainfall      1.60776378      96.14164
## geology      -1.12423788      21.06029
## aachn         1.43735630     119.92253
## dem10m        2.32156030     155.38747
## downslpgra    1.20924443     100.51875
## eastness     -2.23643429     551.87825
## greenrefl     1.60843858     209.93800
## landsatb1     0.77428603      91.22189
## landsatb2    -0.27315400     110.81490
## landsatb3     0.41860685      58.53299
## landsatb7     6.17884621     474.25724
## maxc100       2.36710030     120.32573
## maxent        3.92961246     504.55316
## minc100       2.40686361     110.33522
## mirref        2.60215441     278.94992
## ndvi         -0.07287648     127.97940
## northeastn    1.54360598     310.16171
## northness    -0.31505280     128.98307
## northwestn    3.32424426     208.22920
## planc100      1.67030708     135.87763
## proc100       2.03608565     120.78686
## protection    3.84496173     139.27790
## relpos11      2.10709209     113.73810
## slp50         4.21102711     101.40035
## solar         2.84346752     128.22621
## tanc75       -0.01805980     164.84606
```

For each tree, each predictor in the OOB sample is randomly permuted and passed through the tree to obtain an error rate (mean square error (MSE) for regression and Gini index for classification). The error rate from the unpermuted OOB is then subtracted from the error rate of the permuted OOB data, and averaged across all trees. When this value is large, it implies that a variable is highly correlated to the dependent variable and is needed in the model. 

In a regression tree analysis, randomForest uses %IncMSE and IncNodePurity to rank variable importance. %IncMSE is simply the average increase in squared residuals of the test set when variables are randomly permuted (little importance = little change in model when variable is removed or added) and IncNodePurity is the increase in homogeneity in the data partitions. In a classification tree analysis, randomForest uses MeanDecreaseAccuracy and MeanDecreaseGini. For MeanDecreaseAccuracy, the more the accuracy of the model decreases due to the addition of a single variable, the more important the variable is deemed. MeanDecreaseGini is a measure of how each variable contributes to the homogeneity of the nodes and leaves.

In the rf model, it is apparent that spodint is the most important variable used in the model, followed by y, landsatb7, Overtype, surfacetex, maxent, hillslope, slp50, rainfall, and protection index. 

### Exercise 2: randomForest
Using the soildata dataset, construct a randomForest model to predict soil order. Hint: remove the one histosol observation before constructing the model (`soildata2<-droplevels(subset(soildata, order!="histosol")) `) and do not include spodint, series, taxon, or subgroup. When finished, answer the following questions:

1) What are the most important variables for separating Inceptisols from Spodosols from Ultisols?
2) What is the out-of-bag error rate?
3) Which soil order was best predicted by the model?

## 8.3 Prediction using Tree-based Models
As with any modeling technique, tree-based models can be used for prediction and can be spatially interpolated using environmental covariates. In order to interpolate a model, R requires that all raster images have a common datum, common cell resolution, are coregistered, and are preferably .img files. The function `stack()` combines all of the rasters into a "raster stack." The `predict()` function is then used in the form of: `predict(rasterstack, fittedmodel, type="")`. Follow along through the example below to interpolate the rpart total O horizon thickness model:


```r
library(raster)
rasters<-stack(list.files(getwd(),pattern="img$",full.names=FALSE)) #combines rasters with a .img file extension stored in the working directory

rasters 

model<-rpart(Otot~maxent+slp50+protection, data=soildata)

predict(rasters,model, type="vector",progress="window",overwrite=TRUE,filename="rpartpredict.img") #type: "vector" for mean response at the node, "prob" for matrix of class probabilities, or "class" for a factor of classifications based on the responses
```

The output raster "rpartpredict.img" can be added and viewed in ArcMap. 

![R GUI image](figure/ch8_predicted.jpg)  


You can also view the interpolated model in R:

```r
rpartpredict <- raster("rpartpredict.img")
plot(rpartpredict, xlab="Easting (m)", ylab="Northing (m)", main="Total O Horizon Thickness (cm)")
```

## 8.4 Summary
Tree-based models are intuitive, quick to run, nonparametric, and are often ideal for exploratory data analysis and prediction. Both rpart and randomForest produce graphical and tabular outputs to aid interpretation. Both packages also perform internal validataion (rpart=10-fold cross validation; randomForest=OOB error estimates) to assess model performance. Tree-based models do require pruning and/or tweaking of model parameters to reduce over-fitting and are unstable in that removing observations (especially outliers) or independent predictors can greatly alter the tree structure. In general,  tree-based models are robust against multicollinearity and low n, high p datasets (low sample size and many predictors).


## 8.5 Additional Reading

Gareth, J., D. Witten, T. Hastie, and R. Tibshirani, 2014. An Introduction to Statistical Learning: with Applications in R. Springer, New York. [http://www-bcf.usc.edu/~gareth/ISL/](http://www-bcf.usc.edu/~gareth/ISL/)

Rad, M.R.P., N. Toomanian, F. Khormali, C.W. Brungard, C.B. Komaki, and P. Bogaert. 2014. Updating soil survey maps using random forest and conditioned Latin hypercube sampling in the loess derived soils of northern Iran. Geoderma. 232-234: 97-106. 

