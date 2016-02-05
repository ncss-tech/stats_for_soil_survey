# Chapter 8: Tree-based Models
Katey Yoast  
January 14, 2016  

##Introduction
Tree-based models are a supervised machine learning method commonly used in soil survey and ecology for exploratory data analysis and prediction due to their simplistic nonparametric design. Instead of fitting a model to the data, tree-based models recursively partition the data into increasingly homogenous groups based on values that minimize a loss function (such as Sum of Squared Errors (SSE) for regression or Gini Index for classification) (McBratney et al., 2003). The two most common packages for generating tree-based models in R are rpart and randomForest. The rpart package creates a regression or classification tree based on binary splits that maximize homogeneity and minimize impurity. The output is a single decision tree that can be further "pruned" or trimmed back using the cross-validation error statistic to reduce over-fitting. The randomForest package is similar to rpart, but is double random in that each node is split using a random subset of predictors AND observations at each node and this process is repeated hundreds of times (as specified by the user). Unlike rpart, random forests do not produce a graphical decision tree since the predictions are averaged across hundreds or thousands of trees. Instead, random forests produce a variable importance plot and a tabular statistical summary. 
<br/>

##8.1 Classification and Regression Trees (CART)
The basic function for all CART models is (y ~ x), where y is the dependent variable to be predicted from x, a set of independent variables. If the dependent variable (y) is numeric, the resulting tree will be a regression tree. Conversely, if the dependent variable (y) is categorical, the resulting tree will be a classification tree. The rpart package allows all data types to be used as independent variables, regardless of whether the model is a classification or regression tree. The rpart algorithm ignores missing values when determining the quality of a split and uses surrogate splits to determine if observation(s) with missing data is best split left or right. If an observation is missing all surrogate splits, then the observation(s) is sent to the child node with the largest relative frequency (Feelders, 1999).

Assuming that the rpart and randomForest packages are already installed on your machine, simply load the packages using the library( ) function.


```r
library(rpart)
library(randomForest)
library(rpart.plot)

file<-'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/wv_transect_editedforR.csv'
download.file(file, destfile = "soildata.csv")
soildata<-read.csv("soildata.csv", header=TRUE, sep=",")
```

Review the data structure to ensure that the data were imported correctly into R.

```r
str(soildata)
```

```
## 'data.frame':	250 obs. of  58 variables:
##  $ FID         : int  0 1 2 3 4 5 6 7 8 9 ...
##  $ x           : num  -79.7 -79.7 -79.7 -79.8 -79.7 ...
##  $ y           : num  38.6 38.6 38.6 38.6 38.6 ...
##  $ NASIS_Pedo  : Factor w/ 236 levels " ","10WV075HLRSP006",..: 108 103 105 93 116 112 96 75 83 98 ...
##  $ overstory   : Factor w/ 199 levels "--","ACRU,ACSA3,PRSE2,BELE",..: 184 181 139 17 95 108 45 24 93 182 ...
##  $ undrstry    : Factor w/ 114 levels "--","(plantation)",..: 47 56 42 11 37 36 9 3 6 46 ...
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
##  $ aspect      : Factor w/ 124 levels "--","1","11",..: 32 3 121 124 1 42 7 108 77 14 ...
##  $ hillslope   : Factor w/ 4 levels "backslope","footslope",..: 1 1 1 1 1 1 1 1 3 1 ...
##  $ slopeshape  : Factor w/ 12 levels "","cc","cl","cv",..: 7 6 6 9 10 7 9 7 10 6 ...
##  $ tipmound    : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ musym       : Factor w/ 14 levels "BgC","BgD","BgE",..: 14 14 14 14 14 14 14 14 13 14 ...
##  $ asym        : Factor w/ 3 levels "WV075","WV083",..: 1 1 1 1 1 1 1 1 1 1 ...
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

The example dataset, soildata, consists of 250 observations and 58 variables that were collected in the field or derived from geospatial data to identify spodic soil properties in West Virginia. Of particular interest is determining best splits for spodic intensity (relative spodicity index). As you can see in that data structure, R interpreted the spodint field as numeric. Since spodint is an index, it will need to be changed to a factor and then to an ordered factor. 


```r
soildata$spodint<-as.factor(soildata$spodint)
soildata$spodint<-ordered(soildata$spodint)
```

If you wanted to create a classification tree for spodint using all of the variables, you would simply type: `rpart(spodint ~ ., data=soildata)`. Since the soildata dataset contains variables such as FID and NASIS_Pedo that are site observation labels, it is not wise to include all variables in the model. Instead, you want to define which variables the model should use. 


```r
spodintmodel<-rpart(spodint~x+y+Overtype+Underconifer+Oi+Oe+Oa+Otot+epipedon+subgroup+order+ps+drainage+slope+surfacetex+stoniness+depthclass+bedrockdepth+hillslope+tipmound+rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, method = "class")

spodintmodel
```

```
## n= 250 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 250 162 0 (0.35 0.084 0.26 0.024 0.28)  
##   2) order=inceptisol,ultisol 179  91 0 (0.49 0.12 0.36 0.028 0)  
##     4) subgroup=aeric,aquic,typic 108  20 0 (0.81 0.18 0.0093 0 0)  
##       8) slp50>=5.61029 100  14 0 (0.86 0.13 0.01 0 0) *
##       9) slp50< 5.61029 8   2 0.5 (0.25 0.75 0 0 0) *
##     5) subgroup=spodic 71   7 1 (0 0.028 0.9 0.07 0) *
##   3) order=histosol,spodosol 71   1 2 (0 0 0 0.014 0.99) *
```

```r
plot(spodintmodel)
text(spodintmodel, cex=0.8) ##cex is text size
```

![](treemodels_files/figure-html/unnamed-chunk-4-1.png) 

When soil order and subgroup are included in the spodint model, they are the two most important variables for seperating spodic intensity. This makes sense given the ratings for spodic intensity: 0 = non spodic, 0.5 = Bs horizon, 1 = spodic subgroup, 1.5 = spodic subgroup OR Spodosol, 2 = Spodosol. Let's omit soil order and subgroup to see what other factors best seperate spodint.  


```r
spodintmodel2<-rpart(spodint~x+y+Overtype+Underconifer+Oi+Oe+Oa+Otot+epipedon+ps+drainage+slope+surfacetex+stoniness+depthclass+bedrockdepth+hillslope+tipmound+rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, method = "class")

spodintmodel2
```

```
## n= 250 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##   1) root 250 162 0 (0.35 0.084 0.26 0.024 0.28)  
##     2) Oa< 3.5 184 104 0 (0.43 0.11 0.31 0.033 0.11)  
##       4) proc100< -0.1166665 40  10 0 (0.75 0.05 0.05 0.025 0.12) *
##       5) proc100>=-0.1166665 144  89 1 (0.35 0.12 0.38 0.035 0.11)  
##        10) eastness>=-0.0713796 66  32 0 (0.52 0.12 0.3 0.015 0.045)  
##          20) aachn>=23.9057 57  24 0 (0.58 0.12 0.23 0.018 0.053)  
##            40) slope>=41 15   1 0 (0.93 0 0.067 0 0) *
##            41) slope< 41 42  23 0 (0.45 0.17 0.29 0.024 0.071)  
##              82) slope>=10.5 35  17 0 (0.51 0.14 0.34 0 0)  
##               164) landsatb2>=0.0555786 27  10 0 (0.63 0.11 0.26 0 0)  
##                 328) minc100< 0.0260241 17   3 0 (0.82 0.12 0.059 0 0) *
##                 329) minc100>=0.0260241 10   4 1 (0.3 0.1 0.6 0 0) *
##               165) landsatb2< 0.0555786 8   3 1 (0.12 0.25 0.62 0 0) *
##              83) slope< 10.5 7   4 2 (0.14 0.29 0 0.14 0.43) *
##          21) aachn< 23.9057 9   2 1 (0.11 0.11 0.78 0 0) *
##        11) eastness< -0.0713796 78  43 1 (0.21 0.13 0.45 0.051 0.17)  
##          22) surfacetex=cnhosil,cnl,cnsil,cnxl,cnxsl,flsil,grl,grvl,hosil,sil 62  31 1 (0.21 0.16 0.5 0.065 0.065)  
##            44) surfacetex=cnhosil,cnsil,flsil,grl,hosil,sil 48  28 1 (0.27 0.17 0.42 0.083 0.062)  
##              88) tipmound< 0.5 19   9 0 (0.53 0.11 0.26 0 0.11) *
##              89) tipmound>=0.5 29  14 1 (0.1 0.21 0.52 0.14 0.034) *
##            45) surfacetex=cnl,cnxl,cnxsl,grvl 14   3 1 (0 0.14 0.79 0 0.071) *
##          23) surfacetex=cnvhol,cnvl,cnvsil,grsil,hol,l 16   7 2 (0.19 0 0.25 0 0.56) *
##     3) Oa>=3.5 66  17 2 (0.12 0.015 0.12 0 0.74)  
##       6) Otot< 8.5 8   4 0 (0.5 0.12 0.25 0 0.12) *
##       7) Otot>=8.5 58  10 2 (0.069 0 0.1 0 0.83) *
```

```r
plot(spodintmodel2)
text(spodintmodel2, cex=0.8)
```

![](treemodels_files/figure-html/unnamed-chunk-5-1.png) 

The decision tree got more complex when soil order and subgroup were omitted. Also, 0.5 and 1.5 were omitted from the terminal nodes. One way to compare the two models is to use the function `printcp()`:


```r
printcp(spodintmodel)
```

```
## 
## Classification tree:
## rpart(formula = spodint ~ x + y + Overtype + Underconifer + Oi + 
##     Oe + Oa + Otot + epipedon + subgroup + order + ps + drainage + 
##     slope + surfacetex + stoniness + depthclass + bedrockdepth + 
##     hillslope + tipmound + rainfall + geology + aachn + dem10m + 
##     downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + 
##     landsatb3 + landsatb7 + maxc100 + maxent + minc100 + mirref + 
##     ndvi + northeastn + northness + northwestn + planc100 + proc100 + 
##     protection + relpos11 + slp50 + solar + tanc75, data = soildata, 
##     method = "class")
## 
## Variables actually used in tree construction:
## [1] order    slp50    subgroup
## 
## Root node error: 162/250 = 0.648
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.432099      0   1.00000 1.00000 0.046614
## 2 0.395062      1   0.56790 0.59877 0.047561
## 3 0.024691      2   0.17284 0.17901 0.031254
## 4 0.010000      3   0.14815 0.19136 0.032168
```

```r
printcp(spodintmodel2)
```

```
## 
## Classification tree:
## rpart(formula = spodint ~ x + y + Overtype + Underconifer + Oi + 
##     Oe + Oa + Otot + epipedon + ps + drainage + slope + surfacetex + 
##     stoniness + depthclass + bedrockdepth + hillslope + tipmound + 
##     rainfall + geology + aachn + dem10m + downslpgra + eastness + 
##     greenrefl + landsatb1 + landsatb2 + landsatb3 + landsatb7 + 
##     maxc100 + maxent + minc100 + mirref + ndvi + northeastn + 
##     northness + northwestn + planc100 + proc100 + protection + 
##     relpos11 + slp50 + solar + tanc75, data = soildata, method = "class")
## 
## Variables actually used in tree construction:
##  [1] aachn      eastness   landsatb2  minc100    Oa         Otot      
##  [7] proc100    slope      surfacetex tipmound  
## 
## Root node error: 162/250 = 0.648
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.253086      0   1.00000 1.00000 0.046614
## 2 0.058642      1   0.74691 0.76543 0.048799
## 3 0.037037      3   0.62963 0.82099 0.048701
## 4 0.030864      4   0.59259 0.77778 0.048799
## 5 0.018519      5   0.56173 0.79630 0.048776
## 6 0.015432      6   0.54321 0.81481 0.048724
## 7 0.012346      8   0.51235 0.82099 0.048701
## 8 0.010000     12   0.45679 0.82716 0.048674
```

The `printcp()` funtion generates a cost complexity parameter table that provides the complexity parameter value (CP), relative model error (1 - relative error = ~variance explained), error estimated from a 10-fold cross validation (xerror), and the standard error of the xerror (xstd). The CP values control the size of the tree; the greater the CP value, the fewer the number of splits in the tree. To determine the optimal CP value, rpart automatically performs a 10-fold cross validation. The optimal size of the tree is generally the row in the CP table that minimizes all error with the fewest branches. Another way to determine the optimal tree size is to use the `plotcp()` function. This will plot the xerror versus cp value and tree size. 


```r
plotcp(spodintmodel)
```

![](treemodels_files/figure-html/unnamed-chunk-7-1.png) 

```r
plotcp(spodintmodel2)
```

![](treemodels_files/figure-html/unnamed-chunk-7-2.png) 

The optimal CP value is 0.099 for spodintmodel and 0.034 for spodintmodel2. Since both spodic intensity models overfit that data, they will need to be pruned using the `prune()` function.


```r
pruned<-prune(spodintmodel, cp=0.099)
printcp(pruned)
```

```
## 
## Classification tree:
## rpart(formula = spodint ~ x + y + Overtype + Underconifer + Oi + 
##     Oe + Oa + Otot + epipedon + subgroup + order + ps + drainage + 
##     slope + surfacetex + stoniness + depthclass + bedrockdepth + 
##     hillslope + tipmound + rainfall + geology + aachn + dem10m + 
##     downslpgra + eastness + greenrefl + landsatb1 + landsatb2 + 
##     landsatb3 + landsatb7 + maxc100 + maxent + minc100 + mirref + 
##     ndvi + northeastn + northness + northwestn + planc100 + proc100 + 
##     protection + relpos11 + slp50 + solar + tanc75, data = soildata, 
##     method = "class")
## 
## Variables actually used in tree construction:
## [1] order    subgroup
## 
## Root node error: 162/250 = 0.648
## 
## n= 250 
## 
##        CP nsplit rel error  xerror     xstd
## 1 0.43210      0   1.00000 1.00000 0.046614
## 2 0.39506      1   0.56790 0.59877 0.047561
## 3 0.09900      2   0.17284 0.17901 0.031254
```

```r
plot(pruned)
text(pruned, cex=0.8)
```

![](treemodels_files/figure-html/unnamed-chunk-8-1.png) 

```r
pruned2<-prune(spodintmodel2, cp=0.034)
printcp(pruned2)
```

```
## 
## Classification tree:
## rpart(formula = spodint ~ x + y + Overtype + Underconifer + Oi + 
##     Oe + Oa + Otot + epipedon + ps + drainage + slope + surfacetex + 
##     stoniness + depthclass + bedrockdepth + hillslope + tipmound + 
##     rainfall + geology + aachn + dem10m + downslpgra + eastness + 
##     greenrefl + landsatb1 + landsatb2 + landsatb3 + landsatb7 + 
##     maxc100 + maxent + minc100 + mirref + ndvi + northeastn + 
##     northness + northwestn + planc100 + proc100 + protection + 
##     relpos11 + slp50 + solar + tanc75, data = soildata, method = "class")
## 
## Variables actually used in tree construction:
## [1] aachn    eastness Oa       proc100 
## 
## Root node error: 162/250 = 0.648
## 
## n= 250 
## 
##         CP nsplit rel error  xerror     xstd
## 1 0.253086      0   1.00000 1.00000 0.046614
## 2 0.058642      1   0.74691 0.76543 0.048799
## 3 0.037037      3   0.62963 0.82099 0.048701
## 4 0.034000      4   0.59259 0.77778 0.048799
```

```r
plot(pruned2)
text(pruned2, cex=0.8)
```

![](treemodels_files/figure-html/unnamed-chunk-8-2.png) 

Spodintmodel explained approximately 83% of the variance in the data while spodintmodel2 only explained 41%. It is evident that soil order and subgroup greatly influence spodic intensity, but is that useful for prediction? 

Another way to visualize rpart's decison tree and model performance is to use the rpart.plot package. 


```r
rpart.plot(pruned2, extra=3) #extra=3 displays the missclassification rate at the node, expressed as the number of incorrect classifications divided by the total observations in the node; there are many options under the extra setting for classification models
```

![](treemodels_files/figure-html/unnamed-chunk-9-1.png) 

```r
rpart.plot(pruned2, extra=103) #adding 100 to the extra setting displays the percentage observations in the node
```

![](treemodels_files/figure-html/unnamed-chunk-9-2.png) 

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
## 1  0.318266      0   1.00000 1.01235 0.164428
## 2  0.111721      1   0.68173 0.71128 0.115823
## 3  0.050419      2   0.57001 0.64045 0.096733
## 4  0.043957      3   0.51959 0.66732 0.109424
## 5  0.031130      4   0.47564 0.67051 0.111011
## 6  0.019235      5   0.44451 0.71322 0.120290
## 7  0.013928      6   0.42527 0.72665 0.121339
## 8  0.010851      7   0.41134 0.74305 0.122376
## 9  0.010394      8   0.40049 0.76132 0.122566
## 10 0.010000      9   0.39010 0.76083 0.122577
```

```r
pruned3<-prune(spodintmodel3, cp=0.050419)
prp(pruned3,type=1,extra=1,branch=1) #prp is another function in the rpart.plot package that has numerous plot customization options
```

![](treemodels_files/figure-html/unnamed-chunk-10-1.png) 

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
## 1 0.318266      0   1.00000 1.01235 0.164428
## 2 0.111721      1   0.68173 0.71128 0.115823
## 3 0.050419      2   0.57001 0.64045 0.096733
```


<br/>

##8.2 Random Forest
The randomForest algorithm fits hundreds to thousands of CART models to random subsets of input data and combines the trees for prediction. Similarly to rpart, randomForest allows all data types to be used as independent variables, regardless of whether the model is a classification or regression tree. Unlike rpart, the randomForest algorithm does not straight forwardly handle missing values with surrogate splits. There is a function called `rfImpute()` that uses a proximity matrix from the randomForest to populate missing values with either the weighted average of the non-missing observations (weighted by the proximities) for continuous predictors or the category with the largest average proximity for categorical predictors. 

Going back to the soildata dataset, it is also of particular interest to determine what properties best predict total O horizon thickness (Otot). Just like rpart, randomForest has the same basic model function: (y ~ x).


```r
rf<-randomForest(Otot~x+y+Overtype+Underconifer+spodint+ps+drainage+slope+surfacetex+stoniness+depthclass+bedrockdepth+hillslope+tipmound+rainfall+geology+aachn+dem10m+downslpgra+eastness+greenrefl+landsatb1+landsatb2+landsatb3+landsatb7+maxc100+maxent+minc100+mirref+ndvi+northeastn+northness+northwestn+planc100+proc100+protection+relpos11+slp50+solar+tanc75, data=soildata, importance=TRUE) 
##Oi, Oe, Oa, order, subgroup, and epipedon were omitted as independent variables
##importance=TRUE will allow the generation of a variable importance plot

rf ## statistical summary
```

```
## 
## Call:
##  randomForest(formula = Otot ~ x + y + Overtype + Underconifer +      spodint + ps + drainage + slope + surfacetex + stoniness +      depthclass + bedrockdepth + hillslope + tipmound + rainfall +      geology + aachn + dem10m + downslpgra + eastness + greenrefl +      landsatb1 + landsatb2 + landsatb3 + landsatb7 + maxc100 +      maxent + minc100 + mirref + ndvi + northeastn + northness +      northwestn + planc100 + proc100 + protection + relpos11 +      slp50 + solar + tanc75, data = soildata, importance = TRUE) 
##                Type of random forest: regression
##                      Number of trees: 500
## No. of variables tried at each split: 13
## 
##           Mean of squared residuals: 21.53648
##                     % Var explained: 40.11
```

```r
plot(rf)  ##out of bag (OOB) error rate versus number of trees
```

![](treemodels_files/figure-html/unnamed-chunk-11-1.png) 

The rf model, generated using the default number of trees and number of variables tried at each split, explained approximately 40% of the variance and produced a mean square error (sum of squared residuals divided by n) of 21 cm2. If you were to run this same model again, the % variance explained and MSE would change slightly due to the random subsetting and averaging in the randomForest algorithm. 

Recall that the rpart model that we constructed earlier for Otot produced a relative error of ~57%, meaning that the rpart model explained approximately 43% of the variance. In this example, the pruned rpart model performed slightly better (meaning that it was able to explain more variance) than the rf model. 

The defaults for the number of trees (`ntree`) and number of variables tried at each split (`mtry`) may need to be adjusted in the `randomForest` command to explain more variance in the data and to reduce model over-fitting. For most datasets, manually tweaking these parameters and examining the statistical summary is often sufficient. The `tuneRF()` function can be used to determine the optimal `mtry` value, but some users have claimed that this algorithm leads to bias. Feel free to manually tweak the `ntree` and `mtry` settings to see how they effect the overall model performance. 

Another way to assess the rf model is to look at the variable importance plot. 


```r
varImpPlot(rf)
```

![](treemodels_files/figure-html/unnamed-chunk-12-1.png) 

```r
importance(rf) ##tabular summary
```

```
##                 %IncMSE IncNodePurity
## x             2.3402075    208.225725
## y             8.0547313    357.035584
## Overtype      5.5773169    373.496640
## Underconifer  3.0816676     38.407187
## spodint      16.8839766   1604.992162
## ps           -0.4547157     56.237939
## drainage     -1.1496095      9.978282
## slope         1.1651088    112.862566
## surfacetex    5.2797175    925.889000
## stoniness     0.7847929     61.850531
## depthclass   -0.3997968     20.723625
## bedrockdepth -0.6808380     10.074805
## hillslope     3.3853060     63.933024
## tipmound      0.3121377     27.700986
## rainfall      3.3941664     68.296713
## geology      -0.1759803     21.904457
## aachn         1.8944733    105.214904
## dem10m        2.4845329    155.905945
## downslpgra    1.5783820    112.941075
## eastness     -3.4403423    663.764110
## greenrefl     4.1821061    193.920905
## landsatb1     0.6446869     93.107404
## landsatb2     0.7437572    115.432093
## landsatb3    -0.6274782     60.876267
## landsatb7     5.1073863    531.455677
## maxc100       1.7806041    131.550859
## maxent        4.5984714    458.330852
## minc100       1.4914124    112.720302
## mirref        2.7281830    273.539674
## ndvi         -1.7066503    133.131298
## northeastn    1.4554500    351.814350
## northness    -1.6660534    125.707183
## northwestn    2.8456009    244.848156
## planc100      1.9275690    144.313284
## proc100       0.1501949    116.744830
## protection    2.5400991    158.445559
## relpos11      2.0639934    121.286983
## slp50         2.8449725    117.282318
## solar         2.1918262    134.981632
## tanc75       -0.1597071    177.459899
```

For each tree, each predictor in the OOB sample is randomly permuted and passed through the tree to obtain an error rate (mean square error (MSE) for regression and Gini index for classification). The error rate from the unpermuted OOB is then subtracted from the error rate of the permuted OOB data, and averaged across all trees. When this value is large, it implies that a variable is highly correlated to the dependent variable and is needed in the model. 

In a regression tree analysis, randomForest uses %IncMSE and IncNodePurity to rank variable importance. %IncMSE is simply the average increase in squared residuals of the test set when variables are randomly permuted (little importance = little change in model when variable is removed or added) and IncNodePurity is the increase in homogeneity in the data partitions. In a classification tree analysis, randomForest uses MeanDecreaseAccuracy and MeanDecreaseGini. For MeanDecreaseAccuracy, the more the accuracy of the model decreases due to the addition of a single variable, the more important the variable is deemed. MeanDecreaseGini is a measure of how each variable contributes to the homogeneity of the nodes and leaves.

In the rf model, it is apparent that spodint is the most important variable used in the model, followed by y, landsatb7, Overtype, surfacetex, maxent, hillslope, slp50, rainfall, and protection index. 

<br/>

##8.3 Prediction using Tree-based Models
As with any modeling technique, tree-based models can be used for prediction and can be spatially interpolated using environmental covariates. In order to spatially interpolate the model using environmental covariates, R requires a raster stack. Below is an example of how to perform a raster stack and use the `predict()` function for our randomForest regression model:


##8.4 Summary
Tree-based models are intuitive, quick to run, nonparametric, and are often ideal for exploratory data analysis and prediction. Both rpart and randomForest produce graphical and tabular outputs to aid interpretation. Both packages also perform internal validataion (rpart=10-fold cross validation; randomForest=OOB error estimates) to assess model performance. Tree-based models do require pruning and/or tweaking of model parameters to reduce over-fitting and are unstable in that removing observations (especially outliers) or independent predictors can greatly alter the tree structure. In general,  tree-based models are robust against multicollinearity and low n, high p datasets (low sample size and many predictors). 
