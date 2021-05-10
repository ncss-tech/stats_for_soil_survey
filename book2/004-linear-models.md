---
  output: html_document
  editor_options: 
    chunk_output_type: console
---

# Linear Regression

![Statistics for pedologists course banner image](static-figures/logo.jpg)



## Introduction

Linear regression models the linear relationship between a response variable (y) and an predictor variable (x). 

$y = \alpha + \beta x + e$

Where:

  * $y$ = the dependent variable

  * $\alpha$ = the intercept of the fitted line

  * $\beta$ = the Regression coefficient, i.e. slope of the fitted line. Strong relationships will have high values.

  * $x$ = the independent variable (aka explanatory or predictor variable(s) )

  * $e$ = the error term

Linear regression has been used for soil survey applications since the early 1900s when Briggs and McLane (1907) developed a pedotransfer function to estimate the wilting coefficient as a function of soil particle size. 

Wilting coefficient = 0.01(sand) + 0.12(silt) + 0.57(clay)

When more than one independent variable is used in the regression, it is referred to as multiple linear regression. In regression models, the response (or dependent) variable must always be continuous. The predictor (or independent) variable(s) can be continuous or categorical. In order to use linear regression or any linear model, the errors (i.e. residuals) must be normally distributed. Most environmental data are skewed and require transformations to the response variable (such as square root or log) for use in linear models. Normality can be assessed using a QQ plot or histogram of the residuals.


## Linear Regression Example

Now that we've got some of the basic theory out of the way we'll move on to a real example, and address any additional theory where it relates to specific steps in the modeling process. The example selected for this chapter comes from the Mojave desert. The landscape is composed primarily of closed basins ringed by granitic hills and mountains (Peterson, 1981). The problem tackled here is modeling mean annual air temperature as a function of PRISM, digital elevation model (DEM) and Landsat derivatives.

This climate study began in 1998 as part of a national study run by the National Lab and led by Henry Mount (Mount and Paetzold, 2002). The objective was to determine if the hyperthermic line was consistent across the southern US, which at the time was thought to be ~3000 in elevation. Up until 2015 their were 77 active MAST sites, and 12 SCAN sites throughout MLRA 30 and 31.

For more details see the "MLRA 30 - Soil Climate Study - Soil Temperature" project in NASIS, on [GitHub](https://github.com/smroecker/mast-mojave), or by [Roecker et al., 2012](https://www.researchgate.net/publication/331977150_Modeling_and_Application_of_Soil_Temperature_in_the_Mojave_and_Lower_Colorado_Deserts_of_California/citations).


```r
library(soilDB)

prj <- get_project_from_NASISWebReport(mlrassoarea = "8-VIC", fiscalyear = 2015)
```

```
## Loading required namespace: rvest
```

```r
subset(prj, projectname == "MLRA 30 - Soil Climate Study - Soil Temperature")
```

```
## # A tibble: 1 x 30
##   mlrassoarea nonmlrassaarea mlraarea projecttypename fiscalyear
##   <chr>       <chr>             <int> <chr>                <int>
## 1 8-VIC       <NA>                 30 MLRA                  2015
## # ... with 25 more variables: fiscalyear_goaled <int>, projectiid <int>,
## #   uprojectid <chr>, projectname <chr>, projectapprovedflag <lgl>,
## #   projectconcerntypename <chr>, projectdesc <chr>, n_projectdesc <int>,
## #   n_areasymbol <int>, n_nationalmusym <int>, n_spatial <int>,
## #   date_start <date>, date_complete <date>, date_qc <date>,
## #   date_qa_start <date>, date_qa_complete <date>, date_qc_spatial <date>,
## #   date_qa_spatial <date>, pl_username <chr>, qa_username <chr>,
## #   muacres <int>, acre_landcat <int>, acre_goal <int>, acre_progress <int>,
## #   pct_progress <int>
```

In addition to the 11-IND MAST modeling efforts there has also been two published studies on the Mojave. The first was by Schmidlin et al. (1983) who examined both the Great Basin and Mojave Deserts in Nevada. The second was by Bai et al. (2010) who examined the Mojave Desert in California. Both studies developed regression models using elevation, but Schmidlin et al. (1983) also incorporated latitude. The results from Bai et al. (2010) displayed considerably larger areas of hyperthermic soils than Schmidlin et al. (1983). This made be due to the unconventional method used by Bai et al. (2010) to measure MAST.


## Data

### Tidy Raw Files

Typically the data for MAST and other monitoring projets is downloaded annually. However, some sites now are online and can be accessed remotely. Eitherway prior to loading the information into a proper database it is best to store the data in txt files (Excel files are discouraged). If the data is organized and the filenames encode the metadata, an example such as the one below can be used to efficiently import them into R and combine them into a data frame.


```r
p <- "D:/projects/soilTemperatureMonitoring/data/rawTxtFilesClean"
setwd(p)

# get file names of HOBO temp data
files <- list.files()

# read files
l <- lapply(files, function(x) {

  # extract the file name
  fileName = strsplit(x, '[.]')[[1]][1]
  # parse the siteid from the file name
  siteid   = strsplit(x, '_')[[1]][1]

  cat(paste("working on", fileName, "\n"))
  f = paste0(p, "/", x)
  # read the files
  f = read.table(file = f, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  f$siteid <- siteid
  names(f)[1:3] <- c("date","tempF","tempC")

  f$tempF <-as.numeric(f$tempF)
  f$tempC <-as.numeric(f$tempC)

  vars = c("date", "siteid", "tempF", "tempC")
  f    = f[vars]
  })

mastSeries_df <- do.call("rbind", l)


# save cached copy
save(mastSeries_df, file = "D:/projects/soilTemperatureMonitoring/data/R/mastSeries.Rdata")

length(unique(mastSeries_df$siteid))
```


### Henry Mount Database

The Henry Mount Database already has 59 of the sites from the Mojave. The full data set however has 68 sites.


```r
library(soilDB)

f <- fetchHenry(sso = "8-VIC")
length(unique(f$sensors$user_site_id))
```


### Aggregate Time Series


```r
# load cached versions
load(file = "D:/projects/soilTemperatureMonitoring/data/R/mastSeries.Rdata")


# Plot sites visually inspect for flat lines and spikes
test <- subset(mastSeries_df, site == "JTNP08")
test.zoo <- read.zoo(test[,c(1,3)],format = "%m/%d/%y %H:%M:%S", tz = "GMT")
plot(test.zoo, ylab = "tempF")


# Aggregate by Year, Month, and Julian day (i.e. 1-365, 366 for leap years)
ms.df <- mastSeries_df
ms.df$date <- as.POSIXlt(ms.df$date, format="%m/%d/%y %H:%M:%S")
ms.df$day  <- as.character(format(ms.df$date, "%m/%d/%y"))
ms.df$Jday <- as.integer(format(ms.df$date, "%j"))

# compute number of days per site
ms.D.df <- aggregate(tempF ~ site + day, data = ms.df, FUN = mean, na.action = na.exclude)
ms.D.df <- aggregate(day ~ site, data = ms.D.df, function(x) sum(!is.na(x)))
names(ms.D.df) <- c("siteid","numDays")

# compute mast per year
ms.Jd.df <- aggregate(tempF ~ siteid + Jday, data = ms.df, mean)
mastSites.df <- aggregate(tempF ~ siteid, data = ms.Jd.df, mean)

# merge mast & numDays
mastSites.df <- merge(mastSites.df, ms.D.df, by = "siteid")
write.csv(mastSites.df, "mastSites.csv")
```


### Final Dataset

Since the Henry Mount database is incomplete we will procede with the aggregation from the txt files.


```r
# Read tempC data
setwd("D:/projects/soilTemperatureMonitoring/data/R")

sites_df <- read.csv("HOBO_List_2013_0923_master.csv")
mast_df  <- read.csv("mastSites.csv")

mast_df <- merge(mast_df, sites_df, by = "siteid")
vars <- c("siteid", "tempF", "numDays", "utmeasting", "utmnorthing")
mast_df <- mast_df[vars]
mast_df$tempC <- (mast_df$tempF - 32) * (5 / 9)
```


## Spatial data

### Plot Coordinates

Where do our points plot? To start we need to convert them to a spatial object first. Then we can create an interactive we map using `mapview`. Also, if we wish we can also export the locations as a Shapefile.


```r
library(sf)
library(mapview)
library(dplyr)

githubURL <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/ch7_data_v2.Rdata"
load(url(githubURL))


# convert to sites to a spatial object
mast_sf <- st_as_sf(mast_df,
                    coords = c("utmeasting", "utmnorthing"),
                    crs = 26911
                    ) %>%
# reproject
st_transform(crs = 4326)

# reduce precision
mast_sf2 <- st_as_sf(as.data.frame(round(st_coordinates(mast_sf), 1)),
                     coords = c("X", "Y"),
                     crs = 4326
                     )

# reproject
mast_sf <- st_transform(mast_sf, 5070)


# MLRAs
mlra <- read_sf(dsn = "D:/geodata/soils/mlra_a_mbr.shp", layer = "mlra_a_mbr") %>% 
  st_transform(crs = 4326) %>%
  filter(MLRARSYM %in% 30:31)

# plot
mapview(mlra, fill = NA) +
  mapview(mast_sf2)
```


### Extracting Spatial Data

Prior to any spatial analysis or modeling, you will need to develop a suite of geodata files that can be intersected with your field data locations. This is, in and of itself a difficult task and should be facilitated by your Regional GIS Specialist. The geodata files typically used would consist of derivatives from a DEM or satellite imagery, and a 'good' geology map. Prior to any prediction it is also necessary to ensure the geodata files have the same projection, extent, and cell size. Once we have the necessary files we can construct a list in R of the file names and paths, read the geodata into R, and then extract the geodata values where they intersect with field data.

As you can see below their are numerous variables we could inspect.


```r
library(raster)

# set file path
folder <- "D:/geodata/project_data/R8-VIC/"

files <- c(elev    = "ned30m_8VIC_elev5.tif",
           slope   = "ned30m_8VIC_slope5.tif",
           aspect  = "ned30m_8VIC_aspect5.tif",
           twi     = "ned30m_8VIC_wetness.tif",
           solar   = "ned30m_8VIC_solar.tif",
           solarcv = "ned30m_8VIC_solarcv.tif",
           tc      = "landsat30m_8VIC_tc123.tif",
           precip  = "prism30m_8VIC_ppt_1981_2010_annual_mm.tif",
           temp    = "prism30m_8VIC_tmean_1981_2010_annual_C.tif"
           )

# combine the folder directory and file names
geodata_f <- paste0(folder, files) 
names(geodata_f) <- names(files)

# Create a raster stack
geodata_r <- stack(geodata_f)

# Extract the geodata and add to a data frame
data <- raster::extract(geodata_r, as(mast_sf, "Spatial"), sp = TRUE)@data

# convert aspect
data$northness <- abs(180 - data$aspect)

# random sample
vars <- c("elev", "temp", "precip", "solar", "tc_1", "twi")
idx <- which(names(geodata_r) %in% vars)
geodata_s <- sampleRegular(geodata_r[[idx]], size = 5000)

# cache files
save(data, mast_df, mast_sf2, mlra, geodata_s, file = "C:/workspace2/github/ncss-tech/stats_for_soil_survey/data/ch7_data_v2.Rdata")
```


## Exploratory Data Analysis (EDA)

Generally before we begin modeling it is good to explore the data. By examining a simple summary we can quickly see the breakdown of our data. It is important to look out for missing or improbable values. Probably the easiest way to identify pecularities in the data is to plot it.


```r
githubURL <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/ch7_data_v2.Rdata"
load(url(githubURL))

summary(data)
```

```
##        siteid       tempF          numDays         tempC      
##  Cheme01  : 1   Min.   :37.58   Min.   : 363   Min.   : 3.10  
##  Clark01  : 1   1st Qu.:65.29   1st Qu.:1831   1st Qu.:18.50  
##  DEVA01   : 1   Median :69.03   Median :2918   Median :20.57  
##  DEVA02   : 1   Mean   :67.88   Mean   :2465   Mean   :19.93  
##  DEVA03   : 1   3rd Qu.:74.70   3rd Qu.:3397   3rd Qu.:23.72  
##  Jawbone01: 1   Max.   :83.78   Max.   :4159   Max.   :28.77  
##  (Other)  :62                                                 
##       elev             slope            aspect             twi        
##  Min.   : -80.02   Min.   : 0.435   Min.   :  7.433   Min.   : 8.519  
##  1st Qu.: 703.60   1st Qu.: 3.221   1st Qu.: 39.861   1st Qu.: 9.502  
##  Median : 946.79   Median : 5.667   Median :113.600   Median :12.947  
##  Mean   :1083.04   Mean   :14.184   Mean   :129.911   Mean   :13.019  
##  3rd Qu.:1489.26   3rd Qu.:24.142   3rd Qu.:187.696   3rd Qu.:15.744  
##  Max.   :3038.61   Max.   :54.319   Max.   :342.366   Max.   :21.226  
##                                                       NA's   :2       
##      solar         solarcv           tc_1             tc_2       
##  Min.   :1370   Min.   :18.36   Min.   : 50.31   Min.   : 25.03  
##  1st Qu.:2031   1st Qu.:31.00   1st Qu.: 93.36   1st Qu.: 48.61  
##  Median :2101   Median :33.00   Median :125.56   Median : 57.87  
##  Mean   :2079   Mean   :33.82   Mean   :122.49   Mean   : 58.36  
##  3rd Qu.:2163   3rd Qu.:34.21   3rd Qu.:152.32   3rd Qu.: 66.18  
##  Max.   :2654   Max.   :58.81   Max.   :197.26   Max.   :108.98  
##                                                                  
##       tc_3             precip            temp         northness      
##  Min.   :  2.067   Min.   : 3.164   Min.   :10.39   Min.   :  2.547  
##  1st Qu.: 32.682   1st Qu.: 6.581   1st Qu.:21.12   1st Qu.: 31.923  
##  Median : 50.433   Median : 7.469   Median :24.05   Median : 91.407  
##  Mean   : 53.769   Mean   :10.367   Mean   :23.61   Mean   : 90.070  
##  3rd Qu.: 75.994   3rd Qu.:10.422   3rd Qu.:26.61   3rd Qu.:146.644  
##  Max.   :124.071   Max.   :45.046   Max.   :31.83   Max.   :172.567  
## 
```

You may recall from discussion of EDA that QQ plots are a visual way to inspect the normality of a variable. If the variable is normally distributed, the points (e.g. soil observations) should line up along the straight line.


```r
# QQ plot

library(ggplot2)

ggplot(data, aes(sample = tempC)) +
  geom_qq() +
  geom_qq_line()
```

<img src="004-linear-models_files/figure-html/qq-plot-1.png" width="672" />

By examining the correlations between some of the predictors we can also determine wheter they are *collinear* (e.g. > 0.6). This is common for similar variables such as landsat bands, terrain derivatives, and climatic variables. Variables that are colinear are redundant and contain no additional information. In additino, collinearity will make it difficult to estimate our regression coefficients. 


```r
vars <- c("tempC", "elev", "temp", "precip", "tc_2", "tc_1", "tc_3")
GGally::ggpairs(data[, vars])
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-4-1.png" width="672" />

```r
vars <- c("tempC", "slope", "twi", "northness", "solar", "solarcv")
GGally::ggpairs(data[, vars])
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values
```

```
## Warning: Removed 2 rows containing missing values (geom_point).

## Warning: Removed 2 rows containing missing values (geom_point).
```

```
## Warning: Removed 2 rows containing non-finite values (stat_density).
```

```
## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values

## Warning in ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, :
## Removed 2 rows containing missing values
```

```
## Warning: Removed 2 rows containing missing values (geom_point).

## Warning: Removed 2 rows containing missing values (geom_point).

## Warning: Removed 2 rows containing missing values (geom_point).
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-4-2.png" width="672" />

The correlation matrices and scatter plots above show that that MAST has moderate correlations with some of the variables, particularly the elevation and the climatic variables. 

Examining the density plots on the diagonal axis of the scatterplots we can also see that some variables are skewed.


### Compare Samples vs Population 

Since our data was not randomly sampled, we had better check the distribution of our samples vs the population. We can accomplish this by overlaying the sample distribution of predictor variables vs a large random sample.


```r
geodata_df <- as.data.frame(geodata_s)

geodata_df <- rbind(
  data.frame(source = "sample",     data[names(geodata_df)]),
  data.frame(source = "population", geodata_df)
)

geodata_w <- reshape::melt(
  geodata_df,
  id.vars = "source",
  measures.vars = vars
  )

ggplot(geodata_w, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Evaluation of Sample Representativeness")
```

```
## Warning: Removed 6466 rows containing non-finite values (stat_density).
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-5-1.png" width="672" />

The overlap between our sample and the population appear satistifactory.


## Linear modeling

R has several functions for fitting linear models. The most common is arguably the `lm()` function from the stats R package, which is loaded by default. The `lm()` function is also extended thru the use of several additional packages such as the car and caret R packages. Another noteworthy R package for linear modeling is rms, which offers the `ols()` function for linear modeling. The rms R package (Harrell et al., 2015) offers an 'almost' comprehesive alternative to `lm()' and it's accessory function. It is difficult to objectively functions say which approach is better. Therefore methods both methods will be demonstrated. Look for comments (i.e. #) below referring to rms, stats, caret or visreg. 


```r
# stats
fit_lm <- lm(tempC ~ elev + aspect + twi + solar + solarcv + tc_1 + tc_2 + tc_3 + precip + temp, data = data, weights = data$numDays)


# rms

library(rms)
```

```
## Warning: package 'rms' was built under R version 4.0.5
```

```
## Loading required package: Hmisc
```

```
## Warning: package 'Hmisc' was built under R version 4.0.4
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Warning: package 'survival' was built under R version 4.0.5
```

```
## Loading required package: Formula
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```
## Loading required package: SparseM
```

```
## Warning: package 'SparseM' was built under R version 4.0.4
```

```
## 
## Attaching package: 'SparseM'
```

```
## The following object is masked from 'package:base':
## 
##     backsolve
```

```r
dd <- datadist(data)
options(datadist = "dd")

fit_ols <- ols(tempC ~ elev + aspect + twi + solar + solarcv + tc_1 + tc_2 + tc_3 + precip + temp, data = data, x = TRUE, y = TRUE, weights = data$numDays)
```


### Diagnostics

#### Residual plots

Once we have a model we need to assess residuals for linearity, normality, and homoscedastivity (or constant variance). Oddly this is one area were the rms R package does not offer convient functions for plotting residuals, therefore we'll simply access the results of `lm()`.


```r
par(mfcol = c(2, 2))

plot(fit_lm)
```

<img src="004-linear-models_files/figure-html/residuals-1.png" width="672" />

```r
termplot(fit_lm, partial.resid = TRUE, col.res = "black", pch = 16)
```

<img src="004-linear-models_files/figure-html/residuals-2.png" width="672" /><img src="004-linear-models_files/figure-html/residuals-3.png" width="672" /><img src="004-linear-models_files/figure-html/residuals-4.png" width="672" />


#### Multicolinearity

As we mentioned earlier multicolinearity should be avoided. To assess a model for multicolinearity we can compute the **variance inflation factor** (VIF). Its square root indicates the amount of increase in the predictor coefficients standard error. A value greater than 3 indicates a doubling the standard error. Rules of thumb vary, but a square root of vif greater than 2 or 3 indicates an unacceptable value.


```r
# vif() function from the rms or car packages
sqrt(car::vif(fit_lm))
```

```
##     elev   aspect      twi    solar  solarcv     tc_1     tc_2     tc_3 
## 9.282520 1.179764 1.879222 6.744169 6.695338 4.081507 2.891362 3.619210 
##   precip     temp 
## 2.041644 8.270426
```

```r
# or 

sqrt(rms::vif(fit_ols)) > 3
```

```
##    elev  aspect     twi   solar solarcv    tc_1    tc_2    tc_3  precip    temp 
##    TRUE   FALSE   FALSE    TRUE    TRUE    TRUE   FALSE    TRUE   FALSE    TRUE
```

The values above indicate we have several colinear variables in the model, which you might have noticed already from the scatter plot matrix.


### Variable selection & model validation

Modeling is an iterative process that cycles between fitting and evaluating alternative models. Compared to tree and forest models, linear and generalized models typically require more scrunity from the user. Automated model selection procedures are available, but should not be taken at face value because they may result in complex and unstable models. This is in part due to correlation amongst the predictive variables that can confuse the model. Also, the order in which the variables are included or excluded from the model effects the significance of the other variables, and thus several weak predictors might mask the effect of one strong predictor. Regardless of the approach used, variable selection is probably the most controversial aspect of linear modeling.

Both the rms and caret packages offer methods for variable selection and cross-validation. In this instance the rms approach is a bit more convinent, with the one line call to `validate()`.


```r
# Set seed for reproducibility
set.seed(42)


# rms
## stepwise selection and validation
step_rms <- validate(fit_ols, method = "crossvalidation", B = 10, bw = TRUE)
```

```
## 
## 		Backwards Step-down - Original Model
## 
##  Deleted Chi-Sq d.f. P      Residual d.f. P      AIC   R2   
##  solar   0.00   1    0.9627 0.00     1    0.9627 -2.00 34736
##  aspect  0.15   1    0.6944 0.16     2    0.9247 -3.84 34736
##  tc_3    0.70   1    0.4032 0.86     3    0.8362 -5.14 34735
##  precip  0.34   1    0.5609 1.19     4    0.8792 -6.81 34735
##  twi     0.84   1    0.3586 2.04     5    0.8441 -7.96 34733
##  temp    3.25   1    0.0716 5.28     6    0.5082 -6.72 34728
## 
## Approximate Estimates after Deleting Factors
## 
##                Coef      S.E.  Wald Z         P
## Intercept 43.887389 2.1856221  20.080 0.000e+00
## elev      -0.006998 0.0003973 -17.613 0.000e+00
## solarcv   -0.190672 0.0211880  -8.999 0.000e+00
## tc_1      -0.036248 0.0078482  -4.619 3.863e-06
## tc_2      -0.093525 0.0245610  -3.808 1.402e-04
## 
## Factors in Final Model
## 
## [1] elev    solarcv tc_1    tc_2
```

The results for `validate()` above and below show which variables were retained and deleted. Below we can see a dot matrix of which variables were retained in during the 10 iterations of the cross validation. In addition, below we can see the difference between the training and test accuracy and error metrics. Remember that it is the test accuracy we should pay attention too.


```r
## test accuracy and error
step_rms
```

```
##           index.orig training   test optimism index.corrected  n
## R-square      0.9480   0.9480 0.6819   0.2661          0.6819 10
## MSE           1.5629   1.5504 2.7899  -1.2395          2.8024 10
## g             5.7490   5.7647 5.4549   0.3098          5.4392 10
## Intercept     0.0000   0.0000 3.1803  -3.1803          3.1803 10
## Slope         1.0000   1.0000 0.8586   0.1414          0.8586 10
## 
## Factors Retained in Backwards Elimination
## 
##  elev aspect twi solar solarcv tc_1 tc_2 tc_3 precip temp
##                        *       *    *                *   
##  *                     *       *    *                    
##  *                     *       *    *                    
##  *                     *       *    *                    
##  *                     *       *    *                    
##              *         *            *                *   
##                        *                             *   
##                        *       *    *                *   
##  *                     *       *    *                    
##                        *       *    *                *   
## 
## Frequencies of Numbers of Factors Retained
## 
## 2 4 
## 1 9
```

The caret package option for variable selection and validation is a bit more verbose than the  rms package. However, the caret package is a more versatile package, with options for over 50 different models, such as other tree-based models.


```r
# caret
library(caret)
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:survival':
## 
##     cluster
```

```r
## cross validation parameters
train.control <- trainControl(method = "cv", number = 10, savePredictions = TRUE, returnData = TRUE)

## stepwise selection and validation
step_caret <- train(tempC ~ elev + solar + aspect + twi + solar + solarcv + tc_1 + tc_2 + tc_3 + log(precip) + temp^2, 
                    data = data,
                    weights = data$numDays,
                    method = "lmStepAIC",
                    trace = FALSE,
                    trControl = train.control,
                    na.action = na.exclude
                    )

## test accuracy and error
summary(step_caret$resample)
```

```
##       RMSE          Rsquared           MAE           Resample        
##  Min.   :0.777   Min.   :0.7788   Min.   :0.6110   Length:10         
##  1st Qu.:1.112   1st Qu.:0.8856   1st Qu.:0.8779   Class :character  
##  Median :1.542   Median :0.9423   Median :1.2662   Mode  :character  
##  Mean   :1.479   Mean   :0.9171   Mean   :1.1279                     
##  3rd Qu.:1.762   3rd Qu.:0.9756   3rd Qu.:1.3318                     
##  Max.   :2.373   Max.   :0.9843   Max.   :1.5093
```

The output from caret is somewhat different. Notice it selected a slightly different combination of variables and more optimistic test accuracy and error.


```r
# summary
summary(step_caret$finalModel)
```

```
## 
## Call:
## lm(formula = .outcome ~ elev + twi + solarcv + tc_1 + tc_2 + 
##     `log(precip)` + temp, data = dat, weights = wts)
## 
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max 
## -132.087  -30.532   -1.176   35.609  103.131 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   30.298762   8.367072   3.621 0.000618 ***
## elev          -0.005103   0.001900  -2.686 0.009420 ** 
## twi           -0.098647   0.067447  -1.463 0.148976    
## solarcv       -0.184135   0.020963  -8.784 3.02e-12 ***
## tc_1          -0.026130   0.009523  -2.744 0.008068 ** 
## tc_2          -0.073104   0.025710  -2.843 0.006154 ** 
## `log(precip)`  0.604990   0.425929   1.420 0.160844    
## temp           0.378019   0.253919   1.489 0.141974    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 55.22 on 58 degrees of freedom
## Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9566 
## F-statistic: 205.7 on 7 and 58 DF,  p-value: < 2.2e-16
```


### Final model & accuracy assessment



```r
# rms
final_ols <- ols(tempC ~ elev + solarcv + tc_1 + tc_2, data = data, weights = data$numDays, x = TRUE, y = TRUE)

validate(final_ols, method = "crossvalidation", B = 10)
```

```
##           index.orig training   test optimism index.corrected  n
## R-square      0.9397   0.9410 0.8379   0.1031          0.8367 10
## MSE           1.7778   1.7181 2.3015  -0.5834          2.3612 10
## g             5.7652   5.6907 5.8429  -0.1521          5.9174 10
## Intercept     0.0000   0.0000 0.7950  -0.7950          0.7950 10
## Slope         1.0000   1.0000 0.9576   0.0424          0.9576 10
```

```r
# caret
final_caret <- train(tempC ~ elev + solarcv + tc_1 + tc_2, 
                     data = data,
                     weights = data$numDays,
                     method = "lm",
                     trControl = train.control,
                     na.action = na.exclude
                     )
final_caret$results
```

```
##   intercept     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
## 1      TRUE 1.441123 0.9340218 1.120288 0.3257457 0.03683471 0.2596735
```

```r
final_lm <- final_caret$finalModel
```



### Model Effects


```r
summary(final_lm)
```

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat, weights = wts)
## 
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max 
## -138.572  -28.633    4.663   35.574  117.141 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 43.8704062  2.1744491  20.175  < 2e-16 ***
## elev        -0.0070036  0.0003957 -17.700  < 2e-16 ***
## solarcv     -0.1919234  0.0211084  -9.092 4.48e-13 ***
## tc_1        -0.0366152  0.0077992  -4.695 1.49e-05 ***
## tc_2        -0.0918754  0.0244223  -3.762 0.000372 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 56.91 on 63 degrees of freedom
## Multiple R-squared:  0.9554,	Adjusted R-squared:  0.9526 
## F-statistic: 337.8 on 4 and 63 DF,  p-value: < 2.2e-16
```

```r
dd <- datadist(data)
options(datadist = "dd")

anova(final_ols)
```

```
##                 Analysis of Variance          Response: tempC 
## 
##  Factor     d.f. Partial SS MS          F      P     
##  elev        1   1014554.05 1014554.054 313.30 <.0001
##  solarcv     1    267708.50  267708.498  82.67 <.0001
##  tc_1        1     71373.80   71373.796  22.04 <.0001
##  tc_2        1     45829.32   45829.318  14.15 4e-04 
##  REGRESSION  4   4375387.71 1093846.928 337.78 <.0001
##  ERROR      63    204013.46    3238.309
```

```r
anova(final_lm)
```

```
## Analysis of Variance Table
## 
## Response: .outcome
##           Df  Sum Sq Mean Sq   F value    Pr(>F)    
## elev       1 4059084 4059084 1253.4581 < 2.2e-16 ***
## solarcv    1  244880  244880   75.6199  2.18e-12 ***
## tc_1       1   25593   25593    7.9033 0.0065688 ** 
## tc_2       1   45829   45829   14.1522 0.0003721 ***
## Residuals 63  204013    3238                        
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
# rms

plot(Predict(final_ols))
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-8-1.png" width="672" />

```r
plot(Predict(final_ols, elev = NA, solarcv = c(23, 33, 51)))
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-8-2.png" width="672" />

```r
# visreg

library(visreg)

par(mfrow = c(2, 2))
visreg(final_lm)
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-8-3.png" width="672" />

```r
par(mfrow = c(1, 1))

visreg(final_lm, xvar = "elev", by = "solarcv", breaks = c(23, 33, 51), overlay = TRUE)
```

<img src="004-linear-models_files/figure-html/unnamed-chunk-8-4.png" width="672" />



## Generate spatial predictions


```r
# Predict tempC model
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE)
  }

mast_r <- predict(geodata_r, final_lm, fun = predfun, index = 1:2, progress = 'text')

writeRaster(mast_r[[1]], filename = "C:/workspace2/mast.tif", format = "GTiff", progress = "text")

writeRaster(mast_r[[2]], filename = "C:/workspace2/mast_se.tif", format = "GTiff", progress = "text")
```


## Create Map


```r
library(raster)

mlra <- st_transform(mlra, 5070)

# mast
mast <- raster("C:/workspace2/mast.tif")
crs(mast) <- "+init=epsg:5070"

plot(mast)
plot(mlra[1], col = NA, add = TRUE)


# mast standard error
mast_se <- raster("C:/workspace2/mast_se.tif")
crs(mast_se) <- "+init=epsg:5070"

plot(mast_se)
plot(mlra[1], col = NA, add = TRUE)
```


## Literature
Bai, Y., T.A. Scott, W. Chen, R.C. Graham, L. Wu, A.C. Chang, and L.J. Lund, 2010. Soil Temperature Regimes in the Mojave Desert. Soil Science, 175(8):398-404. 

Harrell, F.E., 2015. Regression Modeling Strategies: With Applications to Linear Models, Logisitc and Ordinal Regression, and Survival Analysis. Springer, New York. [https://link.springer.com/book/10.1007%2F978-3-319-19425-7](https://link.springer.com/book/10.1007%2F978-3-319-19425-7)

Mount, H.R., and R.F. Paetzold, 2002. The temperature regime for selected soils in the United States. United States Department of Agriculture, Natural Resources Con-servation Service, National Soil Survey Center, Lincoln, Nebraska, Soil Survey Investi-gation Report No. 48.

Peterson, F.F., 1992. Status of Soil Climate Studies in Nevada. pp1-11.  

[Roecker, S.M. and C.A. Haydu-Houdeshell, 2012. Modeling and Application of Soil Temperature in the Mojave and Lower Colorado Deserts of California. 2012 Western Regional Cooperative Soil Survey Conference.](https://www.researchgate.net/publication/331977150_Modeling_and_Application_of_Soil_Temperature_in_the_Mojave_and_Lower_Colorado_Deserts_of_California/citations)

Schmidlin, T.W., F.F. Peterson, and R.O. Gifford, 1983. Soil Temperature Regimes of Nevada. Soil So. Sci. Am. J., 47:977-982. 

## Additional reading

Faraway, J.J., 2002. Practical Regression and Anova using R. CRC Press, New York. [https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf](https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf)

James, G., D. Witten, T. Hastie, and R. Tibshirani, 2014. An Introduction to Statistical Learning: with Applications in R. Springer, New York. [http://www-bcf.usc.edu/~gareth/ISL/](http://www-bcf.usc.edu/~gareth/ISL/)

Hengl, T. 2009. A Practical Guide to Geostatistical Mapping, 2nd Edt. University of Amsterdam, www.lulu.com, 291 p. ISBN 978-90-9024981-0. [http://spatial-analyst.net/book/system/files/Hengl_2009_GEOSTATe2c0w.pdf](http://spatial-analyst.net/book/system/files/Hengl_2009_GEOSTATe2c0w.pdf)

Webster, R. 1997. Regression and functional relations. European Journal of Soil Science, 48, 557-566. [http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2389.1997.tb00222.x/abstract](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2389.1997.tb00222.x/abstract)


## References (Linear Models)


