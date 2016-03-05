# Chapter 1 - Introduction to R and RStudio
Katey Yoast, Skye Wills, Stephen Roecker  
Tuesday, February 24, 2015  


![Statistics for pedologists course banner image](figure/logo.jpg)

# Introduction

Linear regression has been used for soil survey applications since the early 1900s when Briggs and McLane (1907) developed a pedotransfer function to estimate the wilting coefficient as a function of soil particle size. 

Wilting coefficient = 0.01(sand) + 0.12(silt) + 0.57(clay)

Linear regression, as the name implys, models the linear relationship between a response variable (y) and an predictor variable (x). 

$f(y) = \beta_{0} + \beta_{1}x + \varepsilon$

- $\beta_{0}$ = intercept of the fitted line
- $\beta_{1}x$ = slope of the fitted line
- $\varepsilon$ = the error term

When more than one independent variable is used in the regression, it is referred to as multiple linear regression. In regression models, the response (or dependent) variable must always be continuous. The predictor (or independent) variable(s) can be continuous or categorical. In order to use linear regression or any linear model, the errors (i.e. residuals) must be normally distributed. Most environmental data are skewed and require transformations to the response variable (such as square root or log) for use in linear models. Normality can be assessed using a QQ plot or histogram of the residuals.


# Linear Regression Example

Now that we've got some of the basic theory out of the way we'll move on to a real example, and address any additional theory where it relates to specific steps in the modeling process. The example selected for this chapter comes from Joshua Tree National Park (JTNP)(i.e. CA794) in the Mojave desert. The landscape is composed primarily of closed basins ringed by granitic hills and mountains (Peterson, 1981). The problem tackled here is modeling the distribution of surface rock fragments as a function of digital elevation model (DEM) and Landsat derivatives.

With this dataset we'll encounter some challenges. To start with, fan piedmont landscapes typically have relatively little relief. Since most of our predictors will be derivatives of elevation, that won't leave us with much to work with. Also, our elevation data comes from the USGS National Elevation dataset (NED), which provides considerably less detail than say LiDAR or IFSAR (Shi et al., 2012). Lastly our pedon dataset like most in NASIS, hasn't received near as much quality control as have the components. So we'll need to wrangle some of the pedon data before we can analyze it. These are all typical problems encountered in any data analysis and should be good practice.


## Load packages

To start, as always we need to load some extra packages. This is a necessary evil every time you start R. Most of the basic functions we need to develop a linear regression model are contained in base R, but the following contain some useful spatial and data manipulation functions. Believe it or not we will use all of them and more.


```r
library(aqp) # specialized soil classes and functions
library(soilDB) # NASIS and SDA import functions
library(raster) # guess
library(rgdal) # spatial import
library(lattice) # graphing
library(reshape2) # data manipulation
library(plyr) # data manipulation
library(caret) # printing
library(car) # additional regression tools
library(DAAG) # additional regression tools
```


## Read in data

Hopefully like all good soil scientists and ecological site specialists you enter your field data into NASIS. Better yet hopefully someone else did it for you. Once data are captured in NASIS it much easier to import them into R, extract the pieces you need, manipulate them, model them, etc. If it's not entered into NASIS it may as well not exist.


```r
# pedons <- fetchNASIS(rmHzErrors = FALSE) # beware the error messages, by default they don't get imported unless you override the default, which in our case shouldn't cause any problems
load(file = "C:/workspace/ch7_data.Rdata")

str(pedons, max.level = 2) # Examine the makeup of the data we imported from NASIS.
```

```
## Formal class 'SoilProfileCollection' [package "aqp"] with 7 slots
##   ..@ idcol     : chr "peiid"
##   ..@ depthcols : chr [1:2] "hzdept" "hzdepb"
##   ..@ metadata  :'data.frame':	1 obs. of  1 variable:
##   ..@ horizons  :'data.frame':	4990 obs. of  43 variables:
##   ..@ site      :'data.frame':	1168 obs. of  79 variables:
##   ..@ sp        :Formal class 'SpatialPoints' [package "sp"] with 3 slots
##   ..@ diagnostic:'data.frame':	2133 obs. of  4 variables:
```

# Exploratory analysis

## Data Wrangling

Generally before we begin modeling its good to explore the data. By examining a simple summary we can quickly see the breakdown of our data. Unfortunately, odds are all the data haven't been properly populated like they should be.


```r
s <- site(pedons) # extract the site data frame from the pedons soil profile collection object

s$surface_gravel <- with(s, surface_gravel - surface_fgravel) # recalculate gravel to exclude fine gravel
s$frags <- apply(s[grepl("surface", names(s))], 1, sum) # calculate total surface rock fragments

densityplot(~ surface_cobbles + surface_gravel + surface_fgravel + frags, data = s, auto.key = TRUE)
```

![](6_Linear_models_files/figure-html/consistency-1.png)

```r
hist(s$frags, 50)
```

![](6_Linear_models_files/figure-html/consistency-2.png)

```r
apply(s[grepl("surface|frags", names(s))], 2, function(x) round(summary(x))) # summarize all columns that pattern match either "surface" or "frags"
```

```
##         surface_fgravel surface_gravel surface_cobbles surface_stones
## Min.                  0              0               0              0
## 1st Qu.               0             10               0              0
## Median               12             25               5              0
## Mean                 17             31              10              4
## 3rd Qu.              25             48              15              5
## Max.                 95             95              65             55
##         surface_boulders surface_channers surface_flagstones
## Min.                   0                0                  0
## 1st Qu.                0                0                  0
## Median                 0                0                  0
## Mean                   1                0                  0
## 3rd Qu.                1                0                  0
## Max.                  25                5                  0
##         surface_paragravel surface_paracobbles frags
## Min.                     0                   0     0
## 1st Qu.                  0                   0    42
## Median                   0                   0    70
## Mean                     0                   0    63
## 3rd Qu.                  0                   0    86
## Max.                    20                   2   180
```

```r
sum(s$frags > 100) # number of samples greater than 100
```

```
## [1] 12
```

```r
sum(s$frags < 1) # number of samples less than  1
```

```
## [1] 35
```

Examining the results we can see that the distribution of our surface rock fragments are skewed. In addition, we apparently have values in excess of 100 and some that equal 0. Those values in excess of 100 are likely recording errors, while the values that equal 0 could either be truly 0 or NA.


## Geomorphic data

Another obvious place to look is at the geomorphic data in the site table. This information is intended to help differentiate where our soil observations exist on the landscape. If populated consistently it could be used in future disaggregation efforts, as demonstrated by Nauman and Thompson (2014).

### Landform vs frags


```r
quantile2 <- function(x) c(round(quantile(x, probs = c(0.05, 0.5, 0.95))), n = length(x)) # Create a custom quantile function

test <- aggregate(frags ~ landform.string, data = s, quantile2)

test <- subset(test, frags[, 4] > 3) # subset the data frame to only include landforms with greater than 3 observations

arrange(test, frags[, 2], decreasing = TRUE) # sort the data frame by the frags matrix column using plyr package function
```

```
##               landform.string frags.5% frags.50% frags.95% frags.n
## 1                    mountain       70        90        95      59
## 2                     ballena       69        85        99      10
## 3                   hillslope       28        85       100     151
## 4  fan piedmont & fan remnant       80        80        95       5
## 5                        hill        0        80        95      63
## 6                 fan remnant       13        75        98     232
## 7                   inset fan       14        75        90      19
## 8              mountain slope       25        75       100      82
## 9                    pediment       31        75        95      43
## 10    drainageway & fan apron       55        70        85       4
## 11              rock pediment       13        65        86       8
## 12                       spur       20        62        90      31
## 13                drainageway       20        60        90      39
## 14                       wash       16        60        86      15
## 15                  high hill       29        58        85       8
## 16    drainageway & inset fan       43        55        67       4
## 17                  fan apron        5        55        91     152
## 18    fan apron & fan remnant       13        50        75      29
## 19             stream terrace       42        50        64       5
## 20                   low hill       29        46        92      13
## 21               alluvial fan       14        45        91      57
## 22                        fan       20        40        78      11
## 23                    terrace        0        40        85      11
## 24       fan apron & pediment       16        32       142       7
## 25  drainageway & fan remnant        4        25        67       4
## 26                 sand sheet        0         5        40      11
```

```r
# or sort using the order() function from the base package

# test[order(test$surface_total[, 2], decreasing = TRUE), ]
```

There are obviously a wide variety of landforms. However generally it appears that erosional landforms have the most surface rock fragments. Lets generalize the `landform.string` and have a closer look.


```r
s$landform <- ifelse(grepl("fan|terrace|sheet|drainageway|wash", s$landform.string), "fan", "hill") # generalize the landform.string
s$landform <- as.factor(s$landform)

test <- aggregate(frags ~ landform, data = s, quantile2)

arrange(test, landform, frags[, 2], decreasing = TRUE) # sort data frame by column using plyr 
```

```
##   landform frags.5% frags.50% frags.95% frags.n
## 1     hill       20        80        99     513
## 2      fan        5        60        95     655
```

```r
densityplot(~ frags + surface_cobbles + surface_gravel | landform, data = s, auto.key = TRUE)
```

![](6_Linear_models_files/figure-html/unnamed-chunk-1-1.png)

So it does appear that erosional landforms generally do have more surface rock fragments that depositional one, but not by much. It also appears that most of the difference is coming from the amount of cobbles, as seen in the density plot.


### Hillslope position


```r
test <- aggregate(frags ~ landform + hillslope_pos, data = s, quantile2)

arrange(test, landform, frags[, 2], decreasing = TRUE)
```

```
##    landform hillslope_pos frags.5% frags.50% frags.95% frags.n
## 1      hill     Footslope       22        87        96      16
## 2      hill     Backslope       21        78       100     325
## 3      hill      Shoulder       28        72        92      36
## 4      hill        Summit       23        70       100      21
## 5      hill      Toeslope       33        40        72       3
## 6       fan        Summit       13        80        98     130
## 7       fan      Shoulder       21        75       103      24
## 8       fan      Toeslope       13        60        90      93
## 9       fan     Backslope        4        55        91     101
## 10      fan     Footslope        2        50        91      13
```

If we examine the different hillslope positions for each generic landform we can see other trends. For hills it appears that surface rock fragments decrease as we traverse up the slope, with the exception of the toeslopes which are typically associated with drainageways. On fans we see the opposite relationship, with toeslopes again being the exception. 


### Slope shape


```r
test <- aggregate(frags ~ landform + paste(shapedown, shapeacross), data = s, quantile2)

arrange(test, landform, frags[, 2], decreasing = TRUE)
```

```
##    landform paste(shapedown, shapeacross) frags.5% frags.50% frags.95%
## 1      hill               Concave Concave       63        89        94
## 2      hill                Concave Convex       54        89        98
## 3      hill                Linear Concave       36        80       100
## 4      hill                 Linear Convex       21        80       100
## 5      hill                 Linear Linear        4        80       100
## 6      hill                         NA NA        7        80        95
## 7      hill                Concave Linear       18        76        94
## 8      hill                 Convex Convex       26        75        95
## 9      hill                 Convex Linear       32        70        96
## 10     hill                Convex Concave       37        45        85
## 11      fan               Concave Concave       51        86        89
## 12      fan                 Linear Convex       16        72        95
## 13      fan                Convex Concave       70        70        70
## 14      fan                     Linear NA       70        70        70
## 15      fan                Concave Linear       39        67        97
## 16      fan                 Convex Linear       20        65        94
## 17      fan                Linear Concave       29        64       100
## 18      fan                 Convex Convex        3        60        90
## 19      fan                 Linear Linear       10        55        95
## 20      fan                         NA NA        0        38        95
## 21      fan                Concave Convex       21        26        31
##    frags.n
## 1        3
## 2       17
## 3       34
## 4      130
## 5      175
## 6       48
## 7        6
## 8       66
## 9       31
## 10       3
## 11       4
## 12     146
## 13       1
## 14       1
## 15      20
## 16      45
## 17      36
## 18      53
## 19     275
## 20      72
## 21       2
```

When examining slope shape on hills it appears that concave positions have greater amounts of surface rock fragments. I can't see any sensible pattern with slope shape on fans.


### Surface Morphometry, Depth and Surface Rock Fragments


```r
# Subset Generic landforms and Select Numeric Columns
s_fan <- subset(s, landform == "fan", select = c(frags, surface_gravel, bedrckdepth, slope_field, elev_field))
s_hill <- subset(s, landform == "hill", select = c(frags, surface_gravel, bedrckdepth, slope_field, elev_field))

# Correlation Matrices
round(cor(s_fan, use = "pairwise"), 2)
```

```
##                frags surface_gravel bedrckdepth slope_field elev_field
## frags           1.00           0.65        0.17        0.14      -0.28
## surface_gravel  0.65           1.00        0.17        0.06      -0.02
## bedrckdepth     0.17           0.17        1.00       -0.31       0.14
## slope_field     0.14           0.06       -0.31        1.00      -0.02
## elev_field     -0.28          -0.02        0.14       -0.02       1.00
```

```r
round(cor(s_hill, use = "pairwise"), 2)
```

```
##                frags surface_gravel bedrckdepth slope_field elev_field
## frags           1.00           0.58        0.03        0.12      -0.22
## surface_gravel  0.58           1.00        0.12       -0.05      -0.03
## bedrckdepth     0.03           0.12        1.00        0.29      -0.21
## slope_field     0.12          -0.05        0.29        1.00      -0.24
## elev_field     -0.22          -0.03       -0.21       -0.24       1.00
```

```r
# Scatterplot Matrices
spm(s_fan, use = "pairwise", main = "Scatterplot Matrix for Fans")
```

![](6_Linear_models_files/figure-html/unnamed-chunk-4-1.png)

```r
spm(s_hill, use = "pairwise", main = "Scatterplot Matrix for Hills")
```

![](6_Linear_models_files/figure-html/unnamed-chunk-4-2.png)

In examing the correlation matrices we don't see a strong relationships with either elevation for slope gradient.


### Soil Scientist Bias

Next we'll look at soil scientist bias. The question being do some soil scientists have a tendency to describe more surface rock fragments that others. Due to the excess number of soil scientist that have worked on CA794, including detailees, we've filtered the names of soil scientist to include just the top 3 soil scientists and given priority to the most senior soil scientists when they occur together.


```r
# Custom function to filter out the top 3 soil scientists
desc_test <- function(old) {
  old <- as.character(old)
  new <- NA
  # ranked by seniority
  if (is.na(old)) {new <- "other"}
  if (grepl("Stephen", old)) {new <- "Stephen"} # least senior
  if (grepl("Paul", old)) {new <- "Paul"} 
  if (grepl("Peter", old)) {new <- "Peter"} # most senior
  if (is.na(new)) {new <- "other"}
 return(new)
}

s$describer2 <- sapply(s$describer, desc_test)

test <- aggregate(frags ~ landform + describer2, data = s, function(x) round(quantile(x, probs = c(0, 0.5, 1))))

arrange(test, landform, frags[, 2], decreasing = TRUE)
```

```
##   landform describer2 frags.0% frags.50% frags.100%
## 1     hill      Peter        0        88        100
## 2     hill       Paul        0        85        101
## 3     hill      other        0        75        110
## 4     hill    Stephen        5        50        100
## 5      fan       Paul        0        80        180
## 6      fan      Peter        0        65        125
## 7      fan      other        0        60        119
## 8      fan    Stephen        0        35         96
```

In looking at the numbers it appears we have a bit of a trend on both fans and hills. We can see that Stephen always describes the least amount of most surface rock fragments, while Paul and Peter trade places describing the most on hills and fans. By looking the maximum values we can also see who is recording surface rock fragments in excess of 100. However while these trends are suggestive, they are not definitive because they doesn't take into account other factors. We'll examine this potential bias more closely later.


## Plot coordinates

Where do our points plot? We can plot the general location in R, but for a closer look, we'll export them to a Shapefile so that they can viewed in a proper GIS. Notice in the figure below the number of points that fall outside the survey boundary. What it doesn't show is the points in the Ocean or Mexico.


```r
# Convert soil profile collection to a spatial object
pedons2 <- pedons
slot(pedons2, "site") <- s # this is dangerous, but something needs to be fixed in the site() setter function
idx <- complete.cases(site(pedons2)[c("x", "y")]) # create an index to filter out pedons with missing coordinates
pedons2 <- pedons2[idx]
coordinates(pedons2) <- ~ x + y # set the coordinates
proj4string(pedons2) <- CRS("+init=epsg:4326") # set the projection
pedons_sp <- as(pedons2, "SpatialPointsDataFrame") # coerce to spatial object
```

```
## only site data are extracted
```

```r
pedons_sp <- spTransform(pedons_sp, CRS("+init=epsg:5070")) # reproject

# Read in soil survey area boundaries
ssa <- readOGR(dsn = "F:/geodata/soils/soilsa_a_nrcs.shp", layer = "soilsa_a_nrcs")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "F:/geodata/soils/soilsa_a_nrcs.shp", layer: "soilsa_a_nrcs"
## with 3262 features
## It has 8 fields
```

```r
ca794 <- subset(ssa, areasymbol == "CA794") # subset out Joshua Tree National Park
ca794 <- spTransform(ca794, CRS("+init=epsg:5070"))

# Plot
plot(ca794, axes = TRUE)
plot(pedons_sp, add = TRUE) # notice the points outside the boundary
```

![](6_Linear_models_files/figure-html/plot-1.png)

```r
# Write shapefile of pedons
# writeOGR(pedons_sp, dsn = "F:/geodata/project_data/8VIC", "pedon_locations", driver = "ESRI Shapefile") 
```


### Exercise 1: View the data in ArcGIS

- Examine the shapefile in ArcGIS along with our potential predictive variables (hint classify the Shapefile symbology using the frags column)
- Discuss with your group, and report your observations or hypotheses


## Extracting spatial data

Prior to any spatial analysis or modeling, you need to develop a suite of geodata files that can be intersected with your field data locations. This is in and of itself is a difficult task, and should be facilitated by your Regional GIS Specialist. Typically this would primarily consist of derivatives from a DEM or satellite imagery. Prior to any prediction it is also necessary to ensure the geodata files have the same projection, extent, and cell size. Once we have the necessary files we can construct a list in R of the file names and paths, read the geodata into R and extract the geodata values where they intersect with field data.

As you can see below their is an almost limitless number of variables we could inspect.


```r
folder <- "F:/geodata/project_data/8VIC/ca794/"
files <- c(
  elev   = "ned30m_8VIC.tif", # elevation
  slope  = "ned30m_8VIC_slope5.tif", # slope gradient
  aspect = "ned30m_8VIC_aspect5.tif", # slope aspect
  twi    = "ned30m_8VIC_wetness.tif", # topographic wetness index
  twi_sc = "ned30m_8VIC_wetness_sc.tif", # transformed twi
  ch     = "ned30m_8VIC_cheight.tif", # catchment height
  z2str  = "ned30m_8VIC_z2stream.tif", # height above streams
  mrrtf  = "ned30m_8VIC_mrrtf.tif", # multiresolution ridgetop flatness index
  mrvbf  = "ned30m_8VIC_mrvbf.tif", # multiresolution valley bottom flatness index
  solar  = "ned30m_8VIC_solar.tif", # solar radiation
  precip = "prism30m_8VIC_ppt_1981_2010_annual_mm.tif", # annual precipitation
  precipsum = "prism30m_8VIC_ppt_1981_2010_summer_mm.tif", # summer precipitation
  temp   = "prism30m_8VIC_tmean_1981_2010_annual_C.tif", # annual temperature
  ls     = "landsat30m_8VIC_b123457.tif", # landsat bands
  pc     = "landsat30m_8VIC_pc123456.tif", # principal components of landsat
  tc     = "landsat30m_8VIC_tc123.tif", # tasseled cap components of landsat
  k      = "gamma30m_8VIC_namrad_k.tif", # gamma radiometrics signatures
  th     = "gamma30m_8VIC_namrad_th.tif",
  u      = "gamma30m_8VIC_namrad_u.tif",
  cluster = "cluster152.tif" # unsupervised classification
  )

geodata_f <- paste0(folder, files) # combine the folder directory and file names

# Create a raster stack
geodata_r <- stack(geodata_f)
names(geodata_r) <- names(files)

# Extract the geodata and imbed in a data frame
data <- data.frame(
   as.data.frame(pedons_sp)[c("pedon_id", "taxonname", "frags", "x_std", "y_std", "describer2", "landform.string", "landform", "argillic.horizon")],
   extract(geodata_r, pedons_sp)
   )

# Modify some of the geodata variables
data$cluster <- factor(data$cluster, levels = 1:15)
data$cluster2 <- reorder(data$cluster, data$temp)
data$gsi <- with(data, (ls_3 - ls_1) / (ls_3 + ls_2 + ls_1))
data$ndvi <- with(data, (ls_4 - ls_3) / (ls_4 + ls_3))
data$sw <- cos(data$aspect - 255)

# save(data, ca794, pedons, file = "C:/workspace/ch7_data.Rdata")

# Strip out location and personal information before uploading to the internet
# s[c("describer", "describer2", "x", "y", "x_std", "y_std", "utmnorthing", "utmeasting", "classifier")] <- NA
# slot(pedons, "site") <- s
# data[c("describer2", "x_std", "y_std")] <- NA
# save(data, ca794, pedons, file = "C:/workspace/stats_for_soil_survey/trunk/data/ch7_data.Rdata")
```


## Examine Spatial Data 

With our spatial data in hand, we can now see whether any of the variables have a linear relationship with surface rock fragments. 

At the beginning of our analysis we noticed some issues with our data. Particularly that the distribution of our data was skewed, and included values greater than 100 and equal to 0. So before we start lets filter those out and transform the surface rock fragments using a logit transform.


```r
load(file = "C:/workspace/stats_for_soil_survey/trunk/data/ch7_data.Rdata")
train <- data
train <- subset(train, frags > 0 & frags < 100, select = - c(pedon_id, taxonname, landform.string, x_std, y_std, argillic.horizon, describer2)) # exclude frags greater than 100 and less than 1, and exclude some of the extra columns

# Create custom transform functions
logit <- function(x) log(x / (1 - x)) # logit transform
ilogit <- function(x) exp(x) / (1 + exp(x)) # inverse logit transform

# Transform
train$fragst <- logit(train$frags / 100)

# Create list of predictor names
terrain1 <- c("slope", "solar", "mrrtf", "mrvbf")
terrain2 <- c("twi", "z2str", "ch")
climate <- c("elev", "precip", "precipsum", "temp")
ls <- paste0("ls_", 1:6)
pc <- paste0("pc_", 1:6)
tc <- paste0("tc_", 1:3)
rad <- c("k", "th", "u")

# Compute correlation matrices
round(cor(train[c("fragst", terrain1)], use = "pairwise"), 2)
```

```
##        fragst slope solar mrrtf mrvbf
## fragst   1.00  0.27 -0.09 -0.15 -0.32
## slope    0.27  1.00 -0.30 -0.49 -0.62
## solar   -0.09 -0.30  1.00  0.15  0.15
## mrrtf   -0.15 -0.49  0.15  1.00  0.25
## mrvbf   -0.32 -0.62  0.15  0.25  1.00
```

```r
round(cor(train[c("fragst", terrain2)], use = "pairwise"), 2)
```

```
##        fragst   twi z2str    ch
## fragst   1.00 -0.30  0.16 -0.07
## twi     -0.30  1.00 -0.57  0.70
## z2str    0.16 -0.57  1.00 -0.34
## ch      -0.07  0.70 -0.34  1.00
```

```r
round(cor(train[c("fragst", climate)], use = "pairwise"), 2)
```

```
##           fragst  elev precip precipsum  temp
## fragst      1.00 -0.32  -0.13     -0.26  0.31
## elev       -0.32  1.00   0.58      0.77 -0.99
## precip     -0.13  0.58   1.00      0.77 -0.55
## precipsum  -0.26  0.77   0.77      1.00 -0.74
## temp        0.31 -0.99  -0.55     -0.74  1.00
```

```r
round(cor(train[c("fragst", ls)], use = "pairwise"), 2)
```

```
##        fragst  ls_1  ls_2  ls_3  ls_4  ls_5  ls_6
## fragst   1.00 -0.06 -0.17 -0.23 -0.35 -0.42 -0.40
## ls_1    -0.06  1.00  0.98  0.94  0.84  0.73  0.77
## ls_2    -0.17  0.98  1.00  0.99  0.93  0.84  0.87
## ls_3    -0.23  0.94  0.99  1.00  0.97  0.90  0.92
## ls_4    -0.35  0.84  0.93  0.97  1.00  0.96  0.95
## ls_5    -0.42  0.73  0.84  0.90  0.96  1.00  0.98
## ls_6    -0.40  0.77  0.87  0.92  0.95  0.98  1.00
```

```r
round(cor(train[c("fragst", pc)], use = "pairwise"), 2)
```

```
##        fragst  pc_1  pc_2  pc_3  pc_4  pc_5  pc_6
## fragst   1.00  0.35 -0.47  0.00  0.09  0.00 -0.11
## pc_1     0.35  1.00 -0.08 -0.24  0.23 -0.25  0.00
## pc_2    -0.47 -0.08  1.00 -0.38 -0.17  0.03  0.06
## pc_3     0.00 -0.24 -0.38  1.00 -0.40 -0.09  0.12
## pc_4     0.09  0.23 -0.17 -0.40  1.00 -0.34 -0.22
## pc_5     0.00 -0.25  0.03 -0.09 -0.34  1.00 -0.02
## pc_6    -0.11  0.00  0.06  0.12 -0.22 -0.02  1.00
```

```r
round(cor(train[c("fragst", tc)], use = "pairwise"), 2)
```

```
##        fragst  tc_1  tc_2  tc_3
## fragst   1.00 -0.31  0.05  0.47
## tc_1    -0.31  1.00 -0.86 -0.90
## tc_2     0.05 -0.86  1.00  0.65
## tc_3     0.47 -0.90  0.65  1.00
```

```r
round(cor(train[c("fragst", rad)], use = "pairwise"), 2)
```

```
##        fragst    k   th    u
## fragst   1.00 0.04 0.02 0.12
## k        0.04 1.00 0.59 0.69
## th       0.02 0.59 1.00 0.77
## u        0.12 0.69 0.77 1.00
```

```r
# Create scatterplots
spm(train[c("fragst", terrain1)])
```

![](6_Linear_models_files/figure-html/spatial-1.png)

```r
spm(train[c("fragst", terrain2)])
```

![](6_Linear_models_files/figure-html/spatial-2.png)

```r
spm(train[c("fragst", climate)])
```

![](6_Linear_models_files/figure-html/spatial-3.png)

```r
spm(train[c("fragst", pc)])
```

![](6_Linear_models_files/figure-html/spatial-4.png)

```r
spm(train[c("fragst", tc)])
```

![](6_Linear_models_files/figure-html/spatial-5.png)

```r
spm(train[c("fragst", rad)])
```

![](6_Linear_models_files/figure-html/spatial-6.png)

```r
# Create boxplots
bwplot(fragst ~ cluster, data = train)
```

![](6_Linear_models_files/figure-html/spatial-7.png)

```r
bwplot(fragst ~ cluster2, data = train)
```

![](6_Linear_models_files/figure-html/spatial-8.png)

The correlation matrices and scatter plots above show that that surface rock fragments have moderate correlations with some of the variables, particularly the landsat bands and derivatives. This makes sense given that surface rock fragments are at the surface, unlike most soil properties. 

By examining the correlations between some of the predictors we can also see that some are *collinear* (e.g. > 0.6), such as the landsat bands. Therefore these variables are redundant as they describe almost the same thing. This collinearity will also make it difficult to estimate our regression coefficients. Considering that we already have other derivatives of landsat it our dataset, which are intentionally designed to reduce their collinearity, we may as well exclude the landsat bands from our dataset all together.

Examining the density plots on the diagonal axis of the scatterplots we can see that some variables are skewed while others are bimodal. Lastly the boxplot show a trend amongst the clusters when their sorted according to annual temperature.


# Modeling

## Model training

Modeling is an iterative process that cycles between fitting and evaluating alternative models. Compared to tree and forest models, linear and generalized models require more input from the user. Automated model selection procedures are available, but are discouraged because they generally result in complex and unstable models. This is in part due to correlation amongst the predictive variables that confuses the model. Also the order is which the variables are included or excluded from the model effects the significance of the others, and thus several weak predictors might mask the effect of one strong predictor. Therefore it is best to begin with a selection of predictors that are known to be useful, and grow the model incrementally. 

The example below is known as a forward selection procedure, where a full model is fit and compared against a null model, to assess the effect of the different predictors. For testing alternative models the Akaike's information criterion (AIC) is used. With AIC smaller is better.


```r
train <- subset(train, select = - c(ls_1, ls_2, ls_3, ls_4, ls_5, ls_6))

full <- lm(fragst ~ . - frags, data = train) # "~ ." includes all columns in the data set, "-" removes variables
null <- lm(fragst ~ 1, data = train) # "~ 1" just includes an intercept

add1(null, full, test = "F") # using the AIC test the effect of '
```

```
## Warning in add1.lm(null, full, test = "F"): using the 917/968 rows from a
## combined fit
```

```
## Single term additions
## 
## Model:
## fragst ~ 1
##              Df Sum of Sq    RSS    AIC  F value    Pr(>F)    
## <none>                    1932.5 685.56                       
## landform      1     51.85 1880.6 662.63  26.6328 2.986e-07 ***
## elev          1    201.73 1730.7 586.46 112.5984 < 2.2e-16 ***
## slope         1    142.30 1790.2 617.42  76.7885 < 2.2e-16 ***
## aspect        1     14.09 1918.3 680.85   7.0975 0.0078476 ** 
## twi           1    170.32 1762.1 602.96  93.3683 < 2.2e-16 ***
## twi_sc        1     80.06 1852.4 648.76  41.7517 1.641e-10 ***
## ch            1      9.89 1922.6 682.86   4.9703 0.0260152 *  
## z2str         1     47.35 1885.1 664.82  24.2627 9.884e-07 ***
## mrrtf         1     41.96 1890.5 667.43  21.4405 4.148e-06 ***
## mrvbf         1    195.50 1737.0 589.76 108.7258 < 2.2e-16 ***
## solar         1     17.00 1915.4 679.46   8.5758 0.0034866 ** 
## precip        1     29.49 1903.0 673.46  14.9720 0.0001164 ***
## precipsum     1    126.85 1805.6 625.30  67.8654 5.655e-16 ***
## temp          1    185.33 1747.1 595.11 102.4712 < 2.2e-16 ***
## pc_1          1    234.32 1698.1 569.03 133.2940 < 2.2e-16 ***
## pc_2          1    412.40 1520.0 467.44 262.0859 < 2.2e-16 ***
## pc_3          1      0.02 1932.4 687.56   0.0101 0.9201030    
## pc_4          1     14.13 1918.3 680.83   7.1168 0.0077642 ** 
## pc_5          1      0.00 1932.5 687.56   0.0019 0.9653904    
## pc_6          1     29.04 1903.4 673.68  14.7383 0.0001316 ***
## tc_1          1    176.14 1756.3 599.92  96.8792 < 2.2e-16 ***
## tc_2          1      4.70 1927.8 685.33   2.3545 0.1252508    
## tc_3          1    416.27 1516.2 465.10 265.2186 < 2.2e-16 ***
## k             1      3.05 1929.4 686.11   1.5295 0.2164953    
## th            1      0.92 1931.5 687.13   0.4615 0.4970728    
## u             1     29.33 1903.1 673.54  14.8887 0.0001216 ***
## cluster      12    500.29 1432.2 434.83  27.8003 < 2.2e-16 ***
## mast          1    185.33 1747.1 595.11 102.4712 < 2.2e-16 ***
## cluster_mast  1     87.72 1844.7 644.96  45.9365 2.122e-11 ***
## cluster2     12    500.29 1432.2 434.83  27.8003 < 2.2e-16 ***
## gsi           1    409.27 1523.2 469.33 259.5586 < 2.2e-16 ***
## sw            1      3.48 1929.0 685.91   1.7424 0.1871459    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

We can see as the correlation matrices showed earlier that the pc predictors have some of the smallest AIC and reduce the deviance the most. So lets add pc\_2 to the `null` model using the `update()` function. Then continue using the `add1()` or `drop1()` functions, until the model is saturated.


```r
fragst_lm <- update(null, . ~ . + pc_2) # add one or several variables to the model 

# or refit

# fragst_lm <- lm(fragst ~ pc_2, data = train)

# add1(fragst_lm, full, test = "F") # iterate until the model is saturated

# drop1(fragst_lm, test = "F") # test effect of dropping a predictor

fragst_lm <- lm(fragst ~ pc_2 + pc_1 + temp + twi + precipsum, data = train)

summary(fragst_lm)
```

```
## 
## Call:
## lm(formula = fragst ~ pc_2 + pc_1 + temp + twi + precipsum, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.1703 -0.6741  0.0487  0.7576  3.7761 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -5.3226111  0.9232371  -5.765 1.12e-08 ***
## pc_2        -0.0390539  0.0058898  -6.631 5.71e-11 ***
## pc_1         0.0088757  0.0009487   9.356  < 2e-16 ***
## temp         0.2183623  0.0296425   7.367 3.92e-13 ***
## twi         -0.0855969  0.0151303  -5.657 2.06e-08 ***
## precipsum    0.0578040  0.0103655   5.577 3.23e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.159 on 912 degrees of freedom
##   (50 observations deleted due to missingness)
## Multiple R-squared:  0.3675,	Adjusted R-squared:  0.364 
## F-statistic:   106 on 5 and 912 DF,  p-value: < 2.2e-16
```

We can continue adding predictors to the model until we no longer see an increase in the adjusted R2. At some point the adjusted R2 will level off, versus the R2 which will continue to incrementally increase. The difference between the adjusted R2 vs the R2, is that the adjusted R2 is penalizes model for each additional predictor added to model, similarly to the AIC.

After we've constructed a model 


```r
# test <- CVlm(na.exclude(train), formula(fragst_lm), m = 10, plotit = FALSE)
# with(test, cor(frags, ilogit(cvpred) * 100)^2) # cv r2

anova(fragst_lm)
```

```
## Analysis of Variance Table
## 
## Response: fragst
##            Df  Sum Sq Mean Sq F value    Pr(>F)    
## pc_2        1  413.65  413.65 307.886 < 2.2e-16 ***
## pc_1        1  192.37  192.37 143.180 < 2.2e-16 ***
## temp        1   28.54   28.54  21.243 4.623e-06 ***
## twi         1   35.61   35.61  26.504 3.223e-07 ***
## precipsum   1   41.78   41.78  31.099 3.231e-08 ***
## Residuals 912 1225.30    1.34                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
par(mfrow = c(1, 4))
plot(fragst_lm)
```

![](6_Linear_models_files/figure-html/unnamed-chunk-8-1.png)

```r
dev.off()
```

```
## null device 
##           1
```

```r
par(mfrow = c(1, 4))
termplot(fragst_lm, partial.resid = TRUE)
dev.off()
```

```
## null device 
##           1
```

```r
vif(fragst_lm)
```

```
##      pc_2      pc_1      temp       twi precipsum 
##    3.1829    1.6868    3.9061    1.5162    2.6580
```

```r
train$predict <- ilogit(predict(fragst_lm, train)) * 100

with(train, plot(frags, predict, xlim = c(0, 100), ylim = c(0, 100)))
abline(0, 1)

with(train, sqrt(mean((frags - predict)^2, na.rm = T))) # RMSE = root mean square error
```

```
## [1] 20.8915
```

```r
with(train, mean(abs(frags - predict), na.rm = T)) # MAE = mean absolute error
```

```
## [1] 15.77283
```

```r
temp <- by(train, list(train$cluster2), FUN = function(x) 
  with(x, data.frame(
  cluster = unique(cluster2), 
  rmse = round(sqrt(mean((frags - predict)^2, na.rm = T))), 
  n = length(frags)
  )))
temp2 <- do.call(rbind, temp)
temp2
```

```
##    cluster rmse   n
## 4        4   33   2
## 5        5   22 112
## 15      15   25  97
## 14      14   20  69
## 12      12   15  63
## 11      11   17 107
## 10      10   21  50
## 2        2   23 149
## 13      13   24  42
## 3        3   10  78
## 6        6   25  57
## 7        7   25  69
## 8        8    7  24
```

```r
# or using dplyr package
# 
# group_by(train, cluster2) %>% summarize(
#   rmse = round(sqrt(mean((frags - predict)^2, na.rm = T))), 
#   n = length(frags)
# )

# or using plyr
# 
# ddply(train, .(cluster2), summarize,
#   rmse = round(sqrt(mean((frags - predict)^2, na.rm = T))), 
#   n = length(frags)
# )

dotplot(rmse ~ cluster, data = temp2)

# fragst_lm <- update(null, . ~ . + pc_2 + pc_1 + temp + twi + precipsum + cluster) # add one or several variables to the model 
```


```r
predfun <- function(model, data) {
  v <- predict(model, data, se.fit=TRUE)
  cbind(p = as.vector(ilogit(v$fit) * 100), se=as.vector(ilogit(v$se.fit)) * 100)
}

r <- predict(geodata_r, fragst_lm, fun = predfun, index = 1:2, progress="text")
writeRaster(r[[1]], "frags.tif", overwrite = T, progress = "text")
writeRaster(r[[2]], "frags_se.tif", overwrite = T, progress = "text")

plot(raster("C:/workspace/test_fragst4.tif"))
plot(ca794, add = TRUE)
```

# References

Nauman, T. W., and J. A. Thompson, 2014. Semi-automated disaggregation of conventional soil maps using knowledge driven data mining and classification trees. Geoderma 213:385-399. [http://www.sciencedirect.com/science/article/pii/S0016706113003066](http://www.sciencedirect.com/science/article/pii/S0016706113003066)

Peterson, F.F., 1981. Landforms of the basin and range province: defined for soil survey. Nevada Agricultural Experiment Station Technical Bulletin 28, University of Nevada - Reno, NV. 52 p. [http://jornada.nmsu.edu/files/Peterson_LandformsBasinRangeProvince.pdf](http://jornada.nmsu.edu/files/Peterson_LandformsBasinRangeProvince.pdf)

Shi, X., L. Girod, R. Long, R. DeKett, J. Philippe, and T. Burke, 2012. A comparison of LiDAR-based DEMs and USGS-sourced DEMs in terrain analysis for knowledge-based digital soil mapping. Geoderma 170:217-226. [http://www.sciencedirect.com/science/article/pii/S0016706111003387](http://www.sciencedirect.com/science/article/pii/S0016706111003387)


# Additional reading

Faraway, J.J., 2002. Practical Regression and Anova using R. CRC Press, New York. [https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf](https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf)

Gareth, J., D. Witten, T. Hastie, and R. Tibshirani, 2014. An Introduction to Statistical Learning: with Applications in R. Springer, New York. [http://www-bcf.usc.edu/~gareth/ISL/](http://www-bcf.usc.edu/~gareth/ISL/)

Hengl, T. 2009. A Practical Guide to Geostatistical Mapping, 2nd Edt. University of Amsterdam, www.lulu.com, 291 p. ISBN 978-90-9024981-0. [http://spatial-analyst.net/book/system/files/Hengl_2009_GEOSTATe2c0w.pdf](http://spatial-analyst.net/book/system/files/Hengl_2009_GEOSTATe2c0w.pdf)

Webster, R. 1997. Regression and functional relations. European Journal of Soil Science, 48, 557-566. [http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2389.1997.tb00222.x/abstract](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2389.1997.tb00222.x/abstract)
