---
title: Chapter 2 The data we use
author: Tom D'Avello, Jay Skovlin, Stephen Roecker
date: "Friday, February 27, 2015"
output: html_document
html_document:
    keep_md: yes
---
![Statistics for pedologists course banner image](figure/logo.jpg)  

# Chapter 2: The data we use

- [2.1 Data Types](#datatypes)
- [2.2 Accuracy, precision, and significant figures](#acc) 
- [2.3 Data in R](#tidydata)
- [2.3.1 Basic data objects](#datatypeinr)
- [2.4 The soil project collection (spc) object](#aqp)
- [2.6 R Tools](#rtools)   
- [2.4 ArcGIS Tools](#tools)  
- [2.5 TEUI Tools](#teui)       
- [2.7 References](#ref)

 
##<a id="datatypes")></a>2.1  Data types   

Data type refers to the _measurement scale_ used. The data type controls the type of statistical operation that can be performed [(Stevens, 1946)](#ref). Four measurement scales, in order of decreasing precision are recognized:  

**Ratio** - measurements having a constant interval size and a true zero point. Examples include: measurements of length, weight, volume, rates, length of time, counts of items and temperature in Kelvin.

**Interval** - measurements having a constant interval size but no true zero point. Examples include: temperature (excluding Kelvin), direction (e.g. slope aspect), time of day. Specific statistical procedures are available to handle circular data like slop aspect.

**Ordinal** - members of a set are differentiated by rank. Examples include Soil interpretation classes (e.g., slight, moderate, severe), soil structure grade (e.g.,structureless, weak, moderate, strong) . 

**Nominal** - members of a set are differentiated by kind. Examples include: Vegetation classes, soil map units, geologic units.

In addition to measurement scale, data types can be categorized by whether their scales are continuous or discontinuous.

**Continuous data** - any measured value.  This includes: ratio and interval scales. Data with a possible value between any observed range. For example, the depth of an Ap horizon could range from 20cm to 30cm, with an infinite number of values between, limited only by the precision of the measurement device.

**Discrete data** - data with exact values. This includes: ordinal and nominal scales. For example, the number of Quercus alba seedlings observed in a square meter plot, the number of legs on a dog, the presence/absence of a feature or phenomenon.

##<a id="acc")></a>2.2  Accuracy, precision, and significant figures  

**Accuracy** - is the closeness of a number to its actual value.

**Precision** - is the closeness of repeated measurements to each other.

**Significant figures** - the digits in a number that define the precision of a measurement. The value of 6 cm has one significant digit. The implied range is 1 cm. The true value lies between 5.50 and 6.49. The value of 6.2 cm has two significant digits. The implied range is 0.1 cm. The true value lies between 6.150 and 6.249. The implied precision is greater for the number 6.0 cm than 6 cm. See page 37 of the [Keys to Soil Taxonomy](http://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/class/taxonomy/?cid=nrcs142p2_053580) for a discussion of how significant figures are applied in Soil Taxonomy. 

##<a id="tidydata")></a>2.3  Tidy data  

When preparing data for statistical analysis, a nicely formatted summary table is not appropriate. The data needs to be in its most raw format, similar to how many tables are structured in NASIS. The generally preferred configuration is a comma delimited text file, where columns contain variables and rows contain individual observations of the variables. Using sand content as an example, you might collect or present your data like this:  

![R GUI image](figure/ch2_fig1.jpg)  

However, for analysis purpose the data needs to be organized with total sand content as one long column with headers for organization. It is also best to remove spaces from column headers beforehand, and simplify and standardize the coding of categorical variables.  There should be only 1 header row followed by data as noted in this formatted table. For an exhaustive discussion on tidy data see [Wickham (2014)](http://www.jstatsoft.org/article/view/v059i10).


```r
sand <- read.csv("C:/workspace/stats_for_soil_survey/trunk/data/sand_example.csv")

library(knitr)

kable(head(sand))
```



|location |landuse |master | depth| sand|
|:--------|:-------|:------|-----:|----:|
|city     |crop    |A      |    14|   19|
|city     |crop    |B      |    25|   21|
|city     |pasture |A      |    10|   23|
|city     |pasture |B      |    27|   34|
|city     |range   |A      |    15|   22|
|city     |range   |B      |    23|   23|
The same table in a format suitable for use by R  


##<a id="dataobjects")></a>2.4 Data structures in R 

R recognizes a dozen or so data structures including: vectors, lists, arrays, matrices, data frames, and several spatial data formats. As a soil scientist, we most often deal with data frames, like the sand file we imported into R in Chapter 1. It is important to understand what data structure you are using or creating and how it is handled in R. 


**Vectors**  
Vectors are the most fundamental data structure in R. All other data structures are simply some combination of vectors. Vectors are 1-dimensional ordered collections of individual elements. You can think of these as a column from a table. These elements can be numerical, character, or logical. Examples include:


```r
vector1 <- c(0, 1, 2, 3, 4, 5, NA) # integers
vector2 <- seq(1.1, 2, 0.1) # numeric
vector3 <- c("a", "b", "c", "d", NA) # characters
vector4 <- c(TRUE, FALSE, NA) # logical

vector1
```

```
## [1]  0  1  2  3  4  5 NA
```

```r
vector2
```

```
##  [1] 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0
```

```r
vector3
```

```
## [1] "a" "b" "c" "d" NA
```

```r
vector4
```

```
## [1]  TRUE FALSE    NA
```

**Matrices**  

Matrices are 2-dimensional vectors that are limited to columns having the same mode (e.g. numeric, character, logical, etc.) and same length. A common command for creating a matrix in R is the `matrix()` function that requires the following inputs: matrix (vector, number of rows, number of columns).  


```r
m <- matrix(1:10, nrow = 5, ncol = 2)
m
```

```
##      [,1] [,2]
## [1,]    1    6
## [2,]    2    7
## [3,]    3    8
## [4,]    4    9
## [5,]    5   10
```

**Arrays**    
Arrays are multi-dimensional matrices that are also limited to columns having the same mode (e.g. numeric, character, logical, etc.) and length. The `array()` function creates arrays. The "dim" option gives the number of rows, columns, and layers, in that order.  


```r
a <- array(1:8, dim = c(4, 2, 2))
a
```

```
## , , 1
## 
##      [,1] [,2]
## [1,]    1    5
## [2,]    2    6
## [3,]    3    7
## [4,]    4    8
## 
## , , 2
## 
##      [,1] [,2]
## [1,]    1    5
## [2,]    2    6
## [3,]    3    7
## [4,]    4    8
```

**Lists**  
Lists are ordered collections of multiple R objects. In the example below, the R objects created are land use, location, and sand. The `list()` function simply serves as a storage bin for land use, location, and sand. Many outputs of R functions are actually lists.


```r
landuse <- sand$landuse
location <- sand$location
sand2 <- sand$sand

sand_list <- list(landuse, location, sand2)
sand_list
```

```
## [[1]]
##  [1] crop    crop    pasture pasture range   range   crop    crop   
##  [9] pasture pasture range   range   crop    crop    pasture pasture
## [17] range   range  
## Levels: crop pasture range
## 
## [[2]]
##  [1] city city city city city city farm farm farm farm farm farm west west
## [15] west west west west
## Levels: city farm west
## 
## [[3]]
##  [1] 19 21 23 34 22 23 31 35 30 36 25 29 27 25 21 26 23 24
```


**Data frames**  

Data frames, like the sand data set, are actually lists that are formatted to resemble tables. Data frames are similar to matrices, but allow different columns to have different modes (e.g. numeric, character, factor, etc.).


```r
sand
```

```
##    location landuse master depth sand
## 1      city    crop      A    14   19
## 2      city    crop      B    25   21
## 3      city pasture      A    10   23
## 4      city pasture      B    27   34
## 5      city   range      A    15   22
## 6      city   range      B    23   23
## 7      farm    crop      A    12   31
## 8      farm    crop      B    31   35
## 9      farm pasture      A    17   30
## 10     farm pasture      B    26   36
## 11     farm   range      A    15   25
## 12     farm   range      B    24   29
## 13     west    crop      A    13   27
## 14     west    crop      B    29   25
## 15     west pasture      A    11   21
## 16     west pasture      B    31   26
## 17     west   range      A    14   23
## 18     west   range      B    24   24
```

##<a id="rtools")></a>2.4  R tools for extracting spatial data

In soil survey we're typically interested in the values for spatial data at point locations or whole polygons. This gives us information the geomorphic setting of our soil observations. With this information we would like to predicts the spatial distribution of soil properties and classes at unobserved sites.

### Extracting point data from a raster

The examples presented in sections 5.1 and 5.2 summarize using traditional metrics like, mean and standard deviation. Other statistics like quantiles are more robust for soil survey purposes. The following script will generate a statistical summary of slope by MUSYM for a SSURGO shapefile. The raster data should be in a common GDAL format like IMAGINE (img) or TIFF.  

**Step 1.**  

Copy this script and paste to an empty Notepad document and save as *��mapunit\_summary\_slope.R* in the same folder where your SSURGO shapefile and slope layer resides.  


```r
# R function to generate statistical summary of MUSYM elevation, slope, aspect, etc. 
# S. Roecker NRCS
# set the following parameters accordingly
# shapefile = shapefile of MUSYM polygons to sample
# sampleSize = number of samples to systematically (e.g. grids) select from MUSYM polygons
# Increasing the sampleSize will increase processing time.
# Excessive sampleSize of 100,000 will generally yield similar statistics as a sampleSize of 10,000 depending on the size of area sampled. 
# Example 
# MUSYM.summary("smr_ca795_a.shp", 10000)

MUSYM.summary=function(shapefile,sampleSize){
  # Import shapefile
  library(maptools)
  gpclibPermit()
  mapunit=readShapePoly("coshocton_mapunits.shp") # Replace this shapfile with your shapefile of interest

  # Sample systemtic regular grid
  library(sp)
  mapunit.sample=spsample(mapunit,n=10000,"regular")
  mapunit.table=cbind(mapunit.sample,over(mapunit.sample,mapunit))
  mapunit.df=as.data.frame(mapunit.sample)

  # Load grids
  grid.list=c("slope30.img") # Replace this slope raster with your slope layer 

  library(raster)
  geodata=stack(c(grid.list))
  NAvalue(geodata)=-99999

  grid.names=c("Gradient")
  names(geodata)=c(grid.names)

  # Extract geodata
  musym.geodata.sample=cbind(extract(geodata,mapunit.sample))
  
  # Prep data
  data<-cbind(mapunit.table,musym.geodata.sample)
  data$MUSYM=as.factor(data$MUSYM)
  data$Gradient<-round(data$Gradient,0);  
  Gradient_levels<-c(0,3,8,15,25,35,70,350); data$Gradient_levels<-cut(data$Gradient,breaks=Gradient_levels); levels(data$Gradient_levels)<-c("0-3","3-8","8-15","15-25","25-35","35-70","70-350")
  write.csv(data,file="data.csv")
  
  ###Create descriptive and graphical summary of map unit###
  print(summary(data))
  library(Rcmdr)
  library(abind)  
  MUSYM.gradient.percentages=round(rowPercents(xtabs(~MUSYM+Gradient_levels, data=data)),0)
    
  write.csv(MUSYM.gradient.percentages,file="MUSYM.gradient.percentages.csv")
  
  print(MUSYM.gradient.percentages)
    
  library(plyr)
  MUSYM.gradient.quantiles<-ddply(data,.(MUSYM),function (x) round(quantile(x$Gradient,probs=c(0,0.05,0.5,0.95,1),na.rm=TRUE),0))

  write.csv(MUSYM.gradient.quantiles,file="MUSYM.gradient.quantiles.csv")

  print(MUSYM.gradient.quantiles)
}

# R function to plot graphical summary of MUSYM elevation, slope, aspect, etc. 
# set the following parameters accordingly
# mapunit = data frame generated from MUSYM.summary() function
# Example - copy and paste the next two lines at R prompt
# data<-read.csv("data.csv")
# MUSYM.plot(data)
#
# Different break methods
#Hist(data.sub$Gradient,scale="percent",breaks="Sturges",col="light green",xlab="Slope gradient (%)",ylab="Percent",main=paste("Map Unit ",MUSYM.list[i],sep=""))
#Hist(data.sub$Gradient,scale="percent",breaks="scott",col="light green",xlab="Slope gradient (%)",ylab="Percent",main=paste("Map Unit ",MUSYM.list[i],sep=""))


MUSYM.plot=function(mapunit){
  library(Rcmdr)
  data=mapunit
  data$MUSYM=as.factor(data$MUSYM)
  MUSYM.list=levels(data$MUSYM)
  for(i in 1:length(MUSYM.list)){
    data.sub=subset(data,MUSYM==MUSYM.list[i])
    png(file=paste(MUSYM.list[i],".png",sep=""),width=8.5,heigh=11,units="in",res=200,pointsize=10)
    par(mfcol=c(3,3))
    par(pty="m")
    par(cex=0.8)
    par(cex.axis=0.8)
  
    Hist(data.sub$Gradient,scale="percent",breaks="FD",col="light green",xlab="Slope gradient (%)",ylab="Percent",main=paste("Map Unit ",MUSYM.list[i],sep=""))
    dev.off()
    }
} 
```

**Step 2.**  

Edit the script to make sure the shapefile (coshocton_mapunits.shp in this example) and slope layer (slope30.img in this example) names match your file names. There are two places, noted by comments, where the edits should be made. In addition, portions of the script related to Gradient Levels should be edited to match your slope breaks. The script uses slope break of 0-3, 3-8, 8-15, 15-25, 25-35, 35-70, 70-350) 

**Step 3.**  

Open R  
C
hange the Directory to the location of your data and script  

Source R code  

![R GUI image](figure/ch5_fig52.jpg)  

At the R prompt type (make sure you use the filename that matches your script and data)  

>MUSYM.summary("coshocton_mapunits", 10000)  

**Step 4.**  

The script creates three *csv* files:  

**data.csv** summary by polygon of mean gradient and gradient level the polygon fits. An excerpt of the table from Excel with polygons highlighted that are outside the range of the named slope class:  

![R GUI image](figure/ch5_fig53.jpg)  

**MUSYM.gradient.percentages.csv** –  percent composition of the map unit by slope class. Values of NaN indicate an insufficient population to sample.  

![R GUI image](figure/ch5_fig54.jpg)  

**MUSYM.gradient.quantiles.csv** – slope value of corresponding quantile. Using CnB in the example below, the minimum slope is 1%, the median is 6%, and the maximum is 30%. The data range for the 5th to 95th quantile (percentile) is 2-14. This indicates that 90% of the map unit has a slope between 2 – 14%.  

![R GUI image](figure/ch5_fig55.jpg)  

**Step 5.**  

Optional histogram representation of slope distribution by mapunit  

At the R prompt type  

```r
data<-read.csv("data.csv")
MUSYM.plot(data)
```

This will produce “png” files for all MUSYMS:  

![R GUI image](figure/ch5_fig56.jpg)  


##<a id="tools")></a>2.5  ArcGIS tools for extracting spatial data

### Extracting point data from a raster

This section discusses the use of *Extract Multi Values to Points*, which assigns the cell value of specified raster datasets to existing points. *Extract Values to Points* and *Sample* achieve similar results. These tools are described in the ESRI help section:  

[An_overview_of_the_Extraction_tools](http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#/An_overview_of_the_Extraction_tools/009z00000028000000/)  

To start assume you have 50 observations across your area of interest contained in a point file in ArcGIS with numerous observed soil properties. You would also like to consider variables like slope, profile curvature, solar insolation, topographic wetness index, relative position and elevation in your analysis. Before proceeding, it is preferable to meet the following conditions:  

 - All data conforms to a common projection and datum
 - All raster data have a common cell resolution
 - All raster data are co-registered, that is, the geographic coordinates of cell centers are the same for all layers. Setting the Snap Raster in the ArcGIS Processing Environment prior to the creation of raster derivatives can insure cell alignment. An ERDAS model is also available to perform this task.  
 
Using Extract Multi Values to Points is the most expedient way to populate raster values to a point file. _If your spatial extent is large and you have many raster layers, e.g. 12, it may be best to proceed using 3 or 4 rasters at a time and running the tool 3 or 4 times_.  

The Extract Multi Values to Points is found in the Extraction Tool Box in Spatial Analyst Tools  

![R GUI image](figure/ch2_fig3.jpg)  

Select your point file and the associated raster files of interest as noted in the example graphic  

![R GUI image](figure/ch2_fig4.jpg)  

The resulting point file will have the corresponding cell values for slope, profile curvature and wetness index attached to the point file:  

![R GUI image](figure/ch2_fig5.jpg)  

The resulting point file may also be saved as a text file for use in R.

ArcGIS also provides the capability of creating histograms for data associated with point files using the Geostatistical Analyst:  

![R GUI image](figure/ch4_fig12.jpg)  


### Extracting zonal statistics from a raster

Gathering statistics of raster for polygons datasets like SSURGO is typically achieved by the use of the *Zonal Statistics as Table* tool. The output will be a tabular summary for the specified *Zone*, usually map unit symbol or individual polygons.  

Example for summarizing by Map Unit Symbol:  

Open the Zonal Statistics as Table Tool in the Zonal Toolbox.

![R GUI image](figure/ch5_fig1.jpg)  

The input zone field is MUSYM and the input raster is slope:  

![R GUI image](figure/ch5_fig2.jpg)  

Which produces a table with the following output:  

![R GUI image](figure/ch5_fig3.jpg)  

The use of the Mean and Standard Deviation are acceptable, provided the distributions are close to normal and the interest is in summarizing the entire population of map units. To get a closer look at individual polygons, summarize using the FID:  

![R GUI image](figure/ch5_fig4.jpg)  

Which produces a table with the following output:  

![R GUI image](figure/ch5_fig5.jpg)  

Use the Join capability to associate the table of statistics to the spatial data:  

![R GUI image](figure/ch5_fig6.jpg)  

![R GUI image](figure/ch5_fig7.jpg)  

Which lets you view results by polygon and search for outliers or polygons that require further investigation:  

![R GUI image](figure/ch5_fig8.jpg)  

In this example, 4 polygons of *ChC2*, *Coshocton silt loam, 6 to 15 percent slopes, eroded*, have an average slope greater than 15 percent. Discrepancies like this will need to be investigated and resolved.  

![R GUI image](figure/ch5_fig9.jpg)  

In another example using a Box plot for assessment of a map unit with a slope class of 15 to 25 percent slopes, indicates half of the polygons with an average slope less than 15 percent:  

![R GUI image](figure/ch5_fig10.jpg)  


##<a id="teui")></a>2.6  TEUI tools

The TEUI toolkit works in a similar manner to *Zonal Statistics as Table*, with the added benefit of interactive graphics to aid in the assessment. TEUI is an ArcGIS Add-in and may be installed without Administrator privilege. Additional information and downloads are available here:  

[http://www.fs.fed.us/eng/rsac/programs/teui/downloads.html](http://www.fs.fed.us/eng/rsac/programs/teui/downloads.html)  

One advantage of TEUI is the ability to output tabular data at both the map unit and polygon level with one operation. SECTIONS 2 - 5 of the TEUI User Guide are excerpted below.  

**SECTION 2. Creating and opening an existing project**

The Toolkit requires the user to specify a folder location to store the database that contains the statistics and the file location of the data used to create them. This database can be an existing project folder location, but it is recommended that a new folder be created to reduce the number of extraneous file in a folder with other data.  

**Create a New Toolkit Project**  

 1.  On the TEUI Toolbar, select the Folder icon.
![R GUI image](figure/ch5_fig11.jpg)  

 2. A dialog window will appear.  
![R GUI image](figure/ch5_fig12.jpg)
 
 3.  Select Browse….  

 4.	In the Browse for Folder dialog, navigate to a location of your choice and select “Make New Folder”  

 5.	Type in an appropriate name for your project  

 6.	Click **OK**
  
**Open an Existing Toolkit Project**  

 1.  On the TEUI Toolbar, select the Folder icon ![R GUI image](figure/ch5_fig11.jpg)  
 
 
 2.  A dialog window will appear.  
 ![R GUI image](figure/ch5_fig13.jpg)  
 
 1.  If the project was recent, click on the project in the Recent dialog box and click Open.  

 2.	If it is older and does not appear in the dialog box, select **Browse**.and navigate to the project folder location. Select the project and click OK.  

**SECTION 3. Manage geospatial data and calculate statistics**

Managing project data with version 5.0 is very simple. The user simply points the program to the file location of the data on your computer that is to be used in the analysis.  

Note: _If you move the geospatial data used in an existing Toolkit project, you will need to re-link the Toolkit to the data when opening that project._  

**Add Analysis Features to a Toolkit Project**  

 1.  On the TEUI Toolbar, select the Data Manager icon. ![R GUI image](figure/ch5_fig14.jpg)  
 2.  The Data Manager dialog window will appear.  
 
 ![R GUI image](figure/ch5_fig15.jpg)  
 
**Analysis Features**: are zones within which you would like to generate statistics. These features
can be feature classes such polygons or points (line features will be available soon) as a shapefile (.shp), as a feature class within a file geodatabase, or within a feature dataset within a file geodatabase. Analysis features can also be a in the form of a discrete raster. The value field will be used as the identifier.  

**Rasters**: are the raster data that are to be used to generate the descriptive statistics. An example would be a digital elevation model (DEM), percent slope, aspect, or land use for a discrete raster.  

 3.  In the Analysis Features area, click on the Add Layer button.    
 
![R GUI image](figure/ch5_fig16.jpg)  

 4.  A data navigation window will appear. Navigate to the analysis layer you wish to use and select **Add**. You can add as many data layers as you wish to analyze.  

![R GUI image](figure/ch5_fig17.jpg)  

Note:_All data layers to be analyzed (analysis features and raster data) must have the same
geographic coordinate system and projection as each other You will receive a warning if they are different._   

**Remove Analysis Features from a Toolkit Project**  

 1. To remove an analysis layer from the Data Manager, click on the layer you wish to remove in the grey layer list, and click Remove. Click OK when prompted that this is correct.  
 
![R GUI image](figure/ch5_fig18.jpg)  
![R GUI image](figure/ch5_fig19.jpg)  

**Add Analysis Features to the ArcMap Table of Contents**  

 1.  To add the selected layer to the ArcMap table of contents, **select the layer you wish to add** by clicking on it in the grey layer list, and click the **Add to Table of Contents** button.    
 
![R GUI image](figure/ch5_fig20.jpg)  

**Choose a Map Unit**

_In some instances, such as TEUI mapping or Soil Survey, you may wish to identify individual polygon features as belonging to a specific map unit. This is simply a repeating identifying symbol or value which will be used to aggregate the statistics of the polygons belonging to that group. You do not need to select a map unit column in order to run the Toolkit. The symbol must be present as an attribute column in either the feature class layer or as an attribute in the VAT table of a discrete raster._  

 1.  Select the Analysis Feature layer that you wish to add a Map Unit symbol for.  

 2.	Select the appropriate map unit column attribute name for your map unit symbol in the drop down menu    
  
![R GUI image](figure/ch5_fig21.jpg) 

**Add Raster Data to the Toolkit Project**  

 1.  To add raster data to your Toolkit project, select the Add Layer button under the Raster heading  
![R GUI image](figure/ch5_fig22.jpg)  

  2.  In the Browse to Raster file location dialog window, navigate to the file location of the raster data you wish to add to the Toolkit project.  
  
![R GUI image](figure/ch5_fig23.jpg)  

 3.  Select the layer you wish to add and select the **Add** button.    

**Add Raster Data to the ArcMap Table of Contents**  


1.  To add raster data to your ArcMap Table of Contents, first select the layer or layers you wish to add by clicking on them in the grey layer list under the **Rasters** section.    


![R GUI image](figure/ch5_fig24.jpg)  

2.  Click the **Add to Table of Contents** button.  

3.	To add multiple raster data layers at once, hold down the **control button** while
**selecting the individual layers** you wish to add.  

**Calculate Statistics**  

 1.  To calculate statistics, you must first select the analysis feature layers and raster data layers you wish to run statistics on by **checking the box next each layer**.  
 
![R GUI image](figure/ch5_fig25.jpg)    

 2.  Click on the **Generate Stats** button. Select **Yes** when prompted by the time warning that appears. This process may take a significant amount of time depending on the amount of data selected to be run.  
 
![R GUI image](figure/ch5_fig26.jpg)  

 3.  A dialog will appear which informs the user of the duration, features rasters being used, and the progress. The tool can calculate more than 1 million cells a second.  
 
![R GUI image](figure/ch5_fig27.jpg)  

 4.  When finished click **OK** when the statistics run is done.  
 
**SECTION 4. Create and Visualize Graphs and Tables**  

A primary Toolkit feature is to be able to visualize the generated statistics in both a graph and table format. The Toolkit creates zonal statistics for the analysis features and raster data selected in the data manager. The results can be visualized in graph form or as summary table both by individual feature or by map unit if one was chosen.  

**Open a graph window**  

 1.  To create a graph, click on the **graph button** on the Toolkit Toolbar.  
 
![R GUI image](figure/ch5_fig28.jpg)  

 2.  A blank graph window will appear. You can open as many individual graph windows as you want.  
 
![R GUI image](figure/ch5_fig29.jpg)  

 3.  Click on the **Add Series** Tab in the upper left corner of the graph window.  
 
![R GUI image](figure/ch5_fig30.jpg)  

 4.  The Add Series window pane will open up. You can **pin the window open** by clicking on the sideways pin in the upper right hand of the Add Series window pane or close it by clicking on the vertical pin.  
 
![R GUI image](figure/ch5_fig31.jpg)  

 5.  In a step wise fashion, select… MUP\MU Comparison: This is the default option. This allows the greatest flexibility when comparing individual polygons against other individual polygons, individual polygons against map units, or map units against map units (if chosen).  
 
MU Comparison: This data view type contains unique default graphs when comparing map units or other aggregated statistics types. These graphs have the mean (solid line), range (darker colored area), and standard deviation (lighter colored area) on the same graph, for each polygon within a map unit.  

![R GUI image](figure/ch5_fig32.jpg)  

Chart type: You can choose the type of graph that best represents your data. The default is the area chart  
 
**Area chart**  
![R GUI image](figure/ch5_fig33.jpg)    

**Bar chart**  

![R GUI image](figure/ch5_fig34.jpg)    

**Point chart**  

![R GUI image](figure/ch5_fig35.jpg)  

**Line chart**  

![R GUI image](figure/ch5_fig36.jpg)  

**Radial chart**  

![R GUI image](figure/ch5_fig37.jpg)  

Feature Source: Choose which feature analysis layer you would like to graph. Polygons or discrete raster  

![R GUI image](figure/ch5_fig38.jpg)  

Map Unit: Chose from which map unit you would like to select a polygon. Alternatively, select just
the map unit you want to graph as a whole (i.e. don’t select an individual feature below). Also, you can leave this option blank which is the default.  

![R GUI image](figure/ch5_fig39a.jpg)  

Feature: Select the individual feature you would like to view statistics for. This is the Feature ID or FID  

![R GUI image](figure/ch5_fig39.jpg)  

Raster Source: Select the raster layer you wish to view the statistics from.  

![R GUI image](figure/ch5_fig40.jpg)  

Raster Band: Select the raster band from the raster layer.  

![R GUI image](figure/ch5_fig41.jpg)  

Set Graph: Click once finished.  

![R GUI image](figure/ch5_fig42.jpg)  

**Common tasks with graphs**

**How do I add more individual features or map units to same graph?**
Within the Add Series pane, simply select a new feature source (if desired), map unit, feature, or raster source. Click Set Graph and the new feature or map unit will be added to the graph and the legend in a new color.  

![R GUI image](figure/ch5_fig43.jpg)  

**Can I add multiple axes to the graph? For example, a graph of elevation and percent slope?**  
To add another axis to your graph window, simply select the feature source, a map unit if desired, or a feature if desired, but select a new raster data source. You will notice in the graph legend map units and features are separated by the raster layer.  

![R GUI image](figure/ch5_fig44.jpg)  

**Is there a way to find a specific value on the graph?**  
If you hover over a specific spot in the graph, a line and window will appear with the specific values that your cursor is on. This is valuable if you are trying to identify outliers in our data.  

![R GUI image](figure/ch5_fig45.jpg)  

**Can I normalize the data ranges?**  
To normalize data so that the data ranges are comparable within the graph window, check the normalize button at the top of the graph.  

![R GUI image](figure/ch5_fig46.jpg)  

**How do I remove a graph series from the graph window?**  
Simply hit the red X next to the graph series you would like to remove in the graph legend.  

![R GUI image](figure/ch5_fig47.jpg)  

**How do I view the tabular data associated with each map unit or feature in the graph window?**  
To view the tabular and descriptive statistics of each map unit or feature in the graph window,
simply select the table icon next to the data series in the graph legend.  

![R GUI image](figure/ch5_fig48.jpg)  

**SECTION 5. Exporting Statistics Data to Excel**  

The statistical data summaries produced by the toolkit can be exported to excel as a .CSV file for further use.  

**Export individual feature statistics**  

![R GUI image](figure/ch5_fig49.jpg)  

 1.  On the main menu bar, click the **export table button** with the green arrow. Note this may take a little while as the program is gathering up the data.  

 2.	Navigate to the location you would like to save the .CSV file. Give the file a name and click
**Save**.  

![R GUI image](figure/ch5_fig50.jpg)  

**Export map unit feature statistics**  

![R GUI image](figure/ch5_fig51.jpg)  

 1.  On the main Toolkit menu bar, click the **export map unit table button** with the blue arrow. Note this may take a little while as the program is gathering up the data.  

 2.	Navigate to the location you would like to save the .CSV file. Give the file a name and click **Save**  .  
 

##<a id="ref")></a>2.7  References  

Stevens, S. S. (1946). [On the theory of measurement scales. Science, 103(2684)](http://www.sciencemag.org/content/103/2684/677.full.pdf)  


##<a id="ref")></a>2.8  Additional reading

[Venables, W. N., D. M. Smith and the R Core Team, 2015. Introduction to R, Notes on R: A Programming Environment for Data Analysis and Graphics Version. (3.2.3, 2015-12-10)](https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf)


