# DRAFT R Reference Card for Soil Scientists
Katey Yoast, Jay Skovlin, Dylan Beaudette, Stephen Roecker, Tom D'Avello, Skye Wills  
April 2016  

</br>
R and RStudio have been installed on all computers with NASIS and are typically updated and CCE approved once a year. USDA machines may be 1 to 3 versions behind the latest available version for public download. Having an outdated version of R though rarely creates a problem, although warnings may appear. RStudio is a powerful and productive user interface for R and is the recommended software for soil scientists.

Tips for using R:  

- R is command-line driven and requires you to type or copy and paste commands after the command prompt (>) that appears when you open R. After typing a command and pressing enter on your keyboard in the R console, the command will run. If your command is not complete, R will issue a continuation prompt (signified by a plus sign, `+`). 

- R has a built in editor where you can edit and select code to run. Some people find it easier to use Notepad instead. 

- R is case sensitive. Make sure your spelling and capitalization are correct!  
 
- Commands in R are also called functions. The basic format of a function in R is: `function.name(argument, options)`  
 
- The up arrow (^) on your keyboard can be used to bring up previous commands that you've typed in the R console. 
 
- The `$` is used to select a particular column within the dataset, `dataset$column`.

- Any text that you do not want R to act on (such as instructions, information, notes or comments) needs to be preceded by a `#`. R will ignore the remainder of the script line following `#`, if it is included in an R script. For example: `plot(x ~ y) # This text will not affect the plot function because of the comment`

</br>
**NRCS Resources for R**

[Soil Data Aggregation using R](https://www.youtube.com/watch?v=wD9Y0Qpv5Tw)

[Introduction to R Webinar](https://www.youtube.com/watch?v=G5mFt9k37a4)

[Statistics for Soil Survey](https://github.com/ncss-tech/stats_for_soil_survey)

[NCSS GitHub Repositories](https://github.com/ncss-tech)

[Package 'aqp' manual](https://cran.r-project.org/web/packages/aqp/aqp.pdf)

[Package 'soilDB' manual](https://cran.r-project.org/web/packages/aqp/soilDB.pdf)

[Package 'sharpshootR' manual](https://cran.r-project.org/web/packages/aqp/sharpshootR.pdf)

[Introduction to soilDB](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/soilDB-Intro.html?root=aqp) 

[Tips on Getting NASIS Data into R](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/fetchNASIS-mini-tutorial.html?root=aqp)

[AQP Tutorials](http://aqp.r-forge.r-project.org/)

[Region 11](https://ems-team.usda.gov/sites/NRCS_SSRA/mo-11/Soils%20%20GIS/Forms/AllItems.aspx?RootFolder=%2Fsites%2FNRCS%5FSSRA%2Fmo%2D11%2FSoils%20%20GIS%2Fguides%2FR&FolderCTID=0x0120007929E36D8FF15644B2C3F1488664C3CD&View=%7BFE55388F%2DFD5F%2D4A7B%2D98BD%2DA1F618066492%7D)


</br>

**Additional Resources for Learning R**

[Quick R website](http://www.statmethods.net/)

[Simple R tutorial](http://cran.r-project.org/doc/contrib/Verzani-SimpleR.pdf)

[R Manuals](http://cran.r-project.org/manuals.html)

[Comprehensive R Archive Network (CRAN) Task View](https://cran.r-project.org/web/views/)

[Gardener's own](http://www.gardenersown.co.uk/Education/Lectures/R/index.htm#nav)   

[David Rossiter's R applications and lecture notes](http://www.css.cornell.edu/faculty/dgr2/)

[Use R for Digital Soil Mapping Tutorial by Brendan Malone](http://www.clw.csiro.au/aclep/documents/DSM_R_manual_2013.pdf)

[NPS Inventory and Monitoring lecture notes](https://science.nature.nps.gov/im/datamgmt/statistics/r/index.cfm)

[Geographic Data Analysis and Visualization: Topics and Examples](http://geog.uoregon.edu/geogr/topics/)

</br>

#Import/Export/Save

```r
# set working directory
setwd("C:/workspace") #R uses / instead of \ for file paths

# view current working directory
getwd( )

# reads a file in a table format with a header row and create a dataframe called x
x <- read.table(filename, header = TRUE, sep = " ")
```

**Additional tips for `read.table( )`:**

--the default separator sep = " " is any whitespace

--use header = TRUE to read the first line as a header of column names

--use as.is = TRUE to prevent character vectors from being converted to factors

--use skip = n to skip n lines before reading data


```r
# reads a CSV file with a header row and create a dataframe called x
x <- read.csv("filename", header = TRUE)
```


```r
# writes a dataframe to a tab delimited text file with a header row
# use the 'sep = ' argument to specify additional output file formats
write.table(dataframe, file = "filename.txt", sep = "\t", header = TRUE)
```


```r
# writes a dataframe to a comma delimitated file with a header row
write.csv(dataframe, file = "filename.csv", header = TRUE)
```


```r
# saves all objects
save.image( )
```


```r
# saves R workspace to a file named workspace.RData
save.image(file = "workspace.RData")
```


```r
# saves a history of the commands that have been executed during a R session as sand.Rhistory
savehistory(file = "sand.Rhistory")  

# loads a history file (sand.Rhistory in this example) containing a list of previously executed commands 
loadhistory(file = "sand.Rhistory")

# displays a list of all previous commands (max.show = Inf)
# maximum number of lines displayed can be specified using the max.show argument
history(max.show = Inf)
```

</br>

#Packages
The aqp package will be used in the following examples:

```r
# installs a package and its dependencies
install.packages("aqp", dep = TRUE )
```


```r
# loads package 
library(aqp)
```


```r
# uninstalls package 
remove.packages(aqp)
```

</br>

#Help

```r
# help window for a command (fetchNASIS in this example); package argument is optional
help(fetchNASIS, package = soilDB)
```


```r
# placing a ? before a specified command will also open a help window if the package containing the command has been installed and loaded
?fetchNASIS
```

</br>

#Graphics

```r
# plot (numeric) data against the index (1:N)
plot(x)

# bivariate plot of x (on the x-axis) and y (on the y-axis)
plot(x,y)	
```


```r
# can be executed after a plot is generated to add labels to a plot
title(main = "NULL", sub = "NULL", xlab = "NULL", ylab = "NULL")
```


```r
# graphing parameters 
par( ) 

# adjust margin size (vector of 4 numbers, 1 per side) (default is c(5, 4, 4, 2) + 0.1)
par(mar = c(2, 2, 1, 1))

# adjust the number of plots per row and per column on the canvas (filled row-wise)
par(mfrow = c(2, 1))    #2 rows and 1 column
plot(x, y)
plot(x, z)
```

See the [par help page](http://stat.ethz.ch/R-manual/R-patched/library/graphics/html/par.html) for more graphing parameter options.


```r
# adds one or more straight lines to a plot
abline( )	
```


```r
# will close the active graphics window
dev.off( )	

# will open a new plot window
dev.new( )
```


```r
# Will close all graphics windows
graphics.off( )	
```

</br>

#Managing Variables and Objects

##Examining Data Structure

The letter "x" will be used to represent a R object in the following examples:


```r
# invokes a spreadsheet-style data viewer on a matrix or dataframe object
View(object)
```


```r
# displays a list of all objects in the search path
ls()
```


```r
# displays the internal structure of an R object 
str(x)

# displays the object class
class(x)
```


```r
# displays the column names of an R object 
names(x)

# displays the first (head) or last (tail) parts of a vector, matrix, table, data frame or function 
head(x)
tail(x)
```


```r
# returns the number of rows
nrow(x)

# returns the number of columns
ncol(x)

# returns the object length 
length(x)
```


```r
# returns the unique values for factors or characters in a vector, dataframe, or array
# levels(x) will produce the same output, but is only applicable to factors
unique(x)
```

**Example:** `unique(SPC$landform.string)` would return a list of all landforms in the SPC object

</br>

##Editing and Summarizing Data


```r
# prints a brief summary of the results of various model fitting functions or each column in a matrix or dataframe
summary(x)
```


```r
# coerces the column to a factor
as.factor(object$column)

# logical flag to determine if the levels in a factor should be regarded as ordered (in the order given)
ordered(object$column)

# coerces the column to be numeric
as.numeric(object$column)

# returns a permutation which rearranges its first argument into ascending or descending order
# order is used when dealing with a sequence of numeric, complex, character or logical vectors, all of the same length
# when dealing with tables, the sort( ) argument can be used to order data
order(object$column) 
order(-object$column)
```


```r
# removes rows in a dataframe, matrix, or vector that contain NA values 
na.omit(x)
```


```r
# invokes a spreadsheet-like text editor where edits can be saved if a new object is specified
new.object <- edit(x)
```


```r
# writes an ASCII text representation of a R object to a file or connection
# useful for converting a list (ex: "2011MT081001" "2011MT081009") into a comma delimited string (ex: c("2011MT0810001", "2011MT0810009")) for use in queries
dput(x, file = " ")
```


```r
# displays a contingency table of the counts at each combination of factor levels
table(x)
```

**Example:** `table(data$subgroup, data$order)` would print out a contingency table of frequency counts by subgroup and order


```r
# sorts (or order) a vector or factor (partially) in ascending or descending order
sort(x)
```

**Example:** `sort(table(object$subgroup), decreasing = TRUE)` would sort a table depicting a frequency count of subgroup in decreasing order

**Example:** `sort(unique(data$hzname))` would return a vector of unique horizon names in ascending order


```r
# the concatenate command combines values into a vector or list
# the output type is determined from the highest type of the components in the hierarchy NULL < raw < logical < integer < double < complex < character < list < expression
c( )
```


```r
# returns the TRUE indices of a logical object, allowing for array indices
which( )
```

**Additional syntax options for use in `which()` criteria:**

Operator | Meaning                                   | Example                                                             | Result
-------- | ----------------------------------------- | ------------------------------------------------------------------- | ----------------
!=       | not-equal-to character "string"           | `which(s$partsizeclass != 'sandy-skeletal')`                        | would return the row numbers of object s where the particle size class was everything except sandy-skeletal
==       | equal to                                  | `which(s$partsizeclass == 'sandy-skeletal')`                        | would return the row numbers of object s where the particle size class is sandy-skeletal
<  >     | less than, greater than                   | `which(s$depthtobedrock > 102)`                                     | would return the row numbers where depth to bedrock is greater than 102
<=  >=   | less than equal to, greater than equal to | `which(s$depthtobedrock >= 102)`                                    | would return the row numbers where depth to bedrock is greater than or equal to 102
%in%     | equivalent to IN() in SQL                 | `which(s$partsizeclass %in% c('loamy-skeletal', 'sandy-skeletal'))` | would return the row numbers of object s where the particle size class is sandy-skeletal or loamy-skeletal


```r
# a value with the same shape as test filled with elements from either yes or no 
ifelse(test, yes, no)
```

**Example:** `ifelse(as.numeric(data$lime) == 2, 1, 0)` would return a numerical vector of 1s and 0s, 1 representing lime = 2 and 0 representing lime = to anything other than 2)


```r
# the grepl( ) command will search for matches to an argument pattern within each element of a character vector
grepl(pattern, vector)
```

**Example:** `grepl('dyst', subgroup)` would pattern match to find the rows with the text `dyst` in the object called subgroup

**Additional syntax options for use in REGEX (regular expression) pattern matching:**

Operator | Meaning                                   | Example                                                             | Result
-------- | ----------------------------------------- | ------------------------------------------------------------------- | ----------------
|        | equiavlent to "OR" in SQL                 | `grep('loamy | sandy', f$partsize_class)`                           | would return TRUE for pedons with loamy or sandy partsize_class
^        | anchors to the left side of the string    | `grep('^sandy', f$partsize_class)`                                  | would return TRUE for pedons with partsize_class beginning with sandy
$        | anchors to the right side of the string   | `grep('skeletal$', f$partsize_class)`                               | would return TRUE for pedons with partsize_class ending in skeletal 
&        | equivalent to "AND" in SQL                | `grep('loamy & sandy', f$partsize_class)`                           | would return TRUE for pedons with loamy and sandy partsize_class


```r
# a generic command that evaluates expr in a local environment constructed from data. Data can be a list, a dataframe, or an integer. 
with(data, expr)
```

**Example:** `with(data, table(hillslope_pos, argillic.horizon, useNA = "ifany"))` would generate a frequency table from data with two columns: hillslope_pos and argillic.horizon. This table would include all NA values since useNA is set to "ifany".

**Example:** `with(data, round(prop.table(table(hillslope_pos, argillic.horizon, useNA = "ifany"), 2) * 100))` would calculate the portions (%) of hillslope_pos with and without argillic horizons described. The "2" calculates the proportions relative to the column totals and the *100 converts the output values to percentages.


```r
# subsets vectors, matrices or dataframes.
subset(data, select = )
```

**Example:** `subset(s, surface_gravel > 0, select= c(landform.string, surface.gravel))` would generate a table listing only the row numbers, surface.gravel, and landform.string where surface.gravel is greater than 0. 


```r
# splits the data into subsets, computes summary statistic for each, and returns the result in a convenient form
aggregate(column1 ~ column2, data = , statistic)
```

**Example:** `aggregate(clay ~ horizon, data = h, mean)` would generate a table of mean clay content for each horizon

**Example:** `aggregate(clay ~ horizon, data = h, summary)` would generate a table of the minimum, 1st and 3rd quantiles, median, mean, and maximum clay content for each horizon


```r
# rounds the values in its first argument to the specified number of decimal places (default 0)
round(x)
```

**Example:** `table(round(data$clay))` would generate a table with rounded clay values


```r
# adds column and row summaries to a table made up of one or more vectors (in this example, x and z)
addmargins(table(x, z))
```


```r
# returns a proportions table
prop.table(table(x), margin = NULL)*100
```

**Example:** `round(prop.table(table(data$horizon, data$texture_class), margin = 1) * 100)` would generate a rounded proportions table relative to the rows, margin = 1 calculates for rows, margin = 2 calculates for columns, margin = NULL calculates for total observations


```r
# combines two vectors (x and z in this example), creating a dataframe
data.frame(x, z)
```


```r
# binds columns or rows into a matrix
cbind( )
rbind( )
```


</br>

#Exploratory Data Analysis

##Distributions


```r
# computes a histogram of the given numerical vector. 
hist(x)
 
# in the lattice package, histogram( ) can be used for nicer histogram plots. In the example, 3 separate histograms (for clay, sand, and total_frags_pct) are plotted
histogram(~ clay + sand + total_frags_pct, data = h)
```

**Example:** `histogram(~ clay | horizon, data = h)` would plot multiple histograms, one for every horizon in the object h


```r
# plots the computed density plot
dp <- density

# in the lattice package, densityplot( ) produces a Kernel Density Plot 
plot(dp)
```

**Example:** `densityplot(~ clay + sand, data = h, auto.key = TRUE)` would compute a density plot with two curves (clay and sand), removing NA values


```r
# generates a QQplot with a line that represents the quantiles of a normal distribution
# if the data are normally distributed, the data points will fall very close to the QQline
qqnorm(x)

qqline(x)
```

</br>

##Central Tendency

```r
# returns the arithmetic average (mean) of a numerical vector after removing NA values	
mean(x, na.rm = TRUE) 
```


```r
# returns the middle measurement of a sample set, and as such is a more robust estimate of central tendency than the mean
# median is also known as the middle or 50th quantile, meaning that there are an equal number of samples with values less than and greater than the median

median(x)
```


```r
# returns the most frequent measurements in the sample (mode)
sort(table(round(data$column)), decreasing = TRUE)[1]
```

</br>

##Dispersion

```r
# computes the variance (sum of squares) of a numeric vector, matrix, or dataframe
var(x)

# computes the standard deviation of a numeric vector 
sd(x)
```


```r
# the default for the quantile( ) function returns the min, 25th percentile, median or 50th percentile, 75th percentile, and max from a numeric vector
quantile(x)

# returns the minimum and maximum values for a numeric vector
range(x)
```


```r
# produces a "box-and-whiskers" plot where y represents a numeric vector of data values to be split into groups according to variable grp (usually a factor) 
boxplot(y ~ grp, data = )
plot(y ~ grp, data = )

# the bwplot command in the lattice package can also be used to generate boxplots
bwplot(y ~ grp, data = )	
```


</br>

##Association


```r
# creates an object called vars that contains numerical vectors (clay, sand, and phfield in the example below)
vars <- c("clay", "sand", "phfield")

# creates a correlation matrix (table of the calculated correlation coefficients)
round(cor(data[vars], use = "complete.obs"), 2) #2 specifies decimal places
```


```r
# generates a scatterplot of two numeric variables (in this example, x and z)
plot(x ~ z, data = )

# scatterplot command in the lattice package
xyplot(x ~ z, data = )

# in the lattice package, produces a scatterplot matrix for all numeric variables (vars) in a dataset
splom(data[vars])

# generates a scatterplot matrix for all numeric variables (vars)
pairs(data[vars])
```


```r
# in the circular package, the circular ( ) command creates a circular object (in the example, aspect) to provide a numerical summary
aspect <- data$aspect_field
aspect <- circular(aspect, template = "geographic", units = "degrees", modulo = "2pi")

# also in the circular package, the rose.diag( ) command generates a rose diagram plot of circular data (in this example, aspect) 
rose.diag(aspect, bins = 8, col = "grey")
```

</br>

#Spatial

##Points and General Mapping Commands


```r
# in the sp package, the coordinates command promotes a dataframe to a spatial object or retrieves spatial coordinates from a Spatial object
coordinates(dataframe) <- ~ x + y

# in the sp package, the proj4string command sets the projection of a spatial object. In this example, the projection is set to WGS84 
proj4string(spatialobject) <- '+proj = longlat + datum = WGS84'
```

[Projection Codes](https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf)


```r
# in the maps package, will plot counties and states. The xlim = c( , ) and ylim = c( , ) parameters allow the user to "zoom in or out" by setting the x and y coordinate limits
map('county', 'statename')
```


```r
# a generic command to draw a sequence of points at the specified spatial extent 
points(object)
```


```r
# returns the bounding box coordinates for a spatial object
bbox(spatialobject)
```


```r
# in the rgdal package, the spTransform command will reproject spatial objects 
spTransform(spatialobject, CRS("+init = epsg:code"))
```

</br>

##Raster
The series of commands in the example below use the raster package to specify a file path, concatenate a list of raster files in the specified file path, and combine the file directory and file names so that the rasters can be stacked: 


```r
# set file path
folder <- "C:/geodata/"

# list of file names
files <- c(
  elev   = "ned30m.tif", 
  slope  = "ned30m_slope5.tif",    
  aspect = "ned30m_aspect5.tif",     
  twi    = "ned30m_wetness.tif")

# combine the folder directory and file names
geodata_f <- sapply(files, function(x) paste0(folder, x))

# create a raster stack
geodata_r <- stack(geodata_f)
```


```r
# alternatively, if all rasters are in the working directory (and only those rasters), the example below can be used to create a raster stack if the raster package is loaded
# the pattern argument can be used to specify input raster file extension

rasters <- stack(list.files(getwd( ),pattern = "img$", full.names = FALSE))
```


```r
# extracts raster data to point data (stored as a spatial object)
# in the example below, the output dataframe object "data" will contain the following columns: pedon_id, taxonname, x_std, y_std, tax_subgroup and extracted raster values for each row in the spatial object for each raster in the raster stack

data <- data.frame( as.data.frame(spatialobject)[c("pedon_id", "taxonname", "x_std", "y_std", "tax_subgroup")], extract(rasterstack, spatialobject))


# in the following example, the data2 dataframe will contain extracted raster values and the single column specified from the spatial object
data2 <- data.frame(spatialobject$column, extract(rasterstack, spatialobject))
```


```r
# export raster using the raster package
writeRaster(r, filename = "C:/workspace/raster.tif", format = "GTiff", progress = "text", overwrite = TRUE)
```


```r
# in the raster package, the projectRaster function will project the values of a raster object to a new raster object with another projection

projectRaster(from, to, method = " ")
```

</br>

##Vector

```r
# import vector data using the rgdal package
readOGR(dsn = "C:/workspace/polygon.shp", layer = "polygon")

# export vector data to ESRI shapefile using the rgdal package 
writeOGR(pol, dsn = "C:/workspace/polygon.shp", layer = "polygon", driver = "ESRI Shapefile", overwrite_layer = TRUE)
```

</br>

#soilDB and AQP

##Convenience Functions in the soilDB Package

- **fetchNASISLabData()**
    - Get KSSL laboratory pedon/horizon layer data from a local NASIS database.
- **fetchNASIS_component_data()**
    - Get selected NASIS mapunit and component data from a local NASIS database (experimental).
        - For more information, check out the following tutorial:
        [**NASIS component data**](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/NASIS-component-data.html?root=aqp).
- **fetchKSSL()**
    - Get KSSL data via BBOX, MLRA, or series name query, from the SoilWeb system.
        - For more information, check out the following tutorial:
        [**KSSL data demo**](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/NASIS-component-data.html?root=aqp).
- **fetchOSD()**
    - Fetch a limited subset of horizon- and site-level attributes for named soil series from the SoilWeb system.
- **fetchRaCA()**
    - Get Rapid Carbon Assessment (RaCA) data by State, geographic bounding-box, RaCA site ID, or series query from the SoilWeb system.
        - For more information, check out the following tutorial:
        [**RaCA data demo**](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/RaCA-demo.html?root=aqp).
- **fetchSCAN()**
    - Query soil and climate data from USDA-NRCS SCAN Stations (experimental).
- **fetchHenry()**
    - Download Data from the Henry Mount Soil Climate Database (experimental).
        - For more information, check out the following tutorial:
        [**Henry Mount Soil Climate Database Tutorial**](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/Henry-demo.html?root=aqp).
- **fetchPedonPC()**
    - Fetch commonly used site/horizon data from a PedonPC v.5 database.
- **SDA_query**
    - Submit queries to the Soil Data Access system.
        - For more information, check out the following tutorial:
        [**Soil Data Access Tutorial**](https://r-forge.r-project.org/scm/viewvc.php/%2acheckout%2a/docs/soilDB/SDA-tutorial.html?root=aqp).
        
</br>

##Get Functions in the soilDB Package
- **get('sites.missing.pedons', envir=soilDB.env)**
    - Returns user site ID's for sites missing pedons.
  
- **get('dup.pedon.ids', envir=soilDB.env)**
    - Returns pedon ID's for sites with duplicate pedon ID's.
  
- **get('bad.pedon.ids', envir=soilDB.env)**
    - Returns user pedon ID's for pedons with inconsistent horizon depths.
    
- **get('bad.horizons', envir=soilDB.env)**
    - Returns a dataframe of horizon-level information for pedons with inconsistent horizon depths.
  
- **get_extended_data_from_NASIS_db( )**
    - Extracts accessory tables and summaries from a local NASIS Database.
      
</br>

##Functions Specific to a Soil Profile Collection Object

In the examples below, SPC will be used to represent a soil profile collection object. 


```r
# returns a list of the site column names in the soil profile collection object 
siteNames(SPC)

# returns a list of the horizon column names in the soil profile collection object 
horizonNames(SPC)

# creates an object called s that contains the site information from a soil profile collection object
s <- site(SPC)

# creates an object called h that contains the horizon information from a soil profile collection object
h <- horizons(SPC)

# converts SPC into a data frame called x
x <- as(SPC, 'data.frame')
```


```r
# invokes a help document containing a detailed list of arguments and examples for plotting soil profile collections
?plotSPC
```


```r
# creates an object called x that contains only the site_id, x_std, and y_std columns out of the site table
x <- site(SPC)[, c('site_id', 'x_std', 'y_std')]
```


```r
# use of grepl( ) to filter and create an index, apply that index to the SPC, and plot the first 10 pedons in the newly created index object
# invert = TRUE would pattern match for everything except for lithic
idx <- grep('lithic', SPC$tax_subgroup, invert = FALSE)

# save this subset of 'lithic' soils for later use  
lithic.subgroup <- SPC[idx, ]

# or use the index directly to summarize a field
sort(table(f$part_size_class[idx]), decreasing = TRUE)

# plot first 10 profiles in lithic.subgroup index labeled by site_id
plot(lithic.subgroup[1:10, ], label = 'site_id')
```

If objects are analogous to nouns in a spoken language, then functions are analogous to verbs. A function defines an action that (usually) results in the creation of an object. An object that a function "acts on" is referred to as an "argument" of that function.


```r
# the example below pattern matches for "t" in the hzname table and creates a dataframe of pedons with hzname "t" with the peiid, phiid, hzname, hzdept, hzdepb, clay, and phfield columns 

findBtHorizons <- function(i) {
  # extract horizons for current profile
  h <- horizons(i) 
  # search for pattern 't' in horizon designations
  idx <- grep('t', h$hzname)
     # subset these horizons
  h2 <- h[idx, ] 
  # subset columns in resulting dataframe
  res <- h2[, c('peiid', 'phiid',  
  'hzname', 'hzdept', 'hzdepb', 
  'clay', 'phfield')]
  # return data
  return(res)}
```

"i" is a temporary object created inside of the context of this function based on the argument supplied; in the example above, the argument 'i' represents a single soil profile.


```r
# the profileApply function in the AQP package will take the function in the first example to left and apply it to all profiles in a soil 
l <- profileApply(SPC, FUN = findBtHorizons, simplify = FALSE)

# convert list into a dataframe
# ldply is a command in the plyr package
Bt.horizons <- ldply(l)
```

The `ddply( )` function is a convenient way to iterate over groups of rows in a dataframe and compute summaries (similar to GROUP BY in SQL). 

**Example:** `Bt.horizons.top <- ddply(Bt.horizons, 'peiid', summarise, depth_to_argillic_cm = min (hzdept))` would create a new object called Bt.horizons.top that would contain a summary of the minimum top depth of the argillic horionz for each peiid. 

This summary could then be added to the soil profile collection, joining on peiid:


```r
# rejoins the Bt.horizons.top dataframe to the site data in the SPC using the peiid
site(SPC) <- Bt.horizons.top

# NOTE: the when used in conjunction with site( ), the assignment operator performs a left-join
```


```r
# the slab command in the AQP package can be used to aggregate soil properties along user-defined 'slabs', and optionally within groups

slab( )
```

**Example:** `slab(SPC, fm = peiid ~ clay + sand, slab.structure = c(25,100), slab.fun = mean, na.rm = TRUE)` would generate a table summarizing the mean clay and mean of sand content for each peiid between 25 and 100 cm.

In the sharpshootR package, the `diagnosticPropertyPlot` command can be used to generate a graphical description of the presence/absence of soil diagnostic properties.


```r
diagnosticPropertyPlot(f, v, k, grid.label = 'pedon_id', dend.label = 'pedon_id')
# where: 
#   f = a SoilProfileCollection object
#   v = a character vector of site-level attribute names that are boolean (e.g. TRUE/FALSE) data
#   k = an integer, number of groups to highlight
#   grid.label = name of a site-level attribute (usually unique) annotating the y-axis of the grid
#   dend.label = name of a site-level attribute (usually unique) annotating dendrogram terminal leaves
```

**Example:**

```r
v <- c('ochric.epipedon', 'cambic.horizon', 'argillic.horizon', 'paralithic.contact', 'lithic.contact')

# generate diagnostic property diagram
diagnosticPropertyPlot(SPC, v, k = 5, grid.label = 'site_id', dend.label = 'taxonname', sort.vars = FALSE)
```


