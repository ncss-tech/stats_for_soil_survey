## ----echo=FALSE, results='hide', warning=FALSE, message=FALSE------------
library(knitr, quietly=TRUE)

opts_chunk$set(message=FALSE, warning=FALSE, background='#F7F7F7', fig.align='center', fig.retina=2, dev='png', tidy=FALSE, verbose=FALSE, antialias='cleartype', cache=FALSE)

# options for R functions
options(width=100, stringsAsFactors=FALSE)

## ----pedons_a, echo=FALSE, results='hide', warning=FALSE-----------------
library(ggplot2)
# assemble data on number of pedon by decade
#Pedons <- c(577, 6152, 9517, 19058, 42587, 112182, 231609, 184913)
#Year <- c("<1950s", "1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s")

Year <- as.numeric(c("1940", "1941", "1942", 
"1943", "1944", "1945", "1946", "1947", "1948", "1949", "1950", 
"1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", 
"1959", "1960", "1961", "1962", "1963", "1964", "1965", "1966", 
"1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", 
"1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", 
"1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", 
"1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", 
"1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", 
"2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", 
"2015", "2016", "2017"))
Pedons <- c(21, 2, 18, 26, 54, 48, 2, 23, 39, 71, 96, 116, 80, 
448, 692, 906, 992, 1107, 1168, 1367, 1249, 1461, 1384, 957, 
1176, 1107, 1101, 1011, 1076, 1362, 1866, 1586, 1646, 1444, 1953, 
2322, 2773, 3089, 3525, 3691, 3409, 3449, 3552, 3471, 4402, 4881, 
3691, 5124, 5334, 9658, 11554, 8485, 10120, 11694, 11038, 10869, 
12618, 12747, 12392, 13855, 14117, 18200, 15991, 15096, 23997, 
20265, 25705, 33700, 37313, 37368, 42288, 59900, 35772, 19062, 
26009, 12776, 11497, 11168)
Cummulative_Pedons <- c(310, 312, 330, 356, 
410, 458, 460, 483, 522, 593, 689, 805, 885, 1333, 2025, 2931, 
3923, 5030, 6198, 7565, 8814, 10275, 11659, 12616, 13792, 14899, 
16000, 17011, 18087, 19449, 21315, 22901, 24547, 25991, 27944, 
30266, 33039, 36128, 39653, 43344, 46753, 50202, 53754, 57225, 
61627, 66508, 70199, 75323, 80657, 90315, 101869, 110354, 120474, 
132168, 143206, 154075, 166693, 179440, 191832, 205687, 219804, 
238004, 253995, 269091, 293088, 313353, 339058, 372758, 410071, 
447439, 489727, 549627, 585399, 604461, 630470, 643246, 654743, 
665911)

#assemble into data.frame
#d <- data.frame(Year, Decade, Pedons, Pedons_c)

# cat("# pedons = ", formatC(sum(Pedons), big.mark = ",", format = "fg"), "\n", "# lab pedons = ~64,000", sep = "")

# plot number of pedons in NASIS 
ggplot(data.frame(Pedons, Year), aes(x=Year, y=Pedons)) + geom_histogram(stat="identity") + scale_y_continuous(name="Pedons", labels = scales::comma)

# plot the cummulative number of pedons in NASIS 
ggplot(data.frame(Cummulative_Pedons, Year), aes(x=Year, y=Cummulative_Pedons)) + geom_bar(stat="identity") + scale_y_continuous(name="Cummulative Pedons", labels = scales::comma)
#theme(axis.text.x = element_text(angle = 90, hjust = 1), cex=0.5)

## ----datatype2, eval=TRUE------------------------------------------------
  # Using the concatenate function we can create the following character and logical vectors
  # character vector: taxonomic subgroup
  subgroup <- c("typic haplocryepts","andic haplocryepts","typic dystrocryepts")  
  subgroup
  # logical vector: boolean field for presence or absence of andic soil properties diagnostic feature
  andic <- c(FALSE,TRUE,FALSE) 
  andic
  
  # Take our two character and logical vectors and convert them into a more useful dataframe.
  # we'll use the data.frame() function to glue these two vectors together into object 'd'
  d <- data.frame(subgroup, andic)
  d

## ----datatype2.1, eval=TRUE----------------------------------------------
  # get the column names of a dataframe
  names(d)
  # we can use 'names()' and 'c()' to rename the columns in a dataframe
  names(d) <- c('tax_subgroup', 'andic.soil.properties')
  d

## ----datatype2a, eval=TRUE-----------------------------------------------
  # format: dataframe_name[rows, columns]
  d[1, ] # first row of dataframe
  d[, 1] # first column of dataframe
  d[2, 2] # second row, second column
  
  # In dataframes we can also use the '$' symbol to reference vector columns within a specific dataframe object
  d$tax_subgroup
  
  # Other useful functions for checking objects and working with dataframes
  # the structure 'str()' function will show you the structure of an object and the data types of the vectors within it
  str(d)
  # 'class()' will tell you the object type or data type
  class(d)
  # use 'colnames()' to get a vector of column names from a dataframe
  colnames(d)
  # ncol and nrow give dimensions
  ncol(d)
  nrow(d)
  
  # building on what we've learned above, we can use the square bracket notation on a dataframe to re-order columns
  d <- d[ ,c('andic.soil.properties', 'tax_subgroup')]
  d
  # another way we could do this is to use the column indexes within the concatenate function
  d <- d[ , c(2,1)]

## ----structure_diagram_a, echo=FALSE, results='hide', warning=FALSE------
library(diagram, quietly=TRUE)
# reset figure margins
par(mar = c(1, 1, 1, 1))

# simple diagram of the pedon data structure
names <- c("Site", "Siteobs", "Pedon", "Horizon")
M <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
M[4, 3] <- M[3, 2] <- M[2, 1] <- ""
pos <- cbind (c(1, 1, 1, 1))
plotmat(M, pos = pos, name = names, lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.4, mx=-0.2)

# parallel simplified SPC structure
names <- c("Site-level", "Horizon-level")
M <- matrix(nrow = 2, ncol = 2, byrow = TRUE, data = 0)
 M[2, 1] <- ""
#pos <- cbind (c(2, 2))
plotmat(M, pos = c(1, 1), name = names, lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.14, box.type = "square", box.prop = 0.75, mx=0.3, my=-0.1, add=TRUE)

# add arrows to the diagram
arrows(0.42, 0.1, x1=0.65, y1=0.1, length = 0.25, code=2, lwd=2, angle = 15)
arrows(0.42, 0.35, x1=0.65, y1=0.54, length = 0.25, code=2, lwd=2, angle = 15)
arrows(0.42, 0.61, x1=0.65, y1=0.61, length = 0.25, code=2, lwd=2, angle = 15)
arrows(0.42, 0.87, x1=0.65, y1=0.68, length = 0.25, code=2, lwd=2, angle = 15)

## ----example_a, echo=FALSE, results='show', warning=FALSE----------------
top <- c(0,38,56,121,135)
bot <- c(30,56,121,135,'')
hzname <- c('A', 'Bt1', 'Bt2', 'Bk', 'R')
d <- data.frame(hzname, top, bot)
d

## ---- eval=FALSE---------------------------------------------------------
## # not run
## library(soilDB)
## help(soilDB)
## 
## # for links to lots of great examples look here!
## library(aqp)
## help(aqp)

## ----gopheridge_a--------------------------------------------------------
options(width=95, stringsAsFactors=FALSE)
library(soilDB)
library(aqp)

# load example dataset
data(gopheridge)

# what kind of object is this?
class(gopheridge)

# what does the internal structure look like?
str(gopheridge, 2)

# let's take a look at the fields at the site and horizon levels within the SPC
siteNames(gopheridge)
horizonNames(gopheridge)

## ---- fig.width=10, fig.height=4-----------------------------------------
par(mar=c(1,1,1,1))
# ommiting pedon IDs and horizon designations
plot(gopheridge, print.id=FALSE, name='')
title('Pedons from the `gopheridge` sample dataset', line=-0.5)

## ----gopheridge_a1, eval=TRUE, echo=FALSE, results='show', warning=FALSE, collapse=TRUE----
s <- site(gopheridge)
# show table of site data
knitr::kable(s[1:2, 1:10])
knitr::kable(s[1:2, 11:20])
knitr::kable(s[1:2, 21:28])
knitr::kable(s[1:2, 28:36])

# use the following to show the data in the R console
#head(site(gopheridge), 2) # show the first 2 lines of the site data

## ----gopheridge_a2, eval=TRUE, echo=FALSE, results='show', warning=FALSE, collapse=TRUE----
h <- horizons(gopheridge)
# show table of site data
knitr::kable(h[1:8, 1:10])
knitr::kable(h[1:8, 11:19])
#knitr::kable(h[1:2, 21:28])
#knitr::kable(h[1:2, 28:36])

# use the following to show the data in the R console
#head(horizons(gopheridge), 5) # show the first 5 rows of the horizon data

## ----owndata_a, results='hide'-------------------------------------------
# load required libraries
library(soilDB)
library(aqp)

# load data from a NASIS selected set
f <- fetchNASIS(from = 'pedons')

# what kind of object is this?
class(f)

# how many pedons
length(f)

# let's take a look at the fields at the site and horizon levels within the SPC
siteNames(f)
horizonNames(f)

# look at the first 2 rows of site and horizon data
head(site(f), 2)
head(horizons(f), 2)

## ----gopheridge_b--------------------------------------------------------
# plot the locations of the gopheridge pedons within R
# Steps:
# 1) subset to a new data frame
# 2) create a spatial points data frame (SPDF)
# 3) plot the data

# load libraries
library(sp)
library(maps)

# subset standard WGS84 decimal degree coordinates from the gopheridge SPC by specifying column names
gopher.locations <- site(gopheridge)[, c('site_id', 'x_std', 'y_std')]

# initialize coordinates in an SPDF
coordinates(gopher.locations) <- ~ x_std + y_std
# define coordinate system
proj4string(gopher.locations) <- '+proj=longlat +datum=WGS84'

# set plot margins
par(mar=c(0,0,0,0))

# plot county boundaries for all of CA
map('county', 'california')
# add pedon data locations, note symbol styling
points(gopher.locations, cex=0.5, pch=3, col='red')
box()

# plot again but zoom in by setting xlim, ylim extents
map('county', 'California', xlim=c(-122.25, -119.75), ylim=c(37, 38.5))
# add pedon data locations, note symbol styling
points(gopher.locations, cex=1, pch=3, col='red')
box()

## ----r_plot_pedons, eval=FALSE, echo=TRUE, results='show', warning=FALSE----
## # load libraries
## library(soilDB)
## library(sp)
## library(maps)
## 
## # get pedons from the selected set
## f <- fetchNASIS(from = 'pedons')
## 
## # subset standard WGS84 decimal degree coordinates from the gopheridge SPC by specifying column names
## f.locations <- site(f)[, c('site_id', 'x_std', 'y_std')]
## nrow(f.locations)
## 
## # remove any sites lacking standard lat/long coordinates
## # notice that there may now be fewer rows of data
## f.locations <- na.omit(f.locations)
## nrow(f.locations)
## 
## # initialize coordinates in an SPDF
## coordinates(f.locations) <- ~ x_std + y_std
## # define coordinate system
## proj4string(f.locations) <- '+proj=longlat +datum=WGS84'
## 
## # set plot margins
## par(mar=c(0,0,0,0))
## 
## # plot pedon locations
## plot(f.locations)
## 
## # plot in CONUS: good way to check for typos
## map('state')
## points(f.locations, cex=0.5, pch=3, col='red')
## 
## # plot again this time with county boundaries for your state
## # ENTER your state!!!
## map('county', 'Montana')
## # add plot of pedon locations
## points(f.locations, cex=0.5, pch=3, col='red')

## ----owndata_b, results='hide'-------------------------------------------
# summarize which soil taxa we have loaded
table(f$taxonname)
# sort results in descending order
sort(table(f$taxonname), decreasing=TRUE)

# could do the same thing for taxonomic subgroups or any column of the SPC at the site or horizon levels
table(f$taxsubgrp)
sort(table(f$taxsubgrp), decreasing=TRUE)

# table() is also useful when testing for null data using IS NA, is.na() or IS NOT NA, !is.na()
table(is.na(f$taxsubgrp))
table(!is.na(f$taxsubgrp))

# it can also be applied to horizon level columns in the SPC
sort(table(f$texture), decreasing=TRUE)

## ----owndata_d, results='hide'-------------------------------------------
# say we wanted to look at what the variation of particle size classes are within a specific subgroup?
# use of grep() to filter and create an index, then apply that index to the SPC 
# and create a new SPC called 'f1' using the square bracket notation
idx <- grep('lithic', f$taxsubgrp, invert=FALSE)
# save this subset of 'lithic' soils for later use  
f1 <- f[idx, ]
# or use the index directly to summarize a field
sort(table(f$taxpartsize[idx]), decreasing=TRUE)

## ----owndata_e, results='show', fig.width=8, fig.height=4----------------
# adjust margins
par(mar=c(1,0,0,1))
# plot the first 10 profiles of the 'f1' subset
# limit plotting to a depth of about 60cm
plot(f1[1:10, ], label='site_id', max.depth=60)
title('Pedons with the word "lithic" at subgroup-level of Soil Taxonomy', line=-2)

## ----owndata_f, results='show', fig.width=8, fig.height=4----------------
# say we wanted to look at what the variation of particle size classes are within a specific subgroup?
# first: use grep to pattern match the tax_subgroup field for the string 'aqu'
idx <- grep('aqu', f$taxsubgrp)
# save this subset
f1 <- f[idx, ]
# check taxonomic range of particle size classes in the data
sort(table(f1$taxsubgrp), decreasing=TRUE)
sort(table(f1$taxpartsize), decreasing=TRUE)

# then further query the subset for only those profiles with particle size class of 'sandy-skeletal'
# notice: a double equal sign '==' is used for exact character or numeric criteria
idx <- which(f1$taxpartsize == 'sandy-skeletal')
# save this subset
f2 <- f1[idx, ]
table(f2$taxpartsize)
# plot  profiles 1 thru 10
par(mar=c(0,0,2,1))
plot(f2[1:10, ], label='site_id')
title('Sandy-skeletal particle size control section class')

## ----owndata_g, results='show'-------------------------------------------
# extract site data from SPC into dataframe 's'
s <- site(f)
names(s)
# extract horizon data from SPC into dataframe 'h'
h <- horizons(f)
names(h)

## ----owndata_a1, eval=FALSE, results='hide'------------------------------
## # use each one of these to return a vector of the pedons where errors were detected
## #get('sites.missing.pedons', envir=soilDB.env)
## #get('dup.pedon.ids', envir=soilDB.env)
## #get('bad.pedon.ids', envir=soilDB.env)
## # example of pedon_id's returned
## #[1] "2011MT0810001" "2011MT0810009" "2011MT0810015" "2011MT0810027" "2011MT0810034"
## 
## #get('bad.horizons', envir=soilDB.env)
## 
## # How could you then remove these from your SPC?
## # since the get() returns the string of bad pedon id's we can use a which() to query any pedon id's that don't match the bad id's
## idx <- which(horizons(f)$pedon_id != get('bad.pedon.ids', envir=soilDB.env))
## f <- f[idx, ]

## ------------------------------------------------------------------------
# make a new object with a sequence of values from 1 to 10
a <- seq(from=1, to=10, by=1)
# result
#[1]  1  2  3  4  5  6  7  8  9 10

# define a function that performs a simple action on a vector of numbers
# "i" is a temporary object created inside of the context of this function based on the argument supplied
aFunction <- function(i) {
  # do something
  res <- i * 10
  # send the results back to the calling context
  return(res)
}

# apply our new function to object "a"
aFunction(a)

## ----owndata_i_gopher, echo=TRUE, results='show', warning=FALSE----------
# load required libraries
library(aqp)
library(soilDB)
library(plyr)

# load example dataset
data(gopheridge)

# rename gopheridge as SPC object 'f'
f <- gopheridge

# the argument 'i' is a single soil profile
findBtHorizons <- function(i) {
  # extract horizons for current profile
  h <- horizons(i) 
  # search for pattern 't' in horizon designations
  idx <- grep('t', h$hzname)
  # subset these horizons
  h2 <- h[idx, ] 
  # subset columns in resulting dataframe
  res <- h2[, c('peiid', 'phiid', 'hzname', 'hzdept', 'hzdepb', 'clay', 'phfield')]
  # return data
  return(res)
}

# apply function to a single profile as a demonstration
findBtHorizons(f[1, ])

## ------------------------------------------------------------------------
l <- profileApply(f, FUN=findBtHorizons, simplify=FALSE)

# convert list into a dataframe, dropping all pedons with no 't' horizons 
Bt.horizons <- ldply(l)

## ----fig.width=4.5, fig.height=3.5---------------------------------------
# standard ddply syntax is as follows (type '?ddply' into the R console):
# ddply(.data, .variables, .fun)
Bt.horizons.top <- ddply(Bt.horizons, 'peiid', summarise, depth_to_Bt_cm=min(hzdept))

# since we have peiid in the 'Bt.horizons.top' dataframe we can easy join it back to site data in the SPC
# NOTE: when used in conjunction with site(), the assignment operator will perform a left-join
site(f) <- Bt.horizons.top

# summary of depth to argillic in the data using a histogram
# reset figure margins
par(mar=c(4.5,4.5,1,1))
hist(f$depth_to_Bt_cm, xlab='Depth to Bt Horizon (cm)', main='')

## ----fig.width=10, fig.height=5------------------------------------------
# index to the first 15 profiles
idx <- 1:15

# reset figure margins
par(mar=c(0,0.5,3,1))

# plot indexed profiles, omitting IDs
plot(f[idx, ], print.id=FALSE)

# add the top depth of the first "Bt" horizon
points(x=1:15, y=f$depth_to_Bt_cm[idx], pch=21, bg='black', col='white')

# title / subtitle
title(main = "Select pedons from the 'gopheridge' sample dataset", line=-0.5)
title(sub= "Depth to first 'Bt' horizon identified", line=-2)

## ----owndata_i1_gopher, eval=FALSE, echo=TRUE, results='show', warning=FALSE----
## # This time we'll go after the thickness of the organic horizons where present.
## 
## # load library
## library(plyr)
## 
## f.organic <- function(i) {
##   # extract horizons
##   h <- horizons(i)
##   # pattern match for 'O' horizon designations in horizon data
##   idx <- grep('O', h$hzname)
##   h2 <- h[idx, ]
##   # subset results
##   res <- h2[, c('peiid', 'phiid', 'hzname', 'hzdept', 'hzdepb')]
##   # return data
##   return(res)
## }
## 
## # apply function to each profile, results are a list of data.frames
## l <- profileApply(f, FUN=f.organic, simplify=FALSE)
## 
## # convert list into a dataframe
## organic <- ldply(l)
## 
## # show contents of the 'organic' dataframe
## head(organic)
## 
## # summarize this dataframe down to one max bottom depth value for each profile
## organic1 <- ddply(organic, 'peiid', summarise, organic_thickness_cm=max(hzdepb))
## 
## # since we have peiid in the 'organic1' dataframe we can join back to site data in the SPC
## site(f) <- organic1
## 
## # summary of organic thickness in the data
## # reset figure margins
## par(mar=c(4.5,4.5,1,1))
## hist(f$organic_thickness_cm, xlab='Thickness of Organic horizons (cm)', main='')

## ----owndata_i, eval=FALSE, echo=TRUE, results='show', warning=FALSE-----
## # load required libraries
## library(plyr)
## 
## # the argument 'i' is a single soil profile
## f.limy <- function(i) {
##   # extract horizons for current profile
##   h <- horizons(i)
##   # search for pattern 'k' in horizon designations
##   idx <- grep('k', h$hzname)
##   # subset these horizons
##   h2 <- h[idx, ]
##   # subset columns in resulting dataframe
##   res <- h2[, c('peiid', 'phiid', 'hzname', 'hzdept', 'hzdepb', 'phfield', 'effclass')]
##   # return data
##   return(res)
## }
## 
## # apply function to each profile, results are a list of dataframes
## l <- profileApply(f, FUN=f.limy, simplify=FALSE)
## 
## # convert list into a dataframe, dropping all pedons with no 'k' horizons
## limy <- ldply(l)
## 
## # view the top 6 rows
## head(limy)
## 
## # still need to reduce this down to one depth value for each profile
## ## ddply() will apply a function (summarise the min(hzdept)) then combine the results into a data frame.
## ## standard ddply syntax is as follows (type '??ddply' into the R console):
## ## ddply(.data, .variables, .fun = NULL....)
## 
## limy1 <- ddply(limy, 'peiid', summarise, depth_to_carbonates_cm=min(hzdept))
## 
## # since we have peiid in the 'limy1' dataframe we can easy join it back to site data in the SPC
## # this won't work if there were no horizons with 'k' suffice
## site(f) <- limy1
## 
## # summary of depth to carbonates in the data using a histogram
## # reset figure margins
## par(mar=c(4.5,4.5,1,1))
## hist(f$depth_to_carbonates_cm, xlab='Depth to Calcium Carbonates (cm)', main='')

## ----owndata_j1, eval=TRUE, echo=TRUE, results='show', warning=FALSE, collapse=TRUE----
# fetch extended site and horizon data
e <- get_extended_data_from_NASIS_db()

### site and pedon related extended data
# vegetation data summary
colnames(e$ecositehistory) 

# diagnostic features
colnames(e$diagnostic) 

# surface rock fragments
colnames(e$surf_frag_summary)

# geomorphic description
colnames(e$geomorph)

# taxonomic history data
colnames(e$taxhistory)

# linked photo stored in site textnotes
colnames(e$photo) 

# site parent materials
colnames(e$pm)

### horizon related extended data
# rock fragments 
colnames(e$frag_summary) 

# soil texture modifers
colnames(e$texmodifier)

# soil structure data
colnames(e$struct) 

## ----owndata_c, results='show'-------------------------------------------
# graphically tabulate the occurrence of landforms
# load required libraries
library(soilDB)
# required for dotchart2()
library(Hmisc)

# load data from a NASIS selected set
f <- fetchNASIS(from = 'pedons')

# create 'lf' object of landform factors sorted in descending order
lf <- sort(table(f$landform_string), decreasing = TRUE)

# plot top 10
dotchart2(lf[1:10], col='black', xlim = c(0, max(lf)), cex.labels = 0.75)

## ----owndata_j3, results='show', warning=FALSE, fig.width=6, fig.height=4----
# rename gopheridge data
f <- gopheridge

# get diagnostic features associated with pedons loaded from selected set
d <- diagnostic_hz(f)

# summary of the diagnostic features in your data!
unique(d$featkind)
sort(table(droplevels(factor(d$featkind))), decreasing = TRUE)

# subset argillic horizons
d <- d[which(d$featkind == 'argillic horizon'), ]

# create a new column and subtract the upper from the lower depth
d$argillic_thickness_cm <- d$featdepb - d$featdept

# create another new column with the upper depth to the diagnostic feature
d$depth_to_argillic_cm <- d$featdept

# omit NA values
d <- na.omit(d)

# subset to pedon records IDs and calculated thickness
d <- d[, c('peiid', 'argillic_thickness_cm', 'depth_to_argillic_cm')]
head(d)

# join these data with existing site data
site(f) <- d

# plot as histogram
# reset figure margins
par(mar=c(4.5,4.5,1,1))
hist(f$argillic_thickness_cm, xlab='Thickness of argillic diagnostic (cm)', main='')
hist(f$depth_to_argillic_cm, xlab='Depth to argillic diagnostic (cm)', main='')

## ----owndata_j4, eval=TRUE, echo=TRUE, results='show', warning=FALSE-----
# start fresh with your own data
f <- fetchNASIS(from = 'pedons')
# get diagnostic features associated with pedons loaded from selected set
d <- diagnostic_hz(f)
# summary of the diagnostic features in your data!
unique(d$featkind)
# top 5 most frequent
sort(table(d$featkind), decreasing = TRUE)[1:5]

# subset argillic horizons - or choose your own diagnostic feature and modify this script!
#idx <- which(d$diag_kind == 'your_diagnostic')
#d <- d[idx, ]

# how would you do the rest.....see if you can work it out!


## ----owndata_k, eval=TRUE, echo=TRUE, results='show', warning=FALSE, fig.height=9, fig.width=7----
## work up diagnostic plot based on gopheridge dataset
library(aqp)
library(soilDB)
library(sharpshootR)

# load data
data(gopheridge)

# can limit which diagnostic features to show by setting 'v' manually
v <- c('ochric.epipedon', 'cambic.horizon', 'argillic.horizon', 'paralithic.contact', 'lithic.contact')

# generate diagnostic property diagram
diagnosticPropertyPlot(gopheridge, v, k=5, grid.label='site_id', dend.label = 'taxonname', sort.vars = FALSE)

# plot again, this time with diagnostic features ordered according to co-occurrence
diagnosticPropertyPlot(gopheridge, v, k=5, grid.label='site_id', dend.label = 'taxonname', sort.vars = TRUE)

## ----owndata_l, eval=FALSE, echo=TRUE, results='hide', warning=FALSE, fig.height=9, fig.width=7----
## library(soilDB)
## library(sharpshootR)
## 
## # load data
## f <- fetchNASIS(from = 'pedons')
## 
## # may need to subset to a particular series or taxa here....to reduce the number of pedons!
## 
## # select a series of diagnostic properties or automatically pull diagnostic feature columns
## # get all diagnostic feature columns from site data by pattern matching on '[.]' in the colnames
## idx <- grep('[.]', colnames(site(f)))
## v <- colnames(site(f))[idx]
## v
## 
## # or insert diagnostics of interest found in your data here from the list of possible diagnostics in 'v'
## v <- c('ochric.epipedon', 'cambic.horizon', 'argillic.horizon', 'paralithic.contact', 'lithic.contact')
## 
## # generate diagnostic property diagram
## diagnosticPropertyPlot(f, v, k=5, grid.label='site_id', dend.label = 'taxonname')

## ---- fig.width=7, fig.height=4------------------------------------------
library(RODBC)

# write query as a long text object
q <- "
-- columns to return
SELECT siteiid as siteiid, peiid, usiteid as site_id, upedonid as pedon_id, obsdate as obs_date,
soitemp, soitempdep

FROM
-- tables that are queried and join conditions
site_View_1 
INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
LEFT OUTER JOIN sitesoiltemp_View_1 ON siteobs_View_1.siteobsiid = sitesoiltemp_View_1.siteobsiidref
LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
-- ordering of rows
order by obs_date, siteiid;"

# setup connection local NASIS
channel <- odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))

# exec query
d <- sqlQuery(channel, q, stringsAsFactors=FALSE)

# close connection
odbcClose(channel)

# check results
str(d)

# remove records missing values
d <- na.omit(d)

# tabulate unique soil depths
table(d$soitempdep)

# extract doy of year
d$doy <- as.integer(format(d$obs_date, "%j"))

# when where measurements collected?
hist(d$doy, xlim=c(1,366), breaks=30, las=1, main='Soil Temperature Measurements', xlab='Day of Year')

# soil temperature by day of year
plot(soitemp ~ doy, data=d, type='p', xlim=c(1, 366), ylim=c(-1, 25), xlab='Day of Year', ylab='Soil Temperature at 50cm (deg C)', las=1)

