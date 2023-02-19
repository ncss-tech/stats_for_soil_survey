options(width=95, stringsAsFactors=FALSE)
library(aqp)
library(soilDB)

# load example dataset
data(gopheridge)

# what kind of object is this?
class(gopheridge)

# what does the internal structure look like?
str(gopheridge, 2)

# let's take a look at the fields at the site and horizon levels within the SPC
siteNames(gopheridge)
horizonNames(gopheridge)

par(mar=c(1,1,1,1))
# ommiting pedon IDs and horizon designations
plot(gopheridge, print.id=FALSE, name='', width=0.3)
title('Pedons from the `gopheridge` sample dataset', line=-0.5)

## # load required libraries
## library(aqp)
## library(soilDB)
## 
## # load data from a NASIS selected set
## pedons <- fetchNASIS(from = 'pedons')
## 
## # what kind of object is this?
## class(pedons)
## 
## # how many pedons
## length(pedons)
## 
## # let's take a look at the fields at the site and horizon levels within the SPC
## siteNames(pedons)
## horizonNames(pedons)
## 
## # look at the first 2 rows of site and horizon data
## head(site(pedons), 2)
## head(horizons(pedons), 2)

# plot the locations of the gopheridge pedons within R
# Steps:
# 1) subset to a new data frame
# 2) create a spatial points data frame (SPDF)
# 3) plot the data

# load libraries
library(sp)
library(mapview)

data("gopheridge")

# subset standard WGS84 decimal degree coordinates from the gopheridge SPC by specifying column names
gopher.locations <- site(gopheridge)

# initialize coordinates in an SPDF
coordinates(gopher.locations) <- ~ x_std + y_std
# define coordinate system
proj4string(gopher.locations) <- '+proj=longlat +datum=WGS84'

# creat interactive map
mapview(gopher.locations, legend=FALSE, map.types='OpenStreetMap', label=gopher.locations$site_id)

## # load libraries
## library(aqp)
## library(soilDB)
## library(sp)
## library(mapview)
## 
## # get pedons from the selected set
## pedons <- fetchNASIS(from = 'pedons')
## 
## # subset standard WGS84 decimal degree coordinates from the
## # gopheridge SPC by specifying column names
## pedons.sp <- site(pedons)[, c('site_id', 'x_std', 'y_std')]
## nrow(pedons.sp)
## 
## # remove any sites lacking standard lat/long coordinates
## # notice that there may now be fewer rows of data
## pedons.sp <- na.omit(pedons.sp)
## nrow(pedons.sp)
## 
## # initialize coordinates in an SPDF
## coordinates(pedons.sp) <- ~ x_std + y_std
## # define coordinate system
## proj4string(pedons.sp) <- '+proj=longlat +datum=WGS84'
## 
## # plot
## mapview(pedons.sp, legend=FALSE, map.types='OpenStreetMap', label=pedons.sp$site_id)

## # summarize which soil taxa we have loaded
## table(pedons$taxonname)
## # sort results in descending order
## sort(table(pedons$taxonname), decreasing=TRUE)
## 
## # could do the same thing for taxonomic subgroups or any column of the SPC at the site or horizon levels
## table(pedons$taxsubgrp)
## sort(table(pedons$taxsubgrp), decreasing=TRUE)
## 
## # table() is also useful when testing for null data using IS NA, is.na() or IS NOT NA, !is.na()
## table(is.na(pedons$taxsubgrp))
## table(!is.na(pedons$taxsubgrp))
## 
## # it can also be applied to horizon level columns in the SPC
## sort(table(pedons$texture), decreasing=TRUE)

## # say we wanted to look at what the variation of particle size classes are within a specific subgroup?
## # use of grep() to filter and create an index, then apply that index to the SPC
## # and create a new SPC called 'f1' using the square bracket notation
## idx <- grep('lithic', pedons$taxsubgrp, invert=FALSE)
## # save this subset of 'lithic' soils for later use
## subset1 <- pedons[idx, ]
## # or use the index directly to summarize a field
## sort(table(pedons$taxpartsize[idx]), decreasing=TRUE)

## # adjust margins
## par(mar=c(1,0,0,1))
## # plot the first 10 profiles of subset1
## # limit plotting to a depth of about 60cm
## plot(subset1[1:10, ], label='site_id', max.depth=60)
## title('Pedons with the word "lithic" at subgroup-level of Soil Taxonomy', line=-2)

## # say we wanted to look at what the variation of particle size classes are within a specific subgroup?
## # first: use grep to pattern match the tax_subgroup field for the string 'aqu'
## idx <- grep('lithic', pedons$taxsubgrp)
## # save this subset
## subset1 <- pedons[idx, ]
## # check taxonomic range of particle size classes in the data
## sort(table(subset1$taxsubgrp), decreasing=TRUE)
## sort(table(subset1$taxpartsize), decreasing=TRUE)
## 
## # then further query the subset for only those profiles with particle size class of 'sandy-skeletal'
## # notice: a double equal sign '==' is used for exact character or numeric criteria
## idx <- which(subset1$taxpartsize == 'loamy')
## # save this subset
## subset2 <- subset1[idx, ]
## table(subset2$taxpartsize)
## # plot  profiles 1 thru 10
## par(mar=c(0,0,2,1))
## plot(subset2, label='site_id')
## title('Loamy particle size control section class')

## # extract site data from SPC into dataframe 's'
## s <- site(pedons)
## names(s)
## # extract horizon data from SPC into dataframe 'h'
## h <- horizons(pedons)
## names(h)

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
## idx <- which(! pedons$pedon_id %in% get('bad.pedon.ids', envir=soilDB.env))
## pedons <- pedons[idx, ]

## # fetch extended site and horizon data
## e <- get_extended_data_from_NASIS_db()
## 
## ### site and pedon related extended data
## # vegetation data summary
## colnames(e$ecositehistory)
## 
## # diagnostic features
## colnames(e$diagnostic)
## 
## # surface rock fragments
## colnames(e$surf_frag_summary)
## 
## # geomorphic description
## colnames(e$geomorph)
## 
## # taxonomic history data
## colnames(e$taxhistory)
## 
## # linked photo stored in site textnotes
## colnames(e$photo)
## 
## # site parent materials
## colnames(e$pm)
## 
## ### horizon related extended data
# # rock fragments
# colnames(e$frag_summary)
## 
## # soil texture modifers
## colnames(e$texmodifier)
## 
## # soil structure data
## colnames(e$struct)

## # graphically tabulate the occurrence of landforms
## # load required libraries
## library(soilDB)
## # required for dotchart2()
## library(Hmisc)
## 
## # load data from a NASIS selected set
## pedons <- fetchNASIS(from = 'pedons')
## 
## # create 'lf' object of landform factors sorted in descending order
## lf <- sort(table(pedons$landform_string), decreasing = TRUE)
## 
## # plot top 10 or length, whichever is shorter
## dotchart2(lf[1:pmin(10, length(lf))], col='black', xlim = c(0, max(lf)), cex.labels = 0.75)

# rename gopheridge data
data("gopheridge")
f <- gopheridge

# get diagnostic features associated with pedons loaded from selected set
d <- diagnostic_hz(f)

# summary of the diagnostic features in your data!
unique(d$featkind)
sort(table(droplevels(factor(d$featkind))), decreasing = TRUE)

# subset argillic horizons
d <- d[d$featkind == 'argillic horizon', ]

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
par(mar = c(4.5,4.5,1,1))
hist(f$argillic_thickness_cm, xlab = 'Thickness of argillic diagnostic (cm)', main='')
hist(f$depth_to_argillic_cm, xlab = 'Depth to argillic diagnostic (cm)', main = '')

## # start fresh with your own data
## f <- fetchNASIS(from = 'pedons')
## # get diagnostic features associated with pedons loaded from selected set
## d <- diagnostic_hz(f)
## # summary of the diagnostic features in your data!
## unique(d$featkind)
## # top 5 most frequent
## sort(table(d$featkind), decreasing = TRUE)[1:5]
## 
## # subset argillic horizons - or choose your own diagnostic feature and modify this script!
## #idx <- which(d$diag_kind == 'your_diagnostic')
## #d <- d[idx, ]
## 
## # how would you do the rest.....see if you can work it out!
## 

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

## library(soilDB)
## library(RODBC)
## 
## # write query as a long text object
## q <- "
## -- columns to return
## SELECT siteiid as siteiid, peiid, usiteid as site_id, upedonid as pedon_id, obsdate as obs_date,
## soitemp, soitempdep
## 
## FROM
## -- tables that are queried and join conditions
## site_View_1
## INNER JOIN siteobs_View_1 ON site_View_1.siteiid = siteobs_View_1.siteiidref
## LEFT OUTER JOIN sitesoiltemp_View_1 ON siteobs_View_1.siteobsiid = sitesoiltemp_View_1.siteobsiidref
## LEFT OUTER JOIN pedon_View_1 ON siteobs_View_1.siteobsiid = pedon_View_1.siteobsiidref
## -- ordering of rows
## ORDER BY obs_date, siteiid;"
## 
## # setup connection local NASIS
## channel <- odbcDriverConnect(connection = getOption("soilDB.NASIS.credentials"))
## 
## # exec query
## d <- sqlQuery(channel, q, stringsAsFactors=FALSE)
## 
## # close connection
## odbcClose(channel)
## 
## # check results
## str(d)
## 
## # remove records missing values
## d <- na.omit(d)
## 
## # tabulate unique soil depths
## table(d$soitempdep)
## 
## # extract doy of year
## d$doy <- as.integer(format(d$obs_date, "%j"))
## 
## # when where measurements collected?
## hist(d$doy, xlim=c(1,366), breaks=30, las=1, main='Soil Temperature Measurements', xlab='Day of Year')
## 
## # soil temperature by day of year
## plot(soitemp ~ doy, data=d, type='p', xlim=c(1, 366), ylim=c(-1, 25), xlab='Day of Year', ylab='Soil Temperature at 50cm (deg C)', las=1)
