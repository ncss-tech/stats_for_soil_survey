## ----setup, echo=FALSE, results='hide', warning=FALSE, message=FALSE-----
# setup
library(knitr, quietly=TRUE)
opts_chunk$set(message=FALSE, warning=FALSE, background='#F7F7F7', fig.align='center', fig.retina=2, dev='png', tidy=FALSE, verbose=FALSE, antialias='cleartype', cache=FALSE)

# options for R functions
options(width=100, stringsAsFactors=FALSE)

# captions added to figures
knit_hooks$set(htmlcap = function(before, options, envir) {
  if(!before) {
    paste('<p class="caption" style="font-size:85%; font-style: italic; font-weight: bold;">',options$htmlcap,"</p><hr>",sep="")
    }
    })

## ------------------------------------------------------------------------
library(sp)
library(raster)
library(rgdal)
library(soilDB)

## ------------------------------------------------------------------------
# you may have to install this
library(mapview)

# get series extents from SoilWeb
pentz <- seriesExtent('pentz')
redding <- seriesExtent('redding')

# make a simple map
m <- mapview(pentz)
# add more data to the map and display
addFeatures(m, redding, color='black', fillColor='red', weight=1)

## ----fig.width=6, fig.height=5, results='hide'---------------------------
library(soilDB)
library(raster)
library(lattice)

# you may have to install this package
library(velox)

# 5-10 seconds for download of SEE data, 
s <- seriesExtent('cecil')

# load pointer to PRISM data, note that this file is stored on my local machine:
r <- raster('E:/gis_data/prism/ffd_50_pct_800m.tif')

# 2.5 seconds sampling
vx <- velox(r)
system.time(e <- vx$extract(s))

# simple summary
densityplot(e$CECIL, plot.points=FALSE, bw=2, lwd=2, col='RoyalBlue', xlab='Frost-Free Days (50% chance)\n800m PRISM Data (1981-2010)', ylab='Density', main='FFD Estimate for Extent of Cecil Series')

## ------------------------------------------------------------------------
# create point geometry from coordinates
p <- SpatialPoints(coords = cbind(-97.721210, 40.446068))
# attach attribute table
p <- SpatialPointsDataFrame(p, data=data.frame(id=1, taxonname='alpha'))
# check internal structure
str(p)

## ------------------------------------------------------------------------
# make some fake data
d <- data.frame(x=runif(10), y=runif(10), id=1:10, code=letters[1:10])
# upgrade to SpatialPointsDataFrame
coordinates(d) <- ~ x + y
# check
class(d)

## ------------------------------------------------------------------------
# [rows, columns]
# [features, attributes]
d[1:5, ]

## ------------------------------------------------------------------------
d$id

## ------------------------------------------------------------------------
# new data must be of the same length as number of features, otherwise recycling will occur
d$rand <- rnorm(n=nrow(d))

## ------------------------------------------------------------------------
idx <- which(d$rand > 0)
d[idx, ]

## ------------------------------------------------------------------------
as(d, 'data.frame')

## ------------------------------------------------------------------------
proj4string(p) <- '+proj=longlat +datum=WGS84' 
str(p)

## ------------------------------------------------------------------------
# transform to UTM zone 14
p.utm <- spTransform(p, CRS('+proj=utm +zone=14 +datum=NAD83'))
cbind(gcs=coordinates(p), utm=coordinates(p.utm))

# transform to GCS NAD27
p.nad27 <- spTransform(p, CRS('+proj=longlat +datum=NAD27'))
cbind(coordinates(p), coordinates(p.nad27))

## ------------------------------------------------------------------------
CRS('+init=epsg:4326')

## ----eval=FALSE----------------------------------------------------------
## x <- readOGR(dsn='E:/gis_data/ca630/FG_CA630_OFFICIAL.gdb', layer='ca630_a')

## ----eval=FALSE----------------------------------------------------------
## x <- readOGR(dsn='E:/gis_data/ca630', layer='pedon_locations')

## ----eval=FALSE----------------------------------------------------------
## writeOGR(x, dsn='E:/gis_data/ca630', layer='pedon_locations', driver = 'ESRI Shapefile')

## ----fig.width=6, fig.height=6-------------------------------------------
# use an example from the raster package
f <- system.file("external/test.grd", package="raster")
# create a reference to this raster
r <- raster(f)
# print the details
print(r)
# default plot method
plot(r)

## ------------------------------------------------------------------------
# check: file is on disk
inMemory(r)
# load into memory, if possible
r <- readAll(r)
# check: file is in memory
inMemory(r)

## ----eval=FALSE----------------------------------------------------------
## # using previous example data set
## writeRaster(r, filename='r.tif', options=c("COMPRESS=LZW"))

## ---- eval=FALSE---------------------------------------------------------
## # store path as a variable, in case you want to keep it somewhere else
## ch2b.data.path <- 'C:/workspace/chapter-2b'
## 
## # make a place to store chapter 2b example data
## dir.create(ch2b.data.path, recursive = TRUE)
## 
## # download example data from github
## # polygons
## download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/chapter_2b-spatial-data/chapter-2b-mu-polygons.zip', paste0(ch2b.data.path, '/chapter-2b-mu-polygons.zip'))
## 
## # raster data
## download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/chapter_2b-spatial-data/chapter-2b-PRISM.zip', paste0(ch2b.data.path, '/chapter-2b-PRISM.zip'))
## 
## # unzip
## unzip(paste0(ch2b.data.path, '/chapter-2b-mu-polygons.zip'), exdir = ch2b.data.path, overwrite = TRUE)
## unzip(paste0(ch2b.data.path, '/chapter-2b-PRISM.zip'), exdir = ch2b.data.path, overwrite = TRUE)

## ----results='hide'------------------------------------------------------
# establish path to example data
ch2b.data.path <- 'C:/workspace/chapter-2b'

# load MLRA polygons
mlra <- readOGR(dsn=ch2b.data.path, layer='mlra-18-15-AEA')

## ------------------------------------------------------------------------
# mean annual air temperature, Deg C
maat <- raster(paste0(ch2b.data.path, '/MAAT.tif'))
# mean annual precipitation, mm
map <- raster(paste0(ch2b.data.path, '/MAP.tif'))
# frost-free days
ffd <- raster(paste0(ch2b.data.path, '/FFD.tif'))
# growing degree days
gdd <- raster(paste0(ch2b.data.path, '/GDD.tif'))
# percent of annual PPT as rain
rain_fraction <- raster(paste0(ch2b.data.path, '/rain_fraction.tif'))
# annual sum of monthly PPT - ET_p
ppt_eff <- raster(paste0(ch2b.data.path, '/effective_preciptitation.tif'))

## ------------------------------------------------------------------------
rs <- stack(maat, map, ffd, gdd, rain_fraction, ppt_eff)
# reset layer names
names(rs) <- c('MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT')

## ------------------------------------------------------------------------
# object class
class(mlra)
class(maat)
class(rs)

# the raster package provides a nice "print" method for raster and sp classes
print(maat)

# coordinate reference systems: note that they are not all the same
proj4string(mlra)
proj4string(maat)
proj4string(rs)

## ---- fig.width=5, fig.height=6.75---------------------------------------
# MLRA polygons in native coordinate system
# recall that mlra is a SpatialPolygonsDataFrame
plot(mlra, main='MLRA 15 and 18')
box()

# MAAT raster
# recall that maat is a raster object
plot(maat, main='PRISM Mean Annual Air Temperature (deg C)')


# plot MAAT raster with MLRA polygons on top
# this requires transforming to CRS of MAAT
mlra.gcs <- spTransform(mlra, CRS(proj4string(maat)))
plot(maat, main='PRISM Mean Annual Air Temperature (deg C)')
plot(mlra.gcs, main='MLRA 15 and 18', add=TRUE)

## ------------------------------------------------------------------------
# hand make a SpatialPoints object
# note that this is GCS
p <- SpatialPoints(coords = cbind(-120, 37.5), proj4string = CRS('+proj=longlat +datum=WGS84'))

# spatial extraction of MLRA data requires a CRS transformation
p.aea <- spTransform(p, proj4string(mlra))
over(p.aea, mlra)

## ------------------------------------------------------------------------
# extract from a single RasterLayer
extract(maat, p)

# extract from a RasterStack
extract(rs, p)

## ------------------------------------------------------------------------
# extract using a buffer with radius specified in meters (1000m)
extract(rs, p, buffer=1000)

## ---- eval=FALSE---------------------------------------------------------
## # sampling single RasterLayer
## sampleRegular(maat, size=10)
## 
## # sampling RasterStack
## sampleRegular(rs, size=10)

## ----fig.width=8, fig.height=5-------------------------------------------
par(mfcol=c(1,2), mar=c(1,1,3,1))

# regular sampling + extraction of raster values
x.regular <- sampleRegular(maat, size = 100, sp=TRUE)
plot(maat, axes=FALSE, legend=FALSE, main='Regular Sampling')
points(x.regular)

# random sample + extraction of raster values
# note that NULL values are removed
x.random <- sampleRandom(maat, size = 100, sp=TRUE, na.rm=TRUE)
plot(maat, axes=FALSE, legend=FALSE, main='Random Sampling with NA Removal')
points(x.random)

## ------------------------------------------------------------------------
# this will be very slow for large grids
mean(values(maat), na.rm=TRUE)

# generally much faster and just as good, given reasonable sampling strategy and sample size
mean(x.regular$MAAT, na.rm=TRUE)
mean(x.random$MAAT, na.rm=TRUE)

## ------------------------------------------------------------------------
# result is a SoilProfileCollection object
auburn <- fetchKSSL(series = 'auburn')

# extract site data
s <- site(auburn)

# these are GCS WGS84 coordinates from NASIS
coordinates(s) <- ~ x + y
proj4string(s) <- '+proj=longlat +datum=WGS84'

## ------------------------------------------------------------------------
# return the result as a data.frame object
e <- extract(rs, s, df=TRUE)

# summarize out the extracted data
# note that we are "leaving out" the first column which contains an ID
summary(e[, -1])

## ------------------------------------------------------------------------
# combine site data with extracted raster values, row-order is identical
res <- cbind(as.data.frame(s), e)

# extract unique IDs and PRISM data
res <- res[, c('pedon_key', 'MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT')]

# join with original SoilProfileCollection object via pedon_key
site(auburn) <- res

## ----fig.width=12, fig.height=6.5----------------------------------------
# create an ordering of pedons based on the extracted effective PPT
new.order <- order(auburn$eff.PPT)

# setup figure margins
par(mar=c(5,2,4,2))
# plot profile sketches
plot(auburn, name='hzn_desgn', print.id=FALSE, color='clay', plot.order=new.order)

# add an axis with extracted raster values
axis(side=1, at = 1:length(auburn), labels = round(auburn$eff.PPT[new.order]), cex.axis=0.75)
mtext('Annual Sum of Monthly (PPT - ET_p) (mm)', side=1, line=2.5)

## ------------------------------------------------------------------------
amador <- seriesExtent(s = 'amador')
class(amador)

## ------------------------------------------------------------------------
s <- spsample(amador, n = 100, type = 'hexagonal')

## ----fig.width=4, fig.height=5-------------------------------------------
par(mar=c(1,1,3,1))
plot(maat, ext=extent(s), main='MAAT and Amador Extent\n100 Sampling Points', axes=FALSE)
plot(amador, add=TRUE)
points(s, cex=0.25)

## ------------------------------------------------------------------------
# return the result as a data.frame object
e <- extract(rs, s, df=TRUE)
# check out the extracted data
summary(e[, -1])

## ------------------------------------------------------------------------
table(mlra$MLRARSYM)

## ------------------------------------------------------------------------
poly.area <- round(sapply(mlra@polygons, slot, 'area') * 0.000247105)
summary(poly.area)
sum(poly.area)

## ---- warning=TRUE-------------------------------------------------------
library(sharpshootR)

# the next function requires a polygon ID: each polygon gets a unique number 1--number of polygons
mlra$pID <- 1:nrow(mlra)
s <- constantDensitySampling(mlra, n.pts.per.ac=0.001)

## ------------------------------------------------------------------------
# spatial overlay: sampling points and MLRA polygons
res <- over(s, mlra)

# row / feature order is preserved, so we can directly copy
s$mlra <- res$MLRARSYM

# tabulate number of samples per MLRA
table(s$mlra)

## ------------------------------------------------------------------------
# raster stack extraction at sampling points
e <- extract(rs, s, df=TRUE)

# convert sampling points from SpatialPointsDataFrame to data.frame
s.df <- as(s, 'data.frame')

# join columns from extracted values and sampling points
s.df <- cbind(s.df, e)

# check results
head(s.df)

## ------------------------------------------------------------------------
library(lattice)
library(reshape2)

# reshape from wide to long format
m <- melt(s.df, id.vars = c('mlra'), measure.vars = c('MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT'))

# check "wide" format
head(m)

## ------------------------------------------------------------------------
# tabular summary of mean values
tapply(m$value, list(m$mlra, m$variable), mean)

## ---- fig.width=8, fig.height=4------------------------------------------
tps <- list(box.rectangle=list(col='black'), box.umbrella=list(col='black', lty=1), box.dot=list(cex=0.75), plot.symbol=list(col=rgb(0.1, 0.1, 0.1, alpha = 0.25, maxColorValue = 1), cex=0.25))


bwplot(mlra ~ value | variable, data=m,                 # setup plot and data source
       as.table=TRUE,                                   # start panels in top/left corner
       varwidth=TRUE,                                   # scale width of box by number of obs
       scales=list(alternating=3, relation='free'),     # setup scales
       strip=strip.custom(bg=grey(0.9)),                # styling for strips
       par.settings=tps,                                # apply box/line/point styling
       panel=function(...) {                            # within in panel, do the following
          panel.grid(-1, -1)                            # make grid lines at all tick marks
          panel.bwplot(...)                             # make box-whisker plot
      }
)

