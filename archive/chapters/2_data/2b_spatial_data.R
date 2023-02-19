library(aqp)
library(sp)
library(raster)
library(rgdal)
library(soilDB)

# load required packages
library(mapview)
library(leafem)

# get series extents from SoilWeb
pentz <- seriesExtent('pentz')
redding <- seriesExtent('redding')

# make a simple map
m <- mapView(pentz)
# add more data to the map and display
addFeatures(m, redding, color='black', fillColor='red', weight=1)

library(soilDB)
library(raster)
library(lattice)

# you may have to install this package
library(velox)

# 5-10 seconds for download of SEE data, 
s <- seriesExtent('san joaquin')

# load pointer to PRISM data, note that this file is stored on my local machine:
r <- raster('C:/workspace/chapter-2b/FFD.tif')

# 0.8 seconds for sampling
vx <- velox(r)
system.time(e <- vx$extract(s))

# simple summary
densityplot(e$`SAN JOAQUIN`, plot.points=FALSE, bw=2, lwd=2, col='RoyalBlue', xlab='Frost-Free Days (50% chance)\n800m PRISM Data (1981-2010)', ylab='Density', main='FFD Estimate for Extent of San Joaquin Series')

# create point geometry from coordinates
p <- SpatialPoints(coords = cbind(-97.721210, 40.446068))
# attach attribute table
p <- SpatialPointsDataFrame(p, data=data.frame(id=1, taxonname='alpha'))
# check internal structure
str(p)

# make some fake data
d <- data.frame(x=runif(10), y=runif(10), id=1:10, code=letters[1:10])
# upgrade to SpatialPointsDataFrame
coordinates(d) <- ~ x + y
# check
class(d)

# [rows, columns]
# [features, attributes]
d[1:5, ]

d$id

# new data must be of the same length as number of features, otherwise recycling will occur
d$rand <- rnorm(n=nrow(d))

idx <- which(d$rand > 0)
d[idx, ]

as(d, 'data.frame')

proj4string(p) <- '+proj=longlat +datum=WGS84' 
str(p)

# transform to UTM zone 14
p.utm <- spTransform(p, CRS('+proj=utm +zone=14 +datum=NAD83'))
cbind(gcs=coordinates(p), utm=coordinates(p.utm))

# transform to GCS NAD27
p.nad27 <- spTransform(p, CRS('+proj=longlat +datum=NAD27'))
cbind(coordinates(p), coordinates(p.nad27))

CRS('+init=epsg:4326')

## x <- readOGR(dsn='E:/gis_data/ca630/FG_CA630_OFFICIAL.gdb', layer='ca630_a')

## x <- readOGR(dsn='E:/gis_data/ca630', layer='pedon_locations')

## writeOGR(x, dsn='E:/gis_data/ca630', layer='pedon_locations', driver = 'ESRI Shapefile')

# use an example from the raster package
f <- system.file("external/test.grd", package="raster")
# create a reference to this raster
r <- raster(f)
# print the details
print(r)
# default plot method
plot(r)

# check: file is on disk
inMemory(r)
# load into memory, if possible
r <- readAll(r)
# check: file is in memory
inMemory(r)

## # using previous example data set
## writeRaster(r, filename='r.tif', options=c("COMPRESS=LZW"))

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

library(aqp)
library(sp)
library(raster)
library(rgdal)
library(soilDB)

# establish path to example data
ch2b.data.path <- 'C:/workspace/chapter-2b'

# load MLRA polygons
mlra <- readOGR(dsn=ch2b.data.path, layer='mlra-18-15-AEA')

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

rs <- stack(maat, map, ffd, gdd, rain_fraction, ppt_eff)
# reset layer names
names(rs) <- c('MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT')

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

# hand make a SpatialPoints object
# note that this is GCS
p <- SpatialPoints(coords = cbind(-120, 37.5), proj4string = CRS('+proj=longlat +datum=WGS84'))

# spatial extraction of MLRA data requires a CRS transformation
p.aea <- spTransform(p, proj4string(mlra))
over(p.aea, mlra)

# extract from a single RasterLayer
extract(maat, p)

# extract from a RasterStack
extract(rs, p)

# extract using a buffer with radius specified in meters (1000m)
extract(rs, p, buffer=1000)

## # sampling single RasterLayer
## sampleRegular(maat, size=10)
## 
## # sampling RasterStack
## sampleRegular(rs, size=10)

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

# this will be very slow for large grids
mean(values(maat), na.rm=TRUE)

# generally much faster and just as good, 
# given reasonable sampling strategy and sample size
mean(x.regular$MAAT, na.rm=TRUE)

# this value will be different than what you get
# it is after all, random sampling
mean(x.random$MAAT, na.rm=TRUE)

# takes a couple of seconds
z <- replicate(100, mean(sampleRandom(maat, size = 100, na.rm=TRUE), na.rm = TRUE))

# 90% of the time the mean MAAT values were within:
quantile(z, probs=c(0.05, 0.95))

# result is a SoilProfileCollection object
auburn <- fetchKSSL(series = 'auburn')

# extract site data
s <- site(auburn)

# these are GCS WGS84 coordinates from NASIS
coordinates(s) <- ~ x + y
proj4string(s) <- '+proj=longlat +datum=WGS84'

# return the result as a data.frame object
e <- extract(rs, s, df=TRUE)

# summarize out the extracted data
# note that we are "leaving out" the first column which contains an ID
summary(e[, -1])

# don't convert character data into factors
options(stringsAsFactors = FALSE)

# combine site data with extracted raster values, row-order is identical
res <- cbind(as(s, 'data.frame'), e)

# extract unique IDs and PRISM data
res <- res[, c('pedon_key', 'MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT')]

# join with original SoilProfileCollection object via pedon_key
site(auburn) <- res

# create an ordering of pedons based on the extracted effective PPT
new.order <- order(auburn$eff.PPT)

# setup figure margins
# set to single figure output, just in case you are working from the top of the examples
par(mar=c(5,2,4,2), mfcol=c(1,1))
# plot profile sketches
plot(auburn, name='hzn_desgn', print.id=FALSE, color='clay', plot.order=new.order, cex.names=0.85)

# add an axis with extracted raster values
axis(side=1, at = 1:length(auburn), labels = round(auburn$eff.PPT[new.order]), cex.axis=0.75)
mtext('Annual Sum of Monthly (PPT - ET_p) (mm)', side=1, line=2.5)
