# plot the locations of the gopheridge pedons within R
# Steps:
# 1) subset to a new data frame
# 2) create a spatial points data frame (SPDF)
# 3) plot the data

# load libraries
library(sp)
library(maps)
library(soilDB)

data("gopheridge")
data("loafercreek")

# join two or more SPCs
loafergopher <- union(list(gopheridge,loafercreek))

# subset standard WGS84 decimal degree coordinates from the gopheridge SPC by specifying column names
gopher.locations <- site(gopheridge)[, c('site_id', 'x_std', 'y_std')]

head(gopher.locations)

# initialize coordinates in an SpatialPointsDataFrame
coordinates(gopher.locations) <- ~ x_std + y_std

gopher.locations@coords

# define coordinate system
proj4string(gopher.locations) <- '+proj=longlat +datum=WGS84'

# set plot margins
par(mar=c(0,0,0,0))

# add pedon data locations, note symbol styling
map(database = 'county', regions = 'california')
points(gopher.locations, cex=0.5, pch=3, col='red')

# add pedon data locations, note symbol styling (graphics limited to just point extent)
plot(gopher.locations, cex=0.5, pch=3, col='red')
map(database = 'county', regions = 'california', add = T)

box()

## Filtering to remove NA
f <- fetchNASIS()

# coordinates(f) <- ~ x_std + y_std
# Error: cannot initialize a SpatialPoints object with missing coordinates

# create `f.with.spatial.info` which is just the NON-NA pedon locations
f.with.spatial.info <- f[!is.na(f$x_std),]

# now the call to coordinates() will work
coordinates(f.with.spatial.info) <- ~ x_std + y_std
proj4string(f.with.spatial.info) <- '+proj=longlat +datum=WGS84'

# plot the spatial slot directly
plot(f.with.spatial.info@sp)

# the spatial slot is just one part of the SoilProfileCollection
class(f.with.spatial.info)
      
# coerce SPC to SPDF
spdf.from.spc <- as(f.with.spatial.info, 'SpatialPointsDataFrame')
class(spdf.from.spc)
rgdal::writeOGR(spdf.from.spc, dsn = '.', layer='example_shapefile', driver = 'ESRI Shapefile')
