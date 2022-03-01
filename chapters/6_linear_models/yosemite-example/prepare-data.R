library(raster)
library(soilDB)
library(rgdal)
library(sp)

# latest data from Henry
# this includes reasonable estimates of MAST, MSST, MWST
m <- fetchHenry(what='all', project='CA790')
m <- m$sensors

# just sensors at 50cm
m <- subset(m, subset=sensor_depth == 50)

# load extra site information
site.data <- read.csv('site-data.csv', stringsAsFactors=FALSE)

# create site_name by adding yosemite_ to each id
site.data$name <- paste0('yosemite_', site.data$id)

# read in elevation and solar radiation data
r.elev <- raster('L:/NRCS/MLRAShared/CA790/DEM-derived/elev30.tif')
r.solar <- raster('L:/NRCS/MLRAShared/CA790/DEM-derived/beam_rad_sum_mj.tif')
r.tci <- raster('L:/NRCS/MLRAShared/CA790/DEM-derived/saga-wetness.tif')

# combine into a stack, and fix names
r.stack <- stack(r.elev, r.solar, r.tci)
names(r.stack) <- c('elev','solar', 'tci')

# transform hobo locations to CRS of raster
m <- spTransform(m, CRS(projection(r.stack)))

# extract raster stack at hobo locations
e <- extract(r.stack, m)

# convert to DF and add site_name
e <- data.frame(name=m$name, e, stringsAsFactors = FALSE)

# join with spatial data + MAST/MSST/MWST
m <- merge(m, e, by='name')

# join O hz thickness
m <- merge(m, site.data[, c('name', 'o.hz.thick')], by='name')

# keep SPDF and re-name
s <- m

# save for later use
save(s, file='cached-data.rda')

xy <- coordinates(spTransform(s, CRS("+init=epsg:4326")))
s2 <- cbind(xy, as.data.frame(s)[-c(25:26)])
write.csv(s2, "henry_CA790_data.csv", row.names = TRUE)



