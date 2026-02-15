# quick clip of a named list of 
library(raster)
library(rgdal)

# read shp file containing desired extent to crop
f <- readOGR("data/ca630_clip.shp")

# config file contains `raster.list` -- named list of raster inputs
source("config.R") 

# iterate through raster list, transform shp to CRS of raster, and crop
res <- lapply(as.list(unlist(raster.list)), function(r) {
  ras <- raster(r)
  f.t <- spTransform(f, CRS(proj4string(ras)))
  fname <- paste0("crop_",basename(slot(slot(ras, 'file'), 'name'))) 
  
  print(paste0(fname))
  crop(ras, f.t, filename=fname)
})

