library(raster)
library(sp)
library(rgdal)

# geodatabase path
mu.dsn <- 'C:/Users/Dylan.Beaudette/Desktop/MLRA-17-18-22A'
# name of featureclass
mu.layer <- 'mlra_v42-AEA'
# map unit symbols / keys to extract
mu.set <- c('18', '15')
mu.col <- 'MLRARSYM'

mu <- try(readOGR(dsn=mu.dsn, layer=mu.layer, stringsAsFactors = FALSE))

# just in case, coerce mu.col to character
mu[[mu.col]] <- as.character(mu[[mu.col]])

# coerce mu.set to character just in case
mu.set <- as.character(mu.set)
# filter
mu <- mu[which(mu[[mu.col]] %in% mu.set), ]

# save map unit data
writeOGR(mu, dsn='.', layer='mlra-18-15-AEA', driver='ESRI Shapefile', overwrite_layer = TRUE)


## raster data
maat <- raster('E:/gis_data/prism/final_MAAT_800m.tif')
map <- raster('E:/gis_data/prism/final_MAP_mm_800m.tif')
rain_fraction <- raster('E:/gis_data/prism/rain_fraction_mean_800m.tif')
eff_ppt <- raster('E:/gis_data/prism/effective_precipitation_800m.tif')
ffd <- raster('E:/gis_data/prism/ffd_mean_800m.tif')
gdd <- raster('E:/gis_data/prism/gdd_mean_800m.tif')


# crop to MU extent
mu.gcs <- spTransform(mu, CRS(proj4string(maat)))

maat <- crop(maat, mu.gcs)
map <- crop(map, mu.gcs)
rain_fraction <- crop(rain_fraction, mu.gcs)
eff_ppt <- crop(eff_ppt, mu.gcs)
ffd <- crop(ffd, mu.gcs)
gdd <- crop(gdd, mu.gcs)

# save rasters
writeRaster(maat, filename='MAAT.tif', options=c("COMPRESS=LZW"))
writeRaster(map, filename='MAP.tif', options=c("COMPRESS=LZW"))
writeRaster(rain_fraction, filename='rain_fraction.tif', options=c("COMPRESS=LZW"))
writeRaster(eff_ppt, filename='effective_preciptitation.tif', options=c("COMPRESS=LZW"))
writeRaster(ffd, filename='FFD.tif', options=c("COMPRESS=LZW"))
writeRaster(gdd, filename='GDD.tif', options=c("COMPRESS=LZW"))

