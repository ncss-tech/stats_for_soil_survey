library(soilDB)
library(sp)
library(raster)
library(rgdal)

library(mapview)
library(leafsync)

# aoi: bounding-box corners (xmin, ymin, xmax, ymax)
# coordinate reference system of AOI (GCS WGS84 )
a <- list(
  aoi = c(-114.16, 47.65, -114.08, 47.68),
  crs = '+init=EPSG:4326'
)

# fetch gSSURGO map unit keys at native resolution (~30m)
x <- mukey.wcs(aoi = a, db = 'gnatsgo')

# convert raster extent into vector 
g <- as(extent(x), 'SpatialPolygons')
proj4string(g) <- projection(x)

# get intersecting SSURGO linework as SpatialPolygonsDataFrame from SDA
p <- SDA_spatialQuery(g, what = 'geom', geomIntersection = TRUE)

# transform to AEA CRS
p <- spTransform(p, CRS(projection(x)))


m1 <- mapview(x, zcol = "mukey", map.types = "Esri.WorldImagery", legend = FALSE, na.color = NA)
m2 <- mapview(p, map.types = "Esri.WorldImagery", legend = FALSE, na.color = NA, color = 'yellow', fill = NA, lwd = 1)

sync(m1, m2)




ssurgo <- SDA_spatialQuery(g, what = 'geom', geomIntersection = TRUE)
statsgo <- SDA_spatialQuery(g, what = 'geom', geomIntersection = TRUE, db = 'STATSGO')


m1 <- mapview(ssurgo, map.types = "Esri.WorldImagery", legend = FALSE, na.color = NA, color = 'yellow', fill = NA, lwd = 1)
m2 <- mapview(statsgo, map.types = "Esri.WorldImagery", legend = FALSE, na.color = NA, color = 'white', fill = NA, lwd = 1)

sync(m1, m2)
