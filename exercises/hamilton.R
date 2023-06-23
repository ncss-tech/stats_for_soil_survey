library(aqp)
library(soilDB)
library(sf)
library(sharpshootR)
library(SoilTaxonomy)

# hamilton, MT
bb <- '-114.2000 46.2411,-114.2000 46.2696,-114.1255 46.2696,-114.1255 46.2411,-114.2000 46.2411'



## assemble AOI polygon into WKT
wkt <- sprintf('POLYGON((%s))', bb)

## init sf polygon
x <- st_as_sfc(wkt)

# set CRS as GCS WGS84
st_crs(x) <- 4326

## get overlapping map unit keys
# could also use SDA_query() with more elaborate SQL
m <- SDA_spatialQuery(x, what = 'mukey')

## compose SQL to return component details for these map unit keys
# return only:
# * map units overlapping with BBOX
# * major components
# * no misc. areas that might share name with a poorly-named soil series
sql <- sprintf(
  "SELECT mukey, cokey, compname, compkind, comppct_r 
  FROM component 
  WHERE mukey IN %s 
  -- AND majcompflag = 'Yes'
  AND compkind != 'Miscellaneous area'
  ", format_SQL_in_statement(as.integer(m$mukey))
)

## send to SDA, result is a data.frame
s <- SDA_query(sql)



## get OSD morphology + extended summaries 
osd <- fetchOSD(unique(s$compname), extended = TRUE)


par(mar = c(1, 0, 1, 2))
plotGeomorphCrossSection(osd, type = 'line', maxIter = 100, j.amount = 0.05, verbose = TRUE)


par(mar = c(1, 0, 1, 2))
plotGeomorphCrossSection(osd, type = 'line', clust = FALSE)

osd$hillpos


o <- reconcileOSDGeomorph(osd, 'hillpos')
res <- vizHillslopePosition(o$geom, verbose = TRUE)
print(res$fig)



