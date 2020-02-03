library(aqp)
library(soilDB)
library(sharpshootR)
library(sp)
library(raster)
library(mapview)
library(leafem)

## Working on Drummer


# ## takes too long, likely time-out
# hz <- SDA_query("SELECT hzname
#           FROM
#           component JOIN chorizon ON component.cokey = chorizon.cokey
#           WHERE compname = 'Drummer' ;")

# tab <- table(hz)
# ptab <- prop.table(tab)
# sort(ptab)


# let SDA do all of the work
hz <- SDA_query("SELECT hzname, COUNT(hzname) AS n
          FROM
          component JOIN chorizon ON component.cokey = chorizon.cokey
          WHERE compname = 'Drummer' 
          GROUP BY hzname ;")

hz
idx <- order(hz$n, decreasing = TRUE)
hz[idx, ]

idx <- grep(hz$hzname, pattern = 'H')
hz[idx, ]




# let SDA find the unique mukey
query <- "SELECT DISTINCT mapunit.mukey
FROM 
mapunit JOIN component ON mapunit.mukey = component.mukey
JOIN chorizon ON component.cokey = chorizon.cokey 
WHERE
compname = 'drummer'
AND hzname LIKE 'H%' ;"

drummer.problem.hz <- SDA_query(query)

head(drummer.problem.hz)

query <- sprintf("SELECT mukey, chorizon.cokey, compname, comppct_r, hzname, hzdept_r, hzdepb_r 
          FROM 
          component JOIN chorizon ON component.cokey = chorizon.cokey
          WHERE mukey = '%s' 
          ORDER BY cokey, hzdept_r ASC;", drummer.problem.hz$mukey[1])

SDA_query(query)

# number mukey ...
nrow(drummer.problem.hz)


drummer.mu.bbox <- fetchSDA_spatial(x=drummer.problem.hz$mukey[1], by.col = 'mukey', method = 'bbox')
(mv <- mapview(drummer.mu.bbox, legend=FALSE, map.types='OpenStreetMap', alpha.regions=0.25))



e <- extent(drummer.mu.bbox)
e <- e + 0.1

(mv <- leafem::addFeatures(map=mv, data=as(e , 'SpatialPolygons'), color='black', fill=FALSE, weight=2))

as.vector(e)

x <- fetchKSSL(bbox=c(e[2], e[3], e[1], e[4]), returnMorphologicData = TRUE, simplifyColors = TRUE)
x <- x$SPC

coordinates(x) <- ~ x + y
proj4string(x) <- '+proj=longlat +datum=WGS84'

plotSPC(x, name='hzn_desgn', color='moist_soil_color', width = 0.2, label='taxonname')

idx <- grep(pattern = 'drummer', x$taxonname, ignore.case = TRUE)
x.sub <- x[idx, ]

horizons(x.sub)[, 1:10]


mv <- leafem::addFeatures(map=mv, data=as(x, 'SpatialPointsDataFrame'), color='darkgreen', fill=TRUE, weight=2, label=x$taxonname, radius=2)

leafem::addFeatures(map=mv, data=as(x.sub, 'SpatialPointsDataFrame'), color='firebrick', fill=FALSE, weight=2, label=x$taxonname, radius=10)

