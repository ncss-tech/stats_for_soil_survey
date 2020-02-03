library(aqp)
library(soilDB)
library(sharpshootR)
library(latticeExtra)


library(sp)
library(raster)
library(mapview)
library(leafem)

### Working on Drummer

soil <- 'DRUMMER'

## think about OSD
osd <- fetchOSD(soils = soil, extended = TRUE)
str(osd, 1)

# singleton SPC, ick
plotSPC(osd$SPC)

# better
par(mar=c(0, 1, 0, 1))
plotSPC(osd$SPC, cex.names=1, axis.line.offset = -5, width=0.1, x.idx.offset = 0.1, name='hzname')

# horizon thickness stats
hz.thick <- with(horizons(osd$SPC), bottom - top)
summary(hz.thick)
sd(hz.thick)

# hmm: simulate 8 Drummer pedons from OSD and stats on hz thickness
s <- sim(osd$SPC, n = 8, hz.sd=6)
str(s, 2)

# combine SoilProfileCollection objects
# original OSD + simulated
s <- aqp::union(list(osd$SPC, s))

# leave room for a summary of the simulated thickness
par(mar=c(0, 0, 3, 1))
plotSPC(s, name='hzname', cex.names=0.8, n.depth.ticks = 8)
title('OSD + 8 Realizations')

# ok that is neat, what about the rest?
str(osd, 1)

# competing series
# https://ncss-tech.github.io/AQP/soilDB/competing-series.html
head(osd$competing)


# get competing series OSD data
spc <- fetchOSD(c(soil, osd$competing$competing))

# this will only work for established series, e.g. those that have been "mapped" somewhere
idx <- which(spc$series_status == 'established')
spc <- spc[idx, ]

# save family taxa and set of series names for later
fm.name <- unique(na.omit(spc$family))
s.names <- unique(site(spc)$id)


par(mar=c(0.25,0,1,1))
plot(spc)
mtext(fm.name, side = 3, at = 0.5, adj = 0, line = -1, font=4)
mtext('source: Official Series Descriptions', side = 1, at = 0.5, adj = 0, line = -1, font=3, cex=1)


## siblings?
# https://ncss-tech.github.io/AQP/soilDB/siblings.html






## color stuff
# http://ncss-tech.github.io/AQP/aqp/investigating-soil-color.html
# http://ncss-tech.github.io/AQP/aqp/color-contrast.html


# a vector of named soil series
# the search is case insensitive
soils <- c('amador', 'pentz', 'pardee', 'auburn', 'loafercreek', 'millvilla')

# moist colors are converted from Munsell -> sRGB by default
s <- fetchOSD(soils)
# also convert dry colors
s.dry <- fetchOSD(soils, colorState = 'dry')

# quickly compare moist to dry colors
par(mar=c(1,0,2,1), mfrow=c(2,1))
plot(s) ; title('Moist Colors')
plot(s.dry) ; title('Dry Colors')





# get all NASIS pedons correlated to DRUMMER
pedons <- fetchNASIS(from='pedons', nullFragsAreZero=TRUE)

str(pedons, 2)


dx <- pedons$obs_date
dy <- rep(1, times=length(pedons))

dx <- sort(dx)
dy <- cumsum(dy)

plot(dx, dy, type='S', las=1, cex=1, cex.axis=0.8)
grid()

rmf <- get_RMF_from_NASIS_db()
lapply(rmf, head)



## KSSL
# http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html





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

