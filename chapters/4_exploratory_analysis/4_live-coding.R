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





amador <- seriesExtent(s = 'amador')
class(amador)

s <- spsample(amador, n = 100, type = 'hexagonal')


par(mar=c(1,1,3,1))
plot(maat, ext=extent(s), main='MAAT and Amador Extent\n100 Sampling Points', axes=FALSE)
plot(amador, add=TRUE)
points(s, cex=0.25)


mukeys <- SDA_query("SELECT DISTINCT mukey FROM component WHERE compname = 'Amador' AND majcompflag = 'Yes' ;")

amador.pts <- fetchSDA_spatial(mukeys$mukey, by.col = 'mukey', method = 'point', chunk.size = 2)

par(mar=c(1,1,3,1))
plot(maat, ext=extent(s), main='MAAT and Amador Extent\n100 Sampling Points', axes=FALSE)
points(amador.pts, cex=0.25, pch='.')


# return the result as a data.frame object
e <- extract(rs, s, df=TRUE)
# check out the extracted data
summary(e[, -1])


# return the result as a data.frame object
e.pts <- extract(rs, amador.pts, df=TRUE)
# check out the extracted data
summary(e.pts[, -1])

maat.comparison <- list(
  'regular samples'=e$MAAT, 
  'polygon centroids'=e.pts$MAAT
  )

lapply(maat.comparison, length)
lapply(maat.comparison, summary)


par(mar=c(4.5, 8, 3, 1))
boxplot(maat.comparison, horizontal=TRUE, las=1, xlab='MAAT (deg C)', varwidth=TRUE, boxwex=0.5)






