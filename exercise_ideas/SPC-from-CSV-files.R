library(aqp)

## load original Sierra Transect (central Sierra, granite) data from CSV
granite <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/dahlgren-granitics.csv', stringsAsFactors = FALSE)

## load parallel Merhten formation transect from CSV
# note that there are two files
andesite <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/rasmussen-andisitic-lahar.csv', stringsAsFactors = FALSE)

andesites.site <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/rasmussen-andisitic-lahar-site.csv', stringsAsFactors = FALSE)


# convert Munsell notation into R colors (sRGB)
granite$soil_color <- with(granite, munsell2rgb(hue, value, chroma))
andesite$soil_color <- with(andesite, munsell2rgb(hue, value, chroma))


## init SoilProfileCollection for granite transect
depths(granite) <- id ~ top + bottom
# transfer site level attributes
site(granite) <- ~ elev + MAAT + MAP + geo + x + y


## init SoilProfileCollection for andesite transect
depths(andesite) <- id ~ top + bottom
# transfer site level attributes
site(andesite) <- ~ elev + precip + MAP + MAT + veg + Fe_d_to_Fe_t

# join coordinates + notes via `id`
site(andesite) <- andesites.site


## label transects via site-level attribute
site(granite)$transect <- 'Granite'
site(andesite)$transect <- 'Andesite'



## combine into single SPC, note that attribute names may not be the same
g <- combine(granite, andesite)

## init spatial data from coordinates
coordinates(g) <- ~ x + y
proj4string(g) <- '+proj=longlat +datum=NAD83'

# set horizon designation in metadata
hzdesgnname(g) <- 'name'


# quick check
par(mar = c(0,0,3,1))
plotSPC(g, width = 0.3)
plotSPC(g, width = 0.3, color = 'clay')

## load and merge sampled raster data
gis.data <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/transect-GIS-data.csv', stringsAsFactors = FALSE)

site(g) <- gis.data

plotSPC(g, width = 0.3, plot.order = order(g$elev))
plotSPC(g, width = 0.3, plot.order = order(g$elev))

plotSPC(g, width = 0.3, color = 'clay', plot.order = order(g$elev))
plotSPC(g, width = 0.3, color = 'clay', plot.order = order(g$effective.ppt_800))

## compute some idices if possible
g$Fe_o_to_Fe_d <- g$Fe_o / g$Fe_d

# nice
plotSPC(g, width = 0.3, color = 'Fe_o_to_Fe_d', plot.order = order(g$effective.ppt_800))



# re-level factors
g$transect <- factor(g$transect, levels = c('Granite', 'Andesite'))

# plot grouped data
par(mar = c(0, 0, 1, 0))
groupedProfilePlot(g, groups = 'transect', group.name.offset = -15)


## interactive map
library(mapview)

# convert site + sp components of SPC -> sp object
g.spdf <- as(g, 'SpatialPointsDataFrame')

mapview(g.spdf, zcol = 'transect')

