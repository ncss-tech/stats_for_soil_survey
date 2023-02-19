library(aqp)

## load original Sierra Transect (central Sierra, granite) data from CSV
granite <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/dahlgren-granitics.csv', stringsAsFactors = FALSE)

## load parallel Merhten formation transect from CSV
# note that there are two files
andesite <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/rasmussen-andesitic-lahar.csv', stringsAsFactors = FALSE)

andesites.site <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/rasmussen-andesitic-lahar-site.csv', stringsAsFactors = FALSE)

# note: these are data.frame objects
class(granite)

## convert Munsell notation into R colors (sRGB)
granite$soil_color <- with(granite, munsell2rgb(hue, value, chroma))
andesite$soil_color <- with(andesite, munsell2rgb(hue, value, chroma))

# results are hex notation of sRGB color coordinates, used by most software and WWW pages
granite$soil_color


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
g <- c(granite, andesite)

## init spatial data from coordinates (aqp 2.0+)
# GCS NAD83
initSpatial(g, crs = "EPSG:4269") <- ~ x + y

## alternately: init spatial data from coordinates (aqp <2.0; warnings on aqp 2.0+)
# coordinates(g) <- ~ x + y
# proj4string(g) <- "+proj=longlat +datum=WGS84"

# set horizon designation in metadata
hzdesgnname(g) <- 'name'


## quick check
# tighter margins
par(mar = c(0, 0, 3, 1))

plotSPC(g, width = 0.3)
plotSPC(g, width = 0.3, color = 'clay')

# fancy plot
par(mar = c(0, 0, 0, 0))
plotSPC(g, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, hz.depths.offset = 0.05, fixLabelCollisions = TRUE, cex.names = 0.66, width = 0.3, shrink = TRUE)

# see ?plotSPC for documentation on all function arguments
# expanded discussion of arguments + examples:
# https://ncss-tech.github.io/AQP/aqp/sketches.html

## load and merge sampled raster data
gis.data <- read.csv('https://github.com/ncss-tech/aqp/raw/master/misc/example-data/sierra-transect/transect-GIS-data.csv', stringsAsFactors = FALSE)

# this is an implicit left join
site(g) <- gis.data

# re-order accorging to elevation and effective PPT
plotSPC(g, width = 0.3, plot.order = order(g$elev))
plotSPC(g, width = 0.3, plot.order = order(g$effective.ppt_800))

par(mar = c(0, 0, 3, 1))
plotSPC(g, width = 0.3, color = 'clay', plot.order = order(g$elev))
plotSPC(g, width = 0.3, color = 'clay', plot.order = order(g$effective.ppt_800))

## compute ratio of oxalate-extractable Fe to CBD-extractable Fe
g$Fe_o_to_Fe_d <- g$Fe_o / g$Fe_d

# nice
plotSPC(g, width = 0.3, color = 'Fe_o_to_Fe_d', plot.order = order(g$effective.ppt_800))


# re-level grouping variable (a factor in R)
g$transect <- factor(g$transect, levels = c('Granite', 'Andesite'))

# plot grouped data
# note we can use the same arguments to plotSPC
par(mar = c(0, 0, 0, 1))
groupedProfilePlot(g, groups = 'transect', group.name.offset = -15, name.style = 'center-center', cex.names = 0.55, width = 0.3, shrink = TRUE)

