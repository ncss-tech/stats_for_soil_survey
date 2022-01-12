# project extent+linework workflow script developed for CA649 update

# load required packages
library(aqp)
library(soilDB)
library(sf)

# define project(s) of interest (we will get related mapunits)
projects <- list('MLRA 18 - CA649 Auburn & Blasingame 999 Map Units on Volcanic Rocks')

# query placeholder projects by name
project <- lapply(projects, function(x) {
  soilDB::fetchNASISWebReport(projectname = x)
})

# create a combined legend table
plegend <- unique(do.call('rbind', lapply(project, function(p) p$mapunit)))

# write legend table to file            
write.csv(plegend, "project_mapunits.csv")

# create a combined SoilProfileCollection with component data
spc <- aqp::combine(lapply(project, function(p) p$spc))

# add mapunit level information to SPC
site(spc) <- unique(plegend[,c('dmuiid','musym','nationalmusym')])

# read geodatabase (could be official one, or some intermediate/edited version)
ssurgo <- st_read('E:/Geodata/soils/MLRA_2_SON_FY2021.gdb', 'mupolygon')

# mukey is same as legend mapunit record ID
mukeys <- project.mapunits$lmapunitiid

# get SSURGO lines for selected mukeys
ssurgo.sub <- subset(ssurgo, ssurgo$MUKEY %in% mukeys)

# or query from SDA (slower but more "generic" and intended for live SSURGO data)
# ssurgo.sub <- fetchSDA_spatial(mukeys)

# write to shapefile
st_write(ssurgo.sub, "my_project_extent.shp", delete_layer = TRUE)

# inspect result
plot(ssurgo.sub$geometry)
