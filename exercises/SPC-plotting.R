library(aqp)
library(soilDB)


##
## use OSDs
##

# soil series mapped with DRUMMER
s <- siblings('drummer')
str(s)

# get OSD morphology, taxonomy, and more
x <- fetchOSD(c('drummer', s$sib$sibling))

# basic plot
par(mar = c(0, 0, 0, 2))
plotSPC(x, cex.names = 0.8)

# subset, just fine-silty soils
x.sub <- subset(x, tax_partsize == 'fine-silty')

# check: ok
plotSPC(x.sub, cex.names = 0.8)

# shrink margins
par(mar = c(0, 0, 0, 2))

# adjust arguments:
# * no depth axis
# * wider
# * larger hz designations
# * smaller ID labels
# * horizon names "inside" of profiles
# * horizon depths next to boundaries
# * scale depths by 110% 
# * shrink horizon designations >= 4 characters
plotSPC(x.sub, plot.depth.axis = FALSE, width = 0.3, cex.names = 0.8, cex.id = 0.75, name.style = 'center-center', hz.depths = TRUE, scaling.factor = 1.1, shrink = TRUE, shrink.cutoff = 4)



##
## NASIS local database / selected set
## query Site/Pedon by taxonname: "lucy"
##
# fetch all pedons from the selected set in local NASIS database
x <- fetchNASIS(from='pedons')

# make sketches of the first 20 pedons
par(mar=c(0,0,3,0))
plotSPC(x[1:20, ], name='hzname', label='taxonname', width = 0.3)

# color horizons using field-described soil texture class
# note that the ordering of textures / colors is not intuitive (alphabetical)
plotSPC(x[1:20, ], name='hzname', label='taxonname', width = 0.3, color = 'texcl')

# tabulate classes in data
table(x$texcl)

# create new horizon level attribute
# using factors to encode ordering within textures
x$texture.factor <- factor(
  x$texcl, 
  levels = SoilTextureLevels()
)

# ahh that is better
plotSPC(x[1:20, ], name='hzname', label='taxonname', width = 0.3, color = 'texture.factor')



##
## make some fake, but plausible data
##

# simulate 5 profiles, using letters a,b,c,d,e as IDs
# result is a list of single-profile SPC objects
x <- lapply(letters[1:5], random_profile, SPC = TRUE, method = 'LPP', n = c(4, 5, 6))

# combine into single SPC object
x <- combine(x)

# basic plot
par(mar = c(0, 0, 3, 2))
plotSPC(x, color = 'p1', cex.names = 0.8)

# demonstrate plot.order

# bottom depth of each profile
.depths <- profileApply(x, max)

# sort `depth` ascending order
new.order <- order(.depths)

# use this index to re-order profiles
plotSPC(x, color = 'p1', cex.names = 0.8, plot.order = new.order)




