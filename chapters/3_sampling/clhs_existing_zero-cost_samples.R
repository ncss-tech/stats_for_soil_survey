## clhs sampling in R - with existing (zero-cost) point observations
## demo v0.1 --- andrew.gbrown@ca.usda.gov
## 
### all credit goes to Dave White for preparing the spatial datasets for this demo
### http://ncss-tech.github.io/stats_for_soil_survey/presentations/examples/clhs.html

## CONCEPT
# the basic premise of this extension to _cost-constrained cLHS_ is that you can force a clhs routine
# include arbitrary (user-defined) records _in your final sample_. These "forced" data come at "zero-cost" 
# because, well (at least theoretically) you _already have them_. In theory, this _very-low-cost_ partially
# counteracts the fact that the existing locations are not likely to span the environmental variable space fully.

## THIS DEMO
# this demo uses the demostration dataset prepared by Dave White.
# A set of 30 random samples is collected. These are hypothetical "pre-existing data".
# They are less likely to consistenly span the full range of the data space, 
# when compared to the exhaustive sampling approach
# 
# Then the raster.stack is regularly sampled ("exhaustively"; n=10,000) and these regular
# samples are passed to the cLHS algorithm. 

# Both CLHS routines are run using the cost raster `r.cost`, but the second object created
# is set up to retain the 30 random samples in the last "set" of points cLHS algorithm
# evaluates (when the number of iterations set by the user is reached). These observations
# are appended to the begining of the `all.observations` SpatialPointsDataFrame, and indexed
# using a numeric index `1:30`
# 
# The final result shows the location of cost-constrained cLHS, with and without pre-existing samples.

# __WARNING:__ Use this routine your own risk. The addition to the cLHS algorithm is new and may potentially have 
# unintended effects on clhs result. This addition was just made to the latest official release of `clhs` 
# (was published to CRAN mid-October 2018)

library(sp)
library(clhs)

setwd("C:/workspace2/clhs/")

# load raster data of same extent and resolution
r.claymin <- raster("claymin.tif")
r.mrrtf <- raster("mrrtf.tif")
r.mrvbf <- raster("mrvbf.tif")
r.ndvi <- raster("ndvi.tif")
r.sagawi <- raster("sagawi.tif")
r.cost <- raster("cost.tif")

r.stack.cost <- stack(r.claymin, r.mrrtf, r.mrvbf, r.ndvi, r.sagawi, r.cost)
names(r.stack.cost) <- c('claymin', 'mrrtf', 'mrvbf', 'ndvi', 'sagawi', 'cost')
r.stack.cost <- readAll(r.stack.cost)

r.extent.poly <- as(extent(r.stack.cost), "SpatialPolygons")

proj4string(r.extent.poly) <- proj4string(r.stack.cost)

# take 30 point locations "randomly" these are our hypothetical existing obsrvations
# note that we set na.rm = TRUE so our random observations are not in NA space
obs.existing <- sampleRandom(r.stack.cost, size = 30, sp=TRUE, na.rm=TRUE)

# use regular sampling to lower the number of cells we are plugging into clhs algorithm
reg.samples <- sampleRegular(r.stack.cost, size = 10000, sp = TRUE)

# 
#obs.existing <- spTransform(obs.existing, CRS(proj4string(reg.samples)))

all.observations <- rbind(obs.existing, reg.samples)
# pending a fix in clhs package (https://github.com/pierreroudier/clhs/issues/3)
# NA cost values must be filtered from the samples
reg.samples <- reg.samples[which(!is.na(reg.samples$cost)), ]
all.observations <- all.observations[which(!is.na(all.observations$cost)), ]

s <- clhs(reg.samples, size = 100, cost='cost', simple=FALSE, iter=2500)

# clhs diagnostic plot for normal c-cLHS
# plot(s, mode = c("obj", "box"))

s.with.existing <- clhs(all.observations, size = 100, 
                        include = 1:nrow(obs.existing), cost = 'cost', 
                        simple = FALSE, iter = 2500)

# clhs diagnostic plot for c-cLHS with existing data
#plot(s.with.existing, mode = c("obj", "box"))

s.all <- rbind(data.frame(method = "c-cLHS", s$sampled_data@data),
              data.frame(method = "c-cLHS (w/ existing)",  s.with.existing$sampled_data@data), 
              data.frame(method = "Exhaustive", reg.samples@data))

samples.idx <- s$index_samples
existing.idx <- s.with.existing$index_samples

# check visually on the sagawi raster
par(mar = c(1,1,1,1))
plot(r.sagawi, axes=FALSE)
contour(r.cost, nlevels=10, col='black', add=TRUE)

# plot the regularly-spaced samples that were selected by normal cLHS
points(reg.samples[samples.idx, ], bg = 'red', pch=21)

# plot the regularly-spaced samples that were selected when set to retain 
# the n=30 existing points that were randomly sampled before running clhs
points(all.observations[existing.idx, ], bg = 'blue', pch=21)

# overplot with yellow to show which ones were the 30 that we started with 
points(obs.existing, bg = 'yellow', pch=21)

# check visualy on the cost raster
par(mar = c(0,0,0,0))

# use cost raster as a backdrop
plot(r.cost, axes=FALSE)

# plot the regularly-spaced samples that were selected by normal cLHS
points(s$sampled_data, bg = 'red', pch=21)

# plot the regularly-spaced samples that were selected when set to retain 
# the n=30 existing points that were randomly sampled before running clhs
points(s.with.existing$sampled_data, bg = 'blue', pch=21)

# overplot with yellow to show which ones were the 30 that we started with 
points(obs.existing, bg = 'yellow', pch=21)
