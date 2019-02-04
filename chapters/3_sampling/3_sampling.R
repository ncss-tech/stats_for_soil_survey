## ----setup, echo=FALSE, warning=FALSE------------------------------------
# setup
library(knitr, quietly=TRUE)

opts_chunk$set(message=FALSE, warning=FALSE, background="#F7F7F7", fig.retina=1, dev="png", tidy=FALSE, verbose=FALSE)

## ------------------------------------------------------------------------
# Clay example. Test to see the number of samples necessary to detect a 3 percent difference in clay between two horizons.
power.t.test(power = 0.95, sd = 2, delta = 16 - 19) # delta = the difference between the two means
power.t.test(power = 0.95, sd = 3, delta = 16 - 19)

## ---- echo=FALSE---------------------------------------------------------
# Generate a graphical comparison of a fictitious clay example with 2 standard deviations
x1 <- seq(12, 20, 0.1)
y1 <- dnorm(x1, mean = 16, sd = 2)

x2 <- seq(15, 23, 0.1)
y2 <- dnorm(x2, mean = 19, sd = 2)

summary(c(x1, x2))

plot(x1, y1, type = "l", xlim = range(c(x1, x2)), 
     main = "Overlapping Populations", 
     xlab = "Clay %"
     )
lines(x2, y2)
abline(v = mean(c(x1, x2)), lty = 2)

## ----simple, fig.width=4, fig.height=4-----------------------------------
# load dataset from Soil Data Access
library(soilDB)
library(raster)

b <- c(-86.35,39.82,-86.34,39.83)
x <- mapunit_geom_by_ll_bbox(b)
polys <- crop(x, extent(b[c(1, 3, 2, 4)]))

plot(polys)

# Generate simple random sample
test <- spsample(polys, n = 15, type = "random")
points(test, pch = 19)

## ----stratified, fig.width=4, fig.height=4-------------------------------
plot(polys, main = "Stratified random sample")

# Generate a spatially stratified random sample
test <- spsample(polys, n = 15, type = "stratified")
points(test, pch = 19)

## ----two_stage, fig.width=4, fig.height=4--------------------------------
plot(polys, main = "Two-stage random")

# Select 8 samples from each square
s <- sapply(slot(polys, "polygons"), function(x) spsample(x, n = 5, type = "random"))
points(sample(s, 1)[[1]], pch = 19) # randomly select 1 square and plot
points(sample(s, 1)[[1]], pch = 19) # randomly select 1 square and plot

## ----systematic, fig.width=4, fig.height=4-------------------------------
plot(polys, main = "Systematic sample")

# Generate systematic random sample
test <- spsample(polys, n = 15, type = "regular")
points(test, pch = 19)

## ----clustered, fig.width=4, fig.height=4--------------------------------
plot(polys, main = "Clustered (n = 3) random sample")

# Generate cluster random sample
test <- spsample(polys, n = 15, type = "clustered", nclusters = 3, iter = 10)
points(test, pch = 19)

## ----clhs, fig.width=4, fig.height=4-------------------------------------
library(clhs)
library(raster)

# import volcano DEM, details at http://geomorphometry.org/content/volcano-maungawhau
data(volcano)
volcano_r <- raster(as.matrix(volcano[87:1, 61:1]), 
                    crs = CRS("+init=epsg:27200"), 
                    xmn = 2667405, xmx = 2667405 + 61 * 10, 
                    ymn = 6478705, ymx = 6478705 + 87 * 10
                    )
names(volcano_r) <- "elev"

# calculate slope from the DEM
slope_r <- terrain(volcano_r, opt = "slope", unit = "degrees")

# Stack Elevation and Slope
rs <- stack(volcano_r, slope_r)

# generate cLHS design
cs <- clhs(rs, size = 20, progress = FALSE, simple = FALSE)

# Plot cLHS Samples
par(mar=c(1,1,1,4))
plot(volcano_r, axes=FALSE)
points(cs$sampled_data)

# Summary of clhs object
summary(cs$sampled_data)$data

# Summary of raster objects
cbind(summary(volcano_r), summary(slope_r))

## ----clhs_sub------------------------------------------------------------
sub_s <- sampleRandom(volcano_r, size = 200, sp = TRUE) # random sample function from the raster package

# s <- clhs(sub_s, size = 20, progress = FALSE, simple = FALSE)

## ----seval---------------------------------------------------------------
# create a polygon from the spatial extent of the volcano dataset
test <- as(extent(volcano_r), "SpatialPolygons")

# take a large random sample
sr400 <- spsample(test, n = 400, type = "random")

# take a small random sample
sr <- spsample(test, n = 20, type = "random")

# take a small stratified random sample
str <- spsample(test, n = 23, type = "stratified", iter = 1000)[1:20]

# take a cLHS sample
# cs <- clhs(rs, size = 20, progress = FALSE, simple = FALSE)

# Combind and Extract Samples
s <- rbind(data.frame(method = "Simple Random 400", extract(rs, sr400)),
           data.frame(method = "Simple Random", extract(rs, sr)),
           data.frame(method = "Stratified Random", extract(rs, str)),
           data.frame(method = "cLHS", cs$sampled_data@data)
           )

# Summarize the sample values
aggregate(slope ~ method, data = s, function(x) round(summary(x)))

# Plot overlapping density plots to compare the distributions between the large and small samples
library(ggplot2)
ggplot(s, aes(x = slope, col = method)) + geom_density(cex = 2)

## ----fig.width=8.25, fig.height=3.5--------------------------------------
# plot the spatial locations            
par(mfrow = c(1, 3), mar=c(1,2,4,5))
plot(volcano_r, main = "Simple random", cex.main = 2, axes=FALSE)
points(sr, pch = 3, cex = 1.2)

plot(volcano_r, main = "Stratified random", cex.main = 2, axes=FALSE)
points(str, pch = 3, cex = 1.2)

plot(volcano_r, main = "cLHS", cex.main = 2, axes=FALSE)
points(cs$sampled_data, pch = 3, cex = 1.2)

