# load dataset from Soil Data Access
library(soilDB)
library(raster)

b <- c(-86.35,39.82,-86.34,39.83)
x <- mapunit_geom_by_ll_bbox(b)

plot(x)

polys <- crop(x, extent(b[c(1, 3, 2, 4)]))

plot(polys)

# Generate simple random sample
set.seed(1235123)
rnorm(1)

test <- spsample(polys, n = 105, type = "random")
points(test, pch = 19, col="BLUE")

# REBOOT OF Ch 3 Sec 2.3
## improved two stage random demo (showing internals)
# the strata are shown with a colored background

# so we create a factor that reflects the unique mapunits
uid <- factor(1:length(polys@polygons))

plot(polys, col = uid, main = "Two-stage random")

s <- sapply(slot(polys, "polygons"), function(x) {
  # we will collect 5 random points from each stratum
  res <- spsample(x, n = 5, type = "random")
  
  plot(res, col="WHITE", pch = 19, add = T)
  return(res)
})

# the second stage of sampling is shown by overplotting with crosshairs
points(sample(s, 1)[[1]], pch = 3, cex=2) # randomly select 1 polygon and plot

points(sample(s, 1)[[1]], pch = 3, cex=2) # randomly select 1 polygon and plot


# each of these is an instance of the second tier of sampling
# sample(s, 1)[[1]]