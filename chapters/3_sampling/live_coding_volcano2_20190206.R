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
cs <- clhs(rs, size = 0.01*ncell(rs), iter=5000, progress = TRUE, simple = FALSE)

# Plot cLHS Samples
par(mar=c(1,1,1,4))
plot(slope_r, axes=FALSE)
points(cs$sampled_data)

# create a polygon from the spatial extent of the volcano dataset
test <- as(extent(volcano_r), "SpatialPolygons")
test <- crop(test, extent(volcano_r))

# take a large random sample
sr400 <- spsample(test, n = 400, type = "random")

# take a small random sample
sr <- spsample(test, n = 20, type = "random")

# take a small stratified random sample
set.seed(13462346)
str <- spsample(test, n = 23, type = "stratified", iter = 1000)

# take a cLHS sample
cs <- clhs(rs, size = 20, progress = FALSE, simple = FALSE)

# Combind and Extract Samples
s <- rbind(data.frame(method = "Simple Random 400", extract(rs, sr400)),
           data.frame(method = "Simple Random", extract(rs, sr)),
           data.frame(method = "Stratified Random", extract(rs, str)),
           data.frame(method = "cLHS", cs$sampled_data@data)
)

aggregate(slope ~ method, data = s, function(x) round(summary(x)))

names(s)
head(slope_r)
