## Demonstrate cLHS using a color photograph, would work for any kind of sampling strategy.



library(terra)
library(clhs)
library(colorspace)


# example image with a few colors
# note 'noflip' argument
u <- 'https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/refs/heads/master/exercises/spatial-model-caveats/scout-color.png'
r <- rast(u, noflip = TRUE)

# coarsen 5x via mean
a <- aggregate(r, fact = 5, method = mean)

# looks OK
plotRGB(a, smooth = FALSE)

# re-name bands for later
names(a) <- c('r', 'g', 'b')

# extract values at 200 regularly spaced points
# keep as spatVector
s <- spatSample(a, size = 200, method = 'regular', as.points = TRUE)

# inspect regular sample points
plotRGB(a, smooth = FALSE)
points(s, col = 'white')

# convert to data.frame for cLHS
d <- as.data.frame(s)

# re-scale sRGB coordinates from [0, 255] -> [0, 1]
d <- d / 255

# convert sRGB -> CIELAB (color space approximates avg human perception)
lab <- convertColor(d, from = 'sRGB', to = 'Lab')
lab <- data.frame(lab)

# single iteration of cLHS, requesting 15 sub-samples
idx <- clhs(lab, size = 15)

# visual explanation of cLHS subset
plotRGB(a, smooth = FALSE)
points(s, col = 'white')
points(s[idx, ], cex = 2, pch = 0, lwd = 2, col = 'green')


# replicate cLHS 10 times
sr <- replicate(10, clhs(lab, size = 15), simplify = FALSE)

# visualize 10 x 15 sub-samples
swatchplot(
  lapply(sr, function(i) {
    rgb(d[i, ])
  })
)

# combined figures from above
par(mfcol = c(1, 2))

plotRGB(a, smooth = FALSE, main = 'Single Interation of cLHS')
points(s, col = 'white')
points(s[idx, ], cex = 2, pch = 0, lwd = 2, col = 'green')

# this time, organize colors based on CIELAB representation
cLHS.colors <- lapply(sr, function(i) {
  # simple clustering (explained in Part 2 of this class)
  col.order <- hclust(dist(lab[i, ]), method = 'ward.D')$order
  
  # re-order cLHS sample index
  col.idx <- i[col.order]
  
  # convert sRGB colors -> hex notation for plotting
  rgb(d[col.idx, ])
})

names(cLHS.colors) <- 1:length(cLHS.colors)

swatchplot(cLHS.colors, mar = c(0, 2, 2, 0))
title('10 Iterations of cLHS')

## Notes:
# * cLHS is non-deterministic
# * cLHS samples aren't ranked by "importance"
# * cLHS samples may not represent the center of natural stratification







