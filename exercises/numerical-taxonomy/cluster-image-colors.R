library(terra)
library(cluster)
library(ape)
library(farver)
library(MASS)
library(colorspace)
library(viridisLite)

## some notes on preparing an example image, not too large 
# library(magick)
# 
# im <- image_read('e:/personal/misc_work_images/a_nice_day_in_valley_springs.jpg')
# im <- image_resize(im, geometry = '400x', filter = 'cubic')
# image_write(im, path = 'photo.png', format = 'PNG')


## use an example image from Dylan

# URL to example image, you can swap out for any image you like
url <- 'https://github.com/ncss-tech/stats_for_soil_survey/raw/master/exercises/numerical-taxonomy/photo.jpg'
tf <- tempfile(fileext = '.jpg')

# download, this may fail if you are on the VPN
download.file(url, destfile = tf, mode = 'wb')

## or, use your own image via local file path or URL

# load as 3-band SpatRast object
# bands: red, green, blue (sRGB color space)
r <- rast(tf)

# check
plotRGB(r)

# re-name bands for access later
names(r) <- c('r', 'g', 'b')
r


## convert to CIE LAB color space, much more useful for analysis of color

# copy SpatRast / update with LAB color coordinates
r.lab <- r

# simple trick to directly access / replace values of a SpatRast
values(r.lab) <- convert_colour(values(r), from = 'rgb', to = 'lab', white_from = 'D65')
names(r.lab) <- c('l', 'a', 'b')

# check
# bands are CIE L, A, B color space coordinates
# https://en.wikipedia.org/wiki/CIELAB_color_space
# positive "A" coordinates are "red"
plot(r.lab, col = mako(25))


## take 500 random samples, extracting CIE LAB color coordinates
set.seed(42)
s <- spatSample(r.lab, size = 500, method = 'random', as.points = TRUE)

# view samples, looks good
plotRGB(r)
points(s, col = 'yellow')

# convert SpatVect -> data.frame
s.lab <- as.data.frame(s)

## convert CIE LAB values extracted at smples -> sRGB
s.rgb <- convert_colour(s.lab[, c('l', 'a', 'b')], from = 'lab', to = 'rgb', white_from = 'D65')

# convert to R colors, hex notation
s.cols <- rgb(s.rgb, maxColorValue = 255)

# 3-variable scatter plot matrix
# CIELAB color space coordinates
# colors are from sRGB color coordinates
pairs(s.lab[, c('l', 'a', 'b')], pch = 16, col = s.cols)


## compute pair-wise distances
# using the CIE2000 color contrast metric
# this is more interpretable / meaningful vs. Euclidian distance
# https://en.wikipedia.org/wiki/Color_difference
d <- compare_colour(from = s.lab[, c('l', 'a', 'b')], to = s.lab[, c('l', 'a', 'b')],, from_space = 'lab', method = 'CIE2000')

# convert full-matrix form to simpler 'dist' representation
d <- as.dist(d)

# check for 0 distances, can cause problems for nMDS
summary(d)

# if there are any, set those to a small value
d[which(d == 0)] <- 1


## perform nMDS (ordination) on distance matrix
mds <- sammon(d)
str(mds)

# simple visualization of nMDS ordination
# colors from sRGB color coordinates, converted to hex notation
par(mar = c(2, 2, 1, 1))
plot(mds$points, pch = 16, col = s.cols, cex = 2, axes = FALSE)
# add horizontal / vertical lines to see "origin" of nMDS coordinates
abline(h = 0, v = 0, lty = 3)
box()
mtext('nMDS Axis 1', side = 1, line = 0.5, font = 2)
mtext('nMDS Axis 2', side = 2, line = 0.5, font = 2)


## cluster into k-classes
# partitioning around medoids
# using the distance matrix vs. data martrix (diss = TRUE)
cl <- pam(d, k = 4, diss = TRUE)

# save clustering vector, these are integers
# interpret as categories
s.lab$cl <- factor(cl$clustering)

## extract medoid colors in sRGB color space
# cl$medoids is an index to the original data matrix
# will use these later
medoid.cols <- rgb(s.rgb[cl$medoids, ], maxColorValue = 255)

# quick check on medoid colors
swatchplot(medoid.cols)


## supervised classification to interpolate between point samples
# linear discriminant analysis (LDA) model works well here
# simple and efficient
m <- lda(cl ~ l + a + b, data = s.lab)

# wrapper function around predict method for LDA models
# only keep the most-likely class
# could also retain class probabilities
predfun <- function(model, data) {
  predict(model, data)$class
}

## predict over entire stack of LAB color coordinates
# this is a r-band SpatRast object
p <- predict(r.lab, m, fun = predfun)

## check:
plot(p, col = medoid.cols)








