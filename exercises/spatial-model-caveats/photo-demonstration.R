library(terra)
library(raster)
library(sf)
library(gstat)
library(randomForest)
library(clhs)
library(rms)
library(NLMR)


r <- rast('ah.png')

# remove color table
coltab(r) <- NULL

# sampling points, high density, but not necessarily in efficient locations
set.seed(101010)
s <- spatSample(r, 250, method = 'random', as.points = TRUE, na.rm = TRUE)

# evaluation / test samples for later
s.eval <- spatSample(r, 100, method = 'regular', as.points = TRUE, na.rm = TRUE)

# colors for thematic map
.cols <- hcl.colors(255, palette = 'mako')

# source data + samples
plot(r, col = .cols, axes = FALSE)
points(s, col = 2, cex = 0.5, pch = 0)
points(s.eval, col = 3, cex = 0.5, pch = 5)


## ordinary kriging

# annoying spatVect -> SPDF
x <- as(s, 'Spatial')

v <- variogram(ah ~ 1, x)
m <- fit.variogram(v, vgm(psill = 12000, model = "Sph", range = 80, nugget = 100))

plot(v, model = m)


## this should work... ?
# gOK <- gstat(NULL, "ah", ah ~ 1, as(s, 'Spatial'), model = m)
# z <- interpolate(r, gOK, debug.level = 10)

# annoying spatRast -> SPixDF
r.sp <- as(raster(r), 'SpatialPixelsDataFrame')

z <- krige(formula = ah ~ 1, locations = x, newdata = r.sp, model = m)

# back to spatRast
z <- rast(z)

# kriging error
plot(z$var1.var, legend = FALSE, col = .cols, axes = FALSE, main = 'Original', mar = c(1,1,1,1))


par(mfrow = c(1, 2))

plot(r, legend = FALSE, col = .cols, axes = FALSE, main = 'Original', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

plot(z$var1.pred, legend = FALSE, col = .cols, axes = FALSE, main = 'Ordinary Kriging', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

# external evaluation
ev <- extract(c(r, z), s.eval)

# RMSE: 35
sqrt(mean((ev$ah - ev$var1.pred)^2))


## conditional simulation

# interesting artistic possibilities

# using the variogram model from above
# beta is roughly the original mean value
# nmax: use only a couple of neighbors, otherwise instability
# setting block argument results in interesting results
set.seed(101010)
csim <- krige(
  ah ~ 1, 
  x, 
  r.sp, 
  model = m,
  nmax = 2, 
  beta = mean(s$ah), 
  nsim = 3
)

csim <- c(r, rast(csim))
# csim <- rast(csim)
plot(csim, legend = FALSE, col = .cols, axes = FALSE, mar = c(0.1, 0.1, 0.1, 0.1), main = '')


## develop additional "covariates"

# neutral landscapes
rc <- nlm_percolation(
  prob = 0.25,
  ncol = ncol(r), nrow = nrow(r)
)

rc <- rast(rc)
plot(rc)

# coordinates
.x <- r
.y <- r
.x[] <- xFromCell(r, 1:ncell(r))
.y[] <- yFromCell(r, 1:ncell(r))

# noise
.noise <- r
values(.noise) <- runif(n = ncell(r), min = 0, max = 255)

# flipped original
.flip <- flip(r)

# .mixed <- (255 - .mangled/2) + .noise

# .roughness <- terrain(r, v = 'roughness')


a <- c(.x, .y, .noise, .flip, rc)
names(a) <- c('x', 'y', 'noise', 'flipped', 'rc')

# a <- aggregate(.mixed, fact = 3)


plot(a, legend = FALSE, axes = FALSE, col = .cols)


# sample
s2 <- s

e <- extract(a, s2)
e$ah <- s2$ah

e <- na.omit(e)


## GAM




## RF


(m <- randomForest(ah ~ flipped + x + y + noise + rc, data = e, nodesize = 15, ntree = 500))

(m <- randomForest(ah ~ flipped + x + y + noise, data = e, nodesize = 15, ntree = 500))

# internal evaluation: 40
sqrt(mean((e$ah - predict(m, newdata = e))^2))


rf.pred <- predict(a, m)

par(mfrow = c(1, 2))

plot(r, col = .cols, axes = FALSE, legend = FALSE, main = 'Original', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

plot(rf.pred, legend = FALSE, col = .cols, axes = FALSE, main = 'RandomForest', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)


# external evaluation
ev <- extract(c(r, rf.pred), s.eval)

# RMSE: 54
sqrt(mean((ev$ah - ev$lyr1)^2))



## MLR
dd <- datadist(e)
options(datadist = "dd")

(m <- ols(ah ~ flipped + x + y + noise + rc, data = e))

# interpretation of partial effects is fraught with peril!
plot(Predict(m))
plot(summary(m))


ols.pred <- predict(a, m)

par(mfrow = c(1, 2))

plot(r, col = .cols, axes = FALSE, legend = FALSE, main = 'Original', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

plot(ols.pred, legend = FALSE, col = .cols, axes = FALSE, main = 'Multiple Linear Regression', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

# external evaluation
ev <- extract(c(r, ols.pred), s.eval)

# RMSE: 84
sqrt(mean((ev$ah - ev$lyr1)^2))


