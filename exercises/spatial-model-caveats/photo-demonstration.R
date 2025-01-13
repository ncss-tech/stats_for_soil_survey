
## NLMR not on CRAN
# remotes::install_github("cran/RandomFieldsUtils")
# remotes::install_github("cran/RandomFields")
# remotes::install_github("ropensci/NLMR")


library(terra)
library(raster)
library(sf)
library(gstat)
library(randomForest)
library(clhs)
library(rms)
library(NLMR)


# example image

# portrait of a famous actor
r <- rast('ah.png')

# an RC airplane
# r <- rast('scout.png')

# init standardized layer name
names(r) <- 'var'

# remove color table
coltab(r) <- NULL

# sampling points, high density, but not necessarily in efficient locations
set.seed(101010)
s <- spatSample(r, 250, method = 'random', as.points = TRUE, na.rm = TRUE)

# evaluation / test samples for later
s.eval <- spatSample(r, 100, method = 'regular', as.points = TRUE, na.rm = TRUE)

# colors for thematic map
.cols <- hcl.colors(n = 25, palette = 'roma')

# source data + samples
plot(r, col = .cols, axes = FALSE)
points(s, col = 1, cex = 1, pch = 16)
points(s.eval, col = 1, cex = 1, pch = 15)


## evaluation of sampled data-space
s




## ordinary kriging

# annoying spatVect -> SPDF
x <- as(s, 'Spatial')

# 
v <- variogram(var ~ 1, x)
m <- fit.variogram(v, vgm(psill = 12000, model = "Sph", range = 80, nugget = 100))

plot(v, model = m)


## this should work... ?
# gOK <- gstat(NULL, "ah", ah ~ 1, as(s, 'Spatial'), model = m)
# z <- interpolate(r, gOK, debug.level = 10)

# annoying spatRast -> SPixDF
r.sp <- as(raster(r), 'SpatialPixelsDataFrame')

z <- krige(formula = var ~ 1, locations = x, newdata = r.sp, model = m)

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

# RMSE
sqrt(mean((ev$var - ev$var1.pred)^2))


## block kriging
z.b <- krige(formula = var ~ 1, locations = x, newdata = r.sp, model = m, block = c(30, 30))

# back to spatRast
z.b <- rast(z.b)


par(mfrow = c(1, 3))

plot(r, legend = FALSE, col = .cols, axes = FALSE, main = 'Original', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

plot(z$var1.pred, legend = FALSE, col = .cols, axes = FALSE, main = 'Ordinary Kriging', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

plot(z.b$var1.pred, legend = FALSE, col = .cols, axes = FALSE, main = 'Block Kriging (30x30)', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)



## conditional simulation

# interesting artistic possibilities

# using the variogram model from above
# beta is roughly the original mean value
# nmax: use only a couple of neighbors, otherwise instability
# setting block argument results in interesting results
set.seed(101010)
csim <- krige(
  var ~ 1, 
  x, 
  r.sp, 
  model = m,
  nmax = 2, 
  beta = mean(s$var), 
  nsim = 3
)

csim <- c(r, rast(csim))
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
e$var <- s2$var

e <- na.omit(e)


## GAM




## RF


(m <- randomForest(var ~ flipped + x + y + noise + rc, data = e, nodesize = 15, ntree = 500))

(m <- randomForest(var ~ flipped + x + y + noise, data = e, nodesize = 15, ntree = 500))

# internal evaluation: 40
sqrt(mean((e$var - predict(m, newdata = e))^2))


rf.pred <- predict(a, m)

par(mfrow = c(1, 2))

plot(r, col = .cols, axes = FALSE, legend = FALSE, main = 'Original', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)

plot(rf.pred, legend = FALSE, col = .cols, axes = FALSE, main = 'RandomForest', mar = c(1,1,1,1))
points(s, col = 2, cex = 0.5, pch = 0)


# external evaluation
ev <- extract(c(r, rf.pred), s.eval)

# RMSE
sqrt(mean((ev$var - ev$lyr1)^2))



## MLR
dd <- datadist(e)
options(datadist = "dd")

(m <- ols(var ~ flipped + x + y + noise + rc, data = e))

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

# RMSE
sqrt(mean((ev$var - ev$lyr1)^2))


