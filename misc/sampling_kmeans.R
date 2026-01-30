
library(sf)
library(dplyr)
library(rmapshaper)
library(terra)
library(ggplot2)
library(sgsR)


# Function ----
cscs <- function(x, vars = NULL, n = NULL, center = TRUE, scale = TRUE, iter.max = 1000, samp = TRUE, probs = TRUE) {
  
  if (class(x) == "SpatRaster") {
    df <- terra::as.data.frame(x, xy = TRUE)
  }
  if (class(x) == "RasterStack") {
    df <- as.data.frame(x, xy = TRUE)
  }
  rn <- row.names(df)
  row.names(df) <- NULL
  
  idx <- df[vars] |>
    complete.cases()
  df2 <- df[idx, vars] |>
    scale(center = center, scale = scale)
  
  cl <- df2 |> 
    kmeans(center = n, iter.max = iter.max)
  
  df2 <- cbind(df2, cl = cl$cluster)
  df$cluster <- NA
  df[row.names(df2), "cluster"] <- cl$cluster
  x$cluster <- df$cluster
  
  n_clus <- cl$centers |> nrow()
  
  if (samp %in% c("both", "TRUE")) {
    d <- fields::rdist(cl$centers, df2[, vars])
    idx2 <- apply(d, 1, which.min)
    idx3 <- row.names(df2)[idx2]
    cl_cen <- df[idx3, ]
    # cl_cen
  }
  
  if (probs == TRUE) {
    idx <- which(complete.cases(df))
    cl_lda <- MASS::lda(x = df[idx, vars], grouping = df[idx, "cluster"])
    # cl_rf <- ranger::ranger(x = df[idx, vars], y = as.factor(df[idx, "cluster"]), probability = TRUE)
    
    pred.pr <- function(model, data) {
      predict(model, data)$posterior
    }
    # r <- predict(cl_lda, df[-idx])$posterior
    r1 <- predict(x[[-9]], cl_lda, fun = pred.pr)
    
    
    # f <- function(model, ...) predict(model, ...)$predictions
    # # r <- predict(cl_lda, df[-idx])$posterior
    # r2 <- predict(x[[-9]], cl_rf, fun = f, na.rm = TRUE)
  }
  
  # idx <- list()
  # for (i in 1:n_clus) {
  #   z <- rbind(cl$centers[i, ], df2[, vars])
  #   d <- dist(z)
  #   idx[[i]] <- which.min(as.matrix(d)[1, -1])
  # }
  # idx <- unlist(idx)
  # 
  # cl_cen <- df2[idx, ]
  # cl_cen
  
  return(list(x = x, samp = cl_cen, probs = r1))
}



# Example ----
# Create a 4 square polygon
n <- 2
bb <- st_make_grid(st_bbox(c(xmin = 0, xmax = n, ymin = 0, ymax = n)), n = n)
grd <- st_as_sf(bb)
grd$ID <- 1:length(bb)
df <- st_centroid(grd) |> st_coordinates() |> as.data.frame()
bb2 <- st_make_grid(st_bbox(c(xmin = 0, xmax = n, ymin = 0, ymax = n)), n = n)
grd2 <- st_as_sf(bb2)
grd2$landform = c(rep("backslope", 3), "summit")
# grd3 <- ms_dissolve(grd2, field = "mapunit")
grd2$component = c(rep("a", 3), "b")
ggplot() +
  geom_sf(data = grd2, aes(fill = component)) +
  ggtitle("map unit") +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(), axis.title = element_blank()
  )


test <- mapply(FUN = function(i, x, n) {
  st_coordinates(st_sample(x[[1]][i, ], size = n))
  },
  i = 1:4,
  x = lapply(1:4, function(x) grd2),
  n = c(1, 1, 1, 3),
  SIMPLIFY = FALSE
  )
test <- do.call("rbind", test)
test_sf <- st_as_sf(as.data.frame(test), coords = 1:2)

ex <- extract(vect(grd2), st_coordinates(test_sf))
test_sf <- cbind(test_sf, ex)
test_sf$component <- ifelse(test_sf$landform == "backslope", "a", "b")
test_sf <- within(test_sf, {
  samp_co = c(
    rbinom(3, 1, prob = 0.75),
    rbinom(3, 1, prob = 0.25)
  )
  samp_clay = c(
    rnorm(3, 15),
    rnorm(3, 30)
  )
  samp_co = ifelse(samp_co == 1, "a", "b")
  p  = c(rep(0.75/ 3, 3), rep(0.25 / 3, 3))
  ip = 1 / p
  w  = p * 4
  iw = 1 / w
})


ggplot() +
  geom_sf(data = grd2, aes(fill = landform)) +
  ggtitle("map unit") +
  theme(
    axis.text = element_blank(), 
    axis.ticks = element_blank(), axis.title = element_blank()
  ) +
  geom_sf(
    data = mutate(test_sf, component = samp_co), 
    aes(shape = component), size = 3
    )
  # geom_sf_text(data = test_sf, aes(label = samp_co), nudge_y = - 0.1, nudge_x = 0.45) +
  # guides(fill = guide_legend((title = "component")))



table(pred    = test_sf$component, obs       = test_sf$samp_co) |>
  addmargins()
table(mapunit = test_sf$component, component = test_sf$samp_co) |> 
  addmargins()

table(test_sf$component) |> as.vector() * c(1, 1/3)
table(test_sf$samp_co == "a")[2:1] |> as.vector() * c(1, 1/3)

vars <- c("component", "samp_co")
test_sf[vars] <- lapply(st_drop_geometry(test_sf[vars]), as.factor)
yardstick::conf_mat(test_sf, truth = samp_co, estimate = component, case_weights = w)$table |>
  addmargins()

# survey
test_sf$id <- 1:nrow(test_sf)
test3 <- test_sf |>
  srvyr::as_survey_design(
    weights = w,
    strata  = landform,
    ids = id,
    nest = TRUE
    )
srvyr::survey_count(test3, samp_co, vartype = "ci")

(tb <- table(test_sf$component))
(wtd <- Hmisc::wtd.table(test_sf$component, weights = test_sf$w, type = "table"))
xtabs(w ~ component, data = test_sf)
(mapunit <- prop.table(wtd) |> round(2))
(accuracy <- mapunit |> as.matrix() |> apply(X = _, 2, max))

weighted.mean(test_sf$samp_clay, w = test_sf$p)

a = c(2.1, 0.8, 1.1, 0.3, 1.4, 0.9, 0.9, 0.6, 1.3, 1.5)
b = c(5.1, 3.4, 4.5, 4, 3.9, 5.5, 4.7, 5.5, 4.2, 4.1)

mean(c(a, b))
weighted.mean(c(a, b), w = c(rep(0.9/10, 10), rep(0.1/10, 10)))



# Create a sixteen square polygon
n <- 6
bb <- st_make_grid(st_bbox(c(xmin = 0, xmax = n, ymin = 0, ymax = n)), n = n)
grd <- st_as_sf(bb)
grd$ID <- 1:length(bb)
df <- st_centroid(grd) |> st_coordinates() |> as.data.frame()
bb2 <- st_make_grid(st_bbox(c(xmin = 0, xmax = n, ymin = 0, ymax = n)), n = n/3)
grd2 <- st_as_sf(bb2)


n2 <- 9


# CSCS
fs <- cscs(df, vars = c("X", "Y"), n =  n2, samp = "both")
fs_sf <- st_as_sf(fs$samp, coords = c("X", "Y"))
grd$cluster <- as.factor(fs$x$cluster)

ggplot() + 
  geom_sf(data = grd, aes(fill = cluster), alpha = 0.5, show.legend = TRUE) +
  geom_sf(data = grd, fill = NA) +
  geom_sf(data = grd2, lwd = 1, fill = NA) +
  geom_sf(data = fs_sf) +
  ggtitle(paste("CSCS; n = ", nrow(fs_sf)))


# cLHS
df2 <- df
idx <- clhs(df2, size =  n2)
df_sf <- st_as_sf(df[idx, ], coords = c("X", "Y"))
brks <- quantile(0:6, probs = seq(0, 1, 1/n2))

ggplot() + 
  geom_sf(data = grd, aes(fill = cluster), alpha = 0.5) +
  geom_sf(data = grd, fill = NA) +
  geom_sf(data = grd2, lwd = 1, fill = NA) +
  geom_sf(data = df_sf) +
  geom_hline(yintercept = brks, lty = "dotted") +
  geom_vline(xintercept = brks, lty = "dotted") +
  ggtitle(paste("cLHS; n =", nrow(df_sf)))


# SRS
idx <- sample(1:nrow(df), n2)
sr_sf <- st_as_sf(df[idx, ] , coords = c("X", "Y"))

ggplot() + 
  geom_sf(data = grd, aes(fill = cluster), alpha = 0.5, show.legend = FALSE) +
  geom_sf(data = grd, fill = NA) +
  geom_sf(data = grd2, lwd = 1, fill = NA) +
  geom_sf(data = sr_sf) +
  ggtitle(paste("SRS; n = ", nrow(fs_sf)))



# Voclano
data("volcano")
volcano_r <- rast(
  volcano[87:1, 61:1], 
  crs = crs("+init=epsg:27200"), 
  extent = c(
    xmin = 2667405, xmax = 2667405 + 61 * 10,
    ymin = 6478705, ymax = 6478705 + 87 * 10
  )
)
names(volcano_r) <- "elev"

dd <- rast("C:/workspace2/dem_derivatives.tif")
names(dd) <- c("elev", "hs", "slp", "kc", "kp", "twi", "rsp")


samp_cscs <- cscs(dd, vars = names(dd), n = 7, samp = "both")
r <- samp_cscs$x
s <- st_as_sf(samp_cscs$samp, coords = c("x", "y"), crs = crs("+init=epsg:27200"))

df <- as.data.frame(dd)
clhs_idx <- clhs(df, 7)
pts <- as.points(dd)[clhs_idx, ] |> st_as_sf()


samp_srs <- spatSample(dd, 5, as.points = TRUE) |> st_as_sf()

tm_shape(r[[9]]) + tm_raster(style = "cat", n = 100, palette = RColorBrewer::brewer.pal(10, "Paired")) + 
  tm_shape(s) + tm_dots(size = 0.5) +
  tm_layout(legend.outside = TRUE, title = "CSCS")

tm_shape(r[[9]]) + tm_raster(style = "cat", n = 100, palette = RColorBrewer::brewer.pal(10, "Paired")) + 
  tm_shape(pts) + tm_dots(size = 0.5) +
  tm_layout(legend.outside = TRUE, title = "cLHS")

tm_shape(r[[9]]) + tm_raster(style = "cat", n = 100, palette = RColorBrewer::brewer.pal(10, "Paired")) + 
  tm_shape(samp_srs) + tm_dots(size = 0.5) +
  tm_layout(legend.outside = TRUE, title = "SRS")

p <- samp_cscs$probs
names(p) <- paste0("cluster_", 1:nlyr(p))
plot(p, col = viridis::cividis(20))

tm_shape(p[[1:4]]) + tm_raster(n = 10, palette = viridis::cividis(10), col = 7) + 
  tm_facets(nrow = 1) +
  tm_shape(s) + tm_dots(size = 0.5) +
  tm_legend() + 
  tm_layout(legend.outside = TRUE, title = "CSCS")

r2 <- c(r, strata = r[["cluster"]])
names(r2)[10] <- "strata"
test_eq <- sample_strat(sraster = r2[["strata"]], 5, allocation = "equal", plot = TRUE)
test_p <- sample_strat(sraster = r2[["strata"]], 35, plot = TRUE)

tm_shape(r[[9]]) + tm_raster(style = "cat", n = 100, palette = RColorBrewer::brewer.pal(10, "Paired")) + 
  tm_shape(test_eq) + tm_dots(size = 0.5) +
  tm_legend() + 
  tm_layout(legend.outside = TRUE, title = "Stratified-Random")
tm_shape(r[[9]]) + tm_raster(style = "cat", n = 100, palette = RColorBrewer::brewer.pal(10, "Paired")) + 
  tm_shape(test_p) + tm_dots(size = 0.5) +
  tm_legend() + 
  tm_layout(legend.outside = TRUE, title = "Stratified-Random")


plot(r[[2]], col = colorRampPalette(c("white", "black"))(100))


r$strata <- r$cluster
test <- sample_strat(r[["cluster"]], 1)


# Weighted Stratified Estimation ----

library(mapac)
library(survey)
library(caret)


exdata <- aa_examples("stehman2014")
df <- as.data.frame(exdata[1:3])
cm <- table(predicted = df$map, observed = df$reference)
cm |> addmargins()


# post stratified confusion matrix ----
cm_aa <- aa_stratified(
  stratum   = exdata$stratum, 
  reference = exdata$ref, 
  map       = exdata$map, 
  h         = exdata[["h"]], 
  N_h       = exdata[["N_h"]]
)
cm_w <- cm_aa$cmp * sum(exdata$N_h)
names(attributes(cm_w)$dimnames) <- c("predicted", "observed")
cm_w |> addmargins()


cm_a <- confusionMatrix(cm)$byClass[, c(1, 3, 8)]
cm_w_a <- confusionMatrix(cm_w)$byClass[, c(1, 3, 8)]
colnames(cm_a) <- c("Prod Acc", "User Acc", "Prevalence")
colnames(cm_w_a) <- c("Prod Acc", "User Acc", "Prevalence")
cm_a |> round(2)
cm_w_a |> round(2)

x1 <- c(2.1, 0.8, 1.1, 0.3, 1.4, 0.9, 0.9, 0.6, 1.3, 1.5)
x2 <- c(5.1, 3.4, 4.5, 4, 3.9, 5.5, 4.7, 5.5, 4.2, 4.1)

iw1 <- 1/rep(1/90, 10)
iw2 <- 1/rep(1/10, 10)

mean(c(x1, x2))
weighted.mean(c(mean(x1), mean(x2)), w = c(0.9, 0.1))
weighted.mean(c(x1, x2), w = c(iw1, iw2))


# weighting strata ----
library(sf)
library(terra)
library(sgsR)

# import volcano DEM, details at http://geomorphometry.org/content/volcano-maungawhau
data("volcano")
volcano_r <- rast(
  volcano[87:1, 61:1],
  crs = crs("+init=epsg:27200"),
  extent = c(
    xmin = 2667405,
    xmax = 2667405 + 61 * 10,
    ymin = 6478705,
    ymax = 6478705 + 87 * 10
  )
)
names(volcano_r) <- "elev"

# calculate slope from the DEM
slope_r <- terrain(volcano_r, v = "slope", unit = "degrees")

# Stack Elevation and Slope
rs  <- c(volcano_r, slope_r)

# Covariate Space Coverage Sampling
k <- 5
n <- 10
set.seed(123)
fs_strata_k <- strat_kmeans(rs, nStrata = k, iter = 10000)
set.seed(123)
fs_strata_n <- strat_kmeans(rs, nStrata = k*n, iter = 1000)
fs_strata <- fs_strata_k * 100 + fs_strata_n
set.seed(123)
fs_samp <- sample_nc(rs, nSamp = k*n, iter = 10000)
# fs_samp <- sample_strat(fs_strata, nSamp = 10, allocation = "equal", method = "random")
fs_sf <- extract(
  c(fs_strata_n, 
    fs_strata_k, 
    fs_strata, 
    rs),
  fs_samp
)
names(fs_sf)[2:4] <- c("n", "k", "k_n")

# Plot CSCS Samples
plot(fs_strata_n, col = map.pal("random", n))
plot(fs_samp, col = "black", cex = 1, pch = 19, add = TRUE)


# Compute weights
k_n <- table(value = fs_sf$n) |> 
  as.data.frame(responseName = "nk_samp") |>
  mutate(value = as.integer(value))
tab <- freq(fs_strata_n) |>
  left_join(k_n, by = "value") |>
  mutate(
    # population proportion
    P       = round(count / sum(count), 2),
    
    # proportional sample reference
    nk_pop  = count * (sum(nk_samp)/sum(count)),
    wk_pop  = count / nk_pop,
    pk_pop  = 1 / wk_pop,
    
    # unequal sample weights
    wk_samp = count / nk_samp,
    pk_samp = 1 / wk_samp,
    
    # pr = pk_p / pk_e,
    wk_ratio = wk_samp / wk_pop,
    # nr = nk_p / nk_e,
    layer = NULL
  ) |>
  rename(strata = value, Nk = count) |>
  select(strata, Nk, P, nk_pop, nk_samp, wk_pop, wk_samp, pk_pop, pk_samp, wk_ratio)
idx <- c(4:10)
tab[idx] <- lapply(tab[idx], function(x) round(x, 2))
tab


