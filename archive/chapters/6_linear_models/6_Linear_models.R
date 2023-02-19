
# knit options
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE, background="#F7F7F7", fig.retina=1, dev="png", tidy=FALSE, verbose=FALSE, cache = TRUE)



library(aqp)     # specialized soil classes and functions
library(soilDB)  # NASIS and SDA import functions
library(raster)  # guess
library(rgdal)   # spatial import
library(ggplot2) # graphing
library(tidyr)   # data manipulation
library(dplyr)   # data manipulation


# pedons <- fetchNASIS(rmHzErrors = FALSE) # beware the error messages, by default they don't get imported unless you override the default, which in our case shouldn't cause any problems

githubURL <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/ch7_data.Rdata"
load(url(githubURL))

str(pedons, max.level = 2) # Examine the makeup of the data we imported from NASIS.



s <- site(pedons) # extract the site data frame from the pedons soil profile collection object

s$surface_gravel <- with(s, surface_gravel - surface_fgravel) # recalculate gravel to exclude fine gravel
s$frags <- apply(s[grepl("surface", names(s))], 1, sum) # calculate total surface rock fragments

# density plot
select(s, surface_cobbles, surface_gravel, surface_fgravel, frags) %>%
  gather() %>%
  ggplot(aes(x = value, color = key)) +
  geom_density(size = 1.5)

# histogram
ggplot(s, aes(x = frags)) +
  geom_histogram()

# summarize all columns that pattern match either "surface" or "frags"
apply(s[grepl("surface|frags", names(s))], 2, function(x) round(summary(x))) 

# number of samples greater than 100
sum(s$frags > 100) 

# number of samples less than  1
sum(s$frags < 1)



test <- aggregate(frags ~ landform.string, data = s, quantile, probs = c(0.05, 0.5, 0.95))

# subset the data frame to only include landforms with greater than 3 observations
test <- subset(test, frags[, 2] > 3) 

# sort the data frame by the frags matrix column
test[order(- test$frags[, 2]), ]



# generalize the landform.string
s$landform <- ifelse(grepl("fan|terrace|sheet|drainageway|wash", s$landform.string), "fan", "hill") 

test <- aggregate(frags ~ landform, data = s, quantile, probs = c(0.05, 0.5, 0.95))

test[order(test$frags[, 2]), ]

# density plot
select(s, frags, surface_cobbles, surface_gravel, landform) %>%
  gather(key = "rocks", value = "percent", - landform) %>%
  ggplot(aes(x = percent, color = rocks)) +
  geom_density(size = 1.5) +
  facet_wrap(~ landform)



test <- aggregate(frags ~ landform + hillslope_pos, data = s, quantile, probs = c(0.05, 0.5, 0.95))

test[order(- test$frags[, 2]), ]



test <- aggregate(frags ~ landform + paste(shapedown, shapeacross), data = s, quantile, probs = c(0.05, 0.5, 0.95))

test[order(- test$frags[, 2]), ]



# Subset Generic landforms and Select Numeric Columns
s_fan <- subset(s, landform == "fan", select = c(frags, surface_gravel, bedrckdepth, slope_field, elev_field))
s_hill <- subset(s, landform == "hill", select = c(frags, surface_gravel, bedrckdepth, slope_field, elev_field))

# Correlation Matrices
round(cor(s_fan, use = "pairwise"), 2)
round(cor(s_hill, use = "pairwise"), 2)

# Scatterplot Matrices
library(GGally)
ggpairs(s_fan)
ggpairs(s_hill)



# Custom function to filter out the data for the 3 soil scientists with the most data
s <- within(s, {
  old = describer
  describer2 = NA
  describer2[grepl("Stephen", old)] = "Stephen" # least senior
  describer2[grepl("Paul", old)]    = "Paul"
  describer2[grepl("Peter", old)]   = "Peter"   # most senior
  })

test <- aggregate(frags ~ landform + describer2, data = s, quantile, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)

test[order(- test$frags[, 2]), ]



# Convert soil profile collection to a spatial object
pedons2 <- pedons
slot(pedons2, "site") <- s # this is dangerous, but something needs to be fixed in the site() setter function
idx <- complete.cases(site(pedons2)[c("x", "y")]) # create an index to filter out pedons with missing coordinates
pedons2 <- pedons2[idx]
coordinates(pedons2) <- ~ x + y # set the coordinates
proj4string(pedons2) <- CRS("+init=epsg:4326") # set the projection
pedons_sp <- as(pedons2, "SpatialPointsDataFrame") # coerce to spatial object
pedons_sp <- spTransform(pedons_sp, CRS("+init=epsg:5070")) # reproject

# Read in soil survey area boundaries
# ssa <- readOGR(dsn = "I:/geodata/soils/soilsa_a_nrcs.shp", layer = "soilsa_a_nrcs")
# ca794 <- subset(ssa, areasymbol == "CA794") # subset out Joshua Tree National Park
# ca794 <- spTransform(ca794, CRS("+init=epsg:5070"))

# Plot
plot(ca794, axes = TRUE)
plot(pedons_sp, col='red', add = TRUE) # notice the points outside the boundary

# Write shapefile of pedons
# writeOGR(pedons_sp, dsn = "F:/geodata/project_data/8VIC", "pedon_locations", driver = "ESRI Shapefile") 


## 
## # set file path
## folder <- "I:/geodata/project_data/R8-VIC/ca794/"
## 
## # list of file names
## files <- c(
##   elev   = "ned30m_8VIC.tif", # elevation
##   slope  = "ned30m_8VIC_slope5.tif", # slope gradient
##   aspect = "ned30m_8VIC_aspect5.tif", # slope aspect
##   twi    = "ned30m_8VIC_wetness.tif", # topographic wetness index
##   twi_sc = "ned30m_8VIC_wetness_sc.tif", # transformed twi
##   ch     = "ned30m_8VIC_cheight.tif", # catchment height
##   z2str  = "ned30m_8VIC_z2stream.tif", # height above streams
##   mrrtf  = "ned30m_8VIC_mrrtf.tif", # multiresolution ridgetop flatness index
##   mrvbf  = "ned30m_8VIC_mrvbf.tif", # multiresolution valley bottom flatness index
##   solar  = "ned30m_8VIC_solar.tif", # solar radiation
##   precip = "prism30m_8VIC_ppt_1981_2010_annual_mm.tif", # annual precipitation
##   precipsum = "prism30m_8VIC_ppt_1981_2010_summer_mm.tif", # summer precipitation
##   temp   = "prism30m_8VIC_tmean_1981_2010_annual_C.tif", # annual temperature
##   ls     = "landsat30m_8VIC_b123457.tif", # landsat bands
##   pc     = "landsat30m_8VIC_pc123456.tif", # principal components of landsat
##   tc     = "landsat30m_8VIC_tc123.tif", # tasseled cap components of landsat
##   k      = "gamma30m_8VIC_namrad_k.tif", # gamma radiometrics signatures
##   th     = "gamma30m_8VIC_namrad_th.tif",
##   u      = "gamma30m_8VIC_namrad_u.tif",
##   cluster = "cluster152.tif" # unsupervised classification
##   )
## 
## # combine the folder directory and file names
## geodata_f <- paste0(folder, files)
## names(geodata_f) <- names(files)
## 
## # Create a raster stack
## geodata_r <- stack(geodata_f)
## 
## # Extract the geodata and add to a data frame
## data <- raster::extract(geodata_r, pedons_sp, sp = TRUE)@data
## 
## # Modify some of the geodata variables
## idx <- aggregate(mast ~ cluster, data = data, mean, na.rm = TRUE)
## names(idx)[2] <- "cluster_mast"
## data <- merge(data, idx, by = "cluster", all.x =  TRUE)
## 
## data <- within(data, {
##   mast = temp - 4
##   cluster  = factor(cluster, levels = 1:15)
##   cluster2 = reorder(cluster, cluster_mast)
##   gsi      = (ls_3 - ls_1) / (ls_3 + ls_2 + ls_1)
##   ndvi     = (ls_4 - ls_3) / (ls_4 + ls_3)
##   sw       = cos(aspect - 255)
##   })
## 
## # save(data, ca794, pedons, file = "C:/workspace/ch7_data.Rdata")
## 
## # # Strip out location and personal information before uploading to the internet
## # s[c("describer", "describer2", "x", "y", "x_std", "y_std", "utmnorthing", "utmeasting", "classifier")] <- NA
## # slot(pedons, "site") <- s
## # data[c("describer2", "x_std", "y_std")] <- NA
## # save(data, ca794, pedons, file = "C:/workspace2/github/stats_for_soil_survey/trunk/data/ch7_data.Rdata")
## 


train <- data
train <- subset(train, frags > 0 & frags < 100, select = - c(pedon_id, taxonname, landform.string, x_std, y_std, argillic.horizon, describer2)) # exclude frags greater than 100 and less than 1, and exclude some of the extra columns

# Create custom transform functions
logit <- function(x) log(x / (1 - x)) # logit transform
ilogit <- function(x) exp(x) / (1 + exp(x)) # inverse logit transform

# Transform
train$fragst <- logit(train$frags / 100)

# Create list of predictor names
terrain1 <- c("slope", "solar", "mrrtf", "mrvbf")
terrain2 <- c("twi", "z2str", "ch")
climate <- c("elev", "precip", "precipsum", "temp")
ls <- paste0("ls_", 1:6)
pc <- paste0("pc_", 1:6)
tc <- paste0("tc_", 1:3)
rad <- c("k", "th", "u")

# Compute correlation matrices
round(cor(train[c("fragst", terrain1)], use = "pairwise"), 2)
round(cor(train[c("fragst", terrain2)], use = "pairwise"), 2)
round(cor(train[c("fragst", climate)], use = "pairwise"), 2)
round(cor(train[c("fragst", ls)], use = "pairwise"), 2)
round(cor(train[c("fragst", pc)], use = "pairwise"), 2)
round(cor(train[c("fragst", tc)], use = "pairwise"), 2)
round(cor(train[c("fragst", rad)], use = "pairwise"), 2)

# Create scatterplots
# ggpairs(train[c("fragst", terrain1)])
ggpairs(train[c("fragst", terrain2)])
ggpairs(train[c("fragst", climate)])

ggpairs(train[c("fragst", ls)])
ggpairs(train[c("fragst", pc)])
# ggpairs(train[c("fragst", tc)])
# ggpairs(train[c("fragst", rad)])


# Create boxplots
ggplot(train, aes(x = cluster, y = frags)) + geom_boxplot()
ggplot(train, aes(x = cluster2, y = frags)) + geom_boxplot()


# Subset training dataset
train <- train[c("fragst", "frags", "cluster2", terrain1, terrain2, climate, pc, tc, rad)]

# Fit full model
full <- lm(fragst ~ . - frags, data = train) # "~ ." includes all columns in the data set, "-" removes variables

# Fit null model
null <- lm(fragst ~ 1, data = train) # "~ 1" just includes an intercept

# Compute AIC
add1(null, full)

## # add one or several variables to the model
## fragst_lm <- update(null, . ~ . + pc_2)
## 
## # or refit
## fragst_lm <- lm(fragst ~ pc_2, data = train)
## 
## # iterate until the model is saturated
## add1(fragst_lm, full, test = "F")

# Fit final model
fragst_lm <- lm(fragst ~ pc_2 + pc_1 + temp + twi + precipsum, data = train)

# Compute AIC
drop1(fragst_lm, test = "F")

summary(fragst_lm)

# Standard diagnostic plots for lm() objects
par(mfrow = c(2, 2))
plot(fragst_lm)

# Term and partial residual plots
termplot(fragst_lm, partial.resid = TRUE)


# vif() function from the car or rms packages
sqrt(car::vif(fragst_lm))

# or 

sqrt(car::vif(fragst_lm)) > 2



# Adjusted R2
summary(fragst_lm)$adj.r.squared

# Generate predictions
train$predict <- ilogit(predict(fragst_lm, train)) * 100 # apply reverse transform

# Root mean square error (RMSE)
with(train, sqrt(mean((frags - predict)^2, na.rm = T)))

# Mean absolute error
with(train, mean(abs(frags - predict), na.rm = T))

# Plot the observed vs predicted values
plot(train$frags, train$predict, xlim = c(0, 100), ylim = c(0, 100))
abline(0, 1)

sum(train$frags < 15)

sum(train$frags > 80)

# Examine the RMSE for each cluster
temp <- group_by(train, cluster2) %>%
  summarize(
    rmse = round(sqrt(mean((frags - predict)^2, na.rm = T))), 
    n = length(frags)
  )

ggplot(temp, aes(x = cluster2, y = rmse)) + 
  geom_point()

# fragst_lm <- update(null, . ~ . + pc_2 + pc_1 + temp + twi + precipsum + cluster) # add one or several variables to the model

# Examine the coefficients
summary(fragst_lm)

ilogit(fragst_lm$coefficients) * 100

anova(fragst_lm) # importance of each predictor assess by the amount of variance they explain


# Custom function to return the predictions and their standard errors
predfun <- function(model, data) {
  v <- predict(model, data, se.fit = TRUE)
  cbind(
    p = as.vector(ilogit(v$fit) * 100),
    se = as.vector(ilogit(v$se.fit)) * 100)
  }

# Generate spatial predictions
# r <- predict(geodata_r, fragst_lm, fun = predfun, index = 1:2, progress = "text")

# Export the results
# writeRaster(r$layer.1, filename = "C:/workspace2/frags.tif", overwrite = TRUE, progress = "text")
# writeRaster(r$layer.2, filename = "C:/workspace2/frags_se.tif", overwrite = TRUE, progress = "text")

plot(raster("C:/workspace2/frags.tif"))
plot(ca794, add = TRUE)

plot(raster("C:/workspace2/frags_se.tif"))
plot(ca794, add = TRUE)


## 
## # Download clipped example from Pinto Basin Joshua Tree
## githubURL <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/frags_pb.zip"
## download.file(githubURL, destfile = "C:/workspace/frags_pb.zip")
## unzip(zipfile="C:/workspace/frags_pb.zip", exdir="C:/workspace")
## 
