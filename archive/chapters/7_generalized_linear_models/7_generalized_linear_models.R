# setup
knitr::opts_chunk$set(message=FALSE, warning=FALSE, tidy=FALSE, verbose=FALSE, background='#F7F7F7', fig.align='center', fig.retina=2, dev='png', antialias='cleartype', cache=FALSE)

p <- sort(c(rbinom(100, 1:100, 1)) / 100)
logp <- log(p / (1 - p))
test <- data.frame(p, logp)[!is.infinite(logp), ]
fit <- lm(p ~ logp, data = test)

plot(logp, p, type = "l", ylab = "proportion (p)", xlab = "logit transform (log(p / (1 - p)))", ylim = c(0, 1), las=1)

library(sp)

data(meuse)
meuse <- meuse[order(meuse$dist.m), ]
meuse$lime <- ifelse(as.numeric(meuse$lime) == 2, 1, 0)

lm_fit <- lm(as.numeric(lime) ~ dist.m, data = meuse)
glm_fit <- glm(lime ~ dist.m, data = meuse, family = binomial())

par(mfrow = c(1, 3))
    plot(meuse$dist.m, lm_fit$fitted.values, type = "l", ylim = c(0, 1), ylab = "linear fit", xlab = "predictor", las=1)
    with(meuse, points(dist.m, lime))
    plot(meuse$dist.m, glm_fit$fitted.values, type = "l", ylim = c(0, 1), ylab = "logistic fit", xlab = "predictor", las=1)
    with(meuse, points(dist.m, lime))
    boxplot(dist.m ~ lime, data = meuse, ylab = "predictor", xlab = "response", col = "grey", las=1)

library(aqp)     # specialized soil classes and functions
library(soilDB)  # NASIS and SDA import functions
library(raster)  # guess
library(rgdal)   # spatial import
library(ggplot2) # graphing
library(tidyr)   # data manipulation
library(caret)   # classification and regression training
library(car)     # additional regression tools


# pedons <- fetchNASIS()
githubURL <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/ch7_data.Rdata"
load(url(githubURL))

# Examine the makeup of the data we imported from NASIS
str(pedons, max.level = 2)



# Check consistency of argillic horizon population

# get the site table
s <- site(pedons) 

# tabulate the number of argillic horizons observed
table(s$argillic.horizon, useNA = "ifany") 

# or

# summary(s$argillic.horizon) 

# Extract argillic presence from the taxonomic subgroup
s$argillic <- grepl("arg", s$tax_subgroup)

table(s$argillic, useNA = "ifany")



d <- diagnostic_hz(pedons)
peiid <- subset(d, diag_kind == "argillic horizon" & featdept < 50, select = peiid)
test <- s$peiid %in% unique(peiid)
summary(test)



# Landform vs argillic presence

# Subset
s_sub <- subset(s, argillic == TRUE)

# Cross tabulate landform vs argillic horizon presence
test <- with(s_sub, 
             table(landform.string, argillic, useNA = "ifany")
             )
# Subset and print landform.string with > 3 observations
test[test > 3,]

# generalize the landform.string
s$landform <- ifelse(grepl("fan|terrace|sheet|drainageway|wash", s$landform.string), "fan", "hill") 



# Hillslope position

# Subset fan landforms
s_sub <- subset(s, landform == "fan") 

# Cross tabulate and calculate proportions, the "2" calculates the proportions relative to the column totals
with(s_sub, round(
  prop.table(table(hillslope_pos, argillic, useNA = "ifany"), 2)
  * 100)
  ) 

# Slope shape
with(s_sub, round(
  prop.table(table(paste(shapedown, shapeacross), argillic, useNA = "ifany"), 2)
  * 100)
  )



# Surface morphometry, depth and surface rock fragments

# Recalculate gravel
s$surface_gravel <- with(s, 
                         surface_gravel - surface_fgravel
                         )
# Calculate the total surface rock fragments
s$frags <- apply(s[grepl("surface", names(s))], 1, sum) 

# Subset to just look and fans, and select numeric columns
s_sub <- subset(s, landform == "fan", select = c(argillic, bedrckdepth, slope_field, elev_field, frags)) 

# convert s_sub to wide data format
s_w <- gather(s_sub, key = key, value = value, - argillic) 
head(s_w, 2)

ggplot(s_w, aes(x = argillic, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scale = "free")



# Custom function to filter out the top 3 soil scientists
s <- within(s, {
  old = describer
  describer2 = NA
  describer2[grepl("Stephen", old)] = "Stephen" # least senior
  describer2[grepl("Paul",    old)] = "Paul"
  describer2[grepl("Peter",   old)] = "Peter"   # most senior
  })



s_sub <- subset(s, landform == "fan")

# By frequency
with(s_sub, table(describer2, argillic, useNA = "ifany"))

# By proportion
with(s_sub, round(
  prop.table(table(describer2, argillic), margin = 1)
  * 100)
  )



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
# ssa <- readOGR(dsn = "F:/geodata/soils/soilsa_a_nrcs.shp", layer = "soilsa_a_nrcs")
# ca794 <- subset(ssa, areasymbol == "CA794") # subset out Joshua Tree National Park
# ca794 <- spTransform(ca794, CRS("+init=epsg:5070"))

# Plot
plot(ca794, axes = TRUE)
plot(pedons_sp, add = TRUE) # notice the points outside the boundary

# Write shapefile of pedons
writeOGR(pedons_sp, dsn = "C:/workspace2", "pedons_sp", driver = "ESRI Shapefile", overwrite_layer = TRUE)


## 
## # set file path
## folder <- "D:/geodata/project_data/R8-VIC/ca794/"
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
##   twi_sc   = abs(twi - 13.8) # 13.8 = twi median
##   })
## 
## # save(data, ca794, pedons, file = "C:/workspace2/ch7_data.Rdata")
## 
## # Strip out location and personal information before uploading to the internet
## # s[c("describer", "describer2", "x", "y", "x_std", "y_std", "utmnorthing", "utmeasting", "classifier")] <- NA
## # slot(pedons, "site") <- s
## # data[c("describer2", "x_std", "y_std")] <- NA
## # save(data, ca794, pedons, file = "C:/workspace2/stats_for_soil_survey/trunk/data/ch7_data.Rdata")
## 


# Load data
load(file = "C:/workspace2/github/stats_for_soil_survey/trunk/data/ch7_data.Rdata")
train <- data

# Select argillic horizons with "arg" in the subgroup name and on fans
# Argillic horizons that occur on hills and mountains more than likely form by different process, and therefore would require a different model.train$argillic 
train$argillic <- ifelse(grepl("arg", train$tax_subgroup) & 
                           train$mrvbf > 0.15,
                         TRUE, FALSE
                         )
train <- subset(train, !is.na(argillic), select = - c(pedon_id, taxonname, x_std, y_std, landform.string, cluster, cluster_mast, argillic.horizon, tax_subgroup, frags)) 

train2 <- subset(train, select = - c(describer2, landform, cluster2))
data_m <- gather(train2, key = key, value = value, - argillic)

ggplot(data_m, aes(x = argillic, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")



# Fit full model
full <- glm(argillic ~ ., data = train, family = binomial) # "~ ." includes all columns in the data set

# Fit null model, 
null <- glm(argillic ~ 1, data = train, family = binomial) # "~ 1" just includes an intercept

# Compute AIC
add1(null, full)


## # add twi_sc to the model, "-" will subtract predictors
## argi.glm <- update(null, . ~ . + twi_sc)
## 
## # or refit
## argi.glm <- glm(argillic ~ twi_sc, data = train, family = binomial)
## 
## # iterate until the model is saturated
## add1(argi.glm, full)

argi.glm <- glm(argillic ~ twi_sc + slope + ls_1 + ch + z2str + mrvbf, data = train, family = binomial)

# Compute AIC
drop1(argi.glm, test = "Chisq") 

# Examine the effect and error for each predictors
summary(argi.glm) 

# Convert the coefficients to an odds scale, who here gambles?
exp(coef(argi.glm))

# Importance of each predictor assessed by the amount of deviance they explain
anova(argi.glm) 



# Residual Plots for GLM
par(mfrow = c(2, 2))
plot(argi.glm)

residualPlots(argi.glm, fit = FALSE, tests = FALSE)



# Variance inflation, greater than 5 or 10 is bad
vif(argi.glm)


# examine possible thresholds
train$predict <- predict(argi.glm, train, type = "response")

ggplot(train, aes(x = predict, fill = argillic)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = 0.5), lty = "dashed") +
  xlab("probability") +
  scale_x_continuous(breaks = seq(0, 1, 0.2))

train$predict <- train$predict > 0.35

# Confusion Matrix
cm <- table(predicted = train$predict, observed = train$argillic)
confusionMatrix(cm, positive = "TRUE")

# Deviance squared
library(modEvA)
Dsquared(argi.glm)

# Adjusted deviance squared
Dsquared(argi.glm, adjust = TRUE)



library(dplyr)

temp <- subset(train, argillic == TRUE) %>%
  group_by(cluster2) %>%
  summarize(
    sum_arg = sum(argillic, na.rm = TRUE),
    sum_pred = sum(predict, na.rm = TRUE),
    sensitivity = round(sum(predict == argillic) / length(argillic), 2)
    )

ggplot(temp, aes(x = cluster2, y = sensitivity)) +
  geom_point()

# Remove outlier clusters         
train_sub <- subset(train, ! cluster2 %in% c(12, 7))

# full <- glm(argillic ~ ., data = train_sub, family = binomial)
# null <- glm(argillic ~ 1, data = train_sub, family = binomial)
# add1(null, full, train = "Chisq")

sub.glm <- glm(argillic ~ slope + twi_sc + ls_1 + mrvbf + z2str + ch, data = train_sub, family = binomial)

# summary(sub.glm)

train_sub$predict <- predict(sub.glm, train_sub, type = "response") > 0.35
cm <- table(predicted = train_sub$predict, observed = train_sub$argillic)
confusionMatrix(cm, positive = "TRUE")

temp <- subset(train_sub, argillic == TRUE) %>%
  group_by(cluster2) %>%
  summarize(
    sum_arg  = sum(argillic, na.rm = TRUE),
    sum_pred = sum(predict, na.rm = TRUE),
    sensitivity = round(sum(predict == argillic) / length(argillic), 2)
    )

ggplot(temp, aes(x = cluster2, y = sensitivity)) +
  geom_point()


## # Custom function to return the predictions and their standard errors
## predfun <- function(model, data) {
##   v <- predict(model, data, type = "response", se.fit = TRUE)
##   cbind(
##     p = as.vector(v$fit),
##     se = as.vector(v$se.fit)
##     )
##   }
## 
## # Generate spatial predictions
## r <- predict(geodata_r, argi.glm, fun = predfun, index = 1:2, progress = "text")
## 
## # Export the results
## writeRaster(r[[1]], "argi.tif", overwrite = T, progress = "text")
## writeRaster(r[[2]], "argi_se.tif", overwrite = T, progress = "text")

plot(raster("C:/workspace2/argi.tif"))
plot(ca794, add = TRUE)
plot(raster("C:/workspace2/argi_se.tif"))
plot(ca794, add = TRUE)

## # Download clipped example from Pinto Basin Joshua Tree
## githubURL <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/logistic/argi_pb.zip"
## download.file(githubURL, destfile = "C:/workspace2/argi_pb.zip")
## unzip(zipfile="C:/workspace2/argi_pb.zip", exdir="C:/workspace2")
