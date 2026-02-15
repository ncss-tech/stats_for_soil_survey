### MU GIS Summary Report
### 2018-05-29
### D.E. Beaudette and J. Wood
###
### configuration file, edit as needed
###

#########################
### Raster Data Sources #
#########################

# data sources can be "commented-out" using the "#" character
# be sure that there is no trailing "," after the last item in each list
# raster summaries are displayed in the same order in which they are listed below

raster.list <- list(
  continuous=list(
    `Mean Annual Air Temperature (degrees C)`='data/crop_final_MAAT_800m.tif',
    `Mean Annual Precipitation (mm)`='data/crop_final_MAP_mm_800m.tif',
    `Effective Precipitation (mm)`='data/crop_effective_precipitation_800m.tif',
    `Frost-Free Days`='data/crop_ffd_50_pct_800m.tif',
    `Growing Degree Days (degrees C)`='data/crop_gdd_mean_800m.tif',
    `Elevation (m)`='data/crop_DEM_30m_SSR2.tif',
    `Slope Gradient (%)`='data/crop_Slope_30m_SSR2.tif',
    # `Annual Beam Radiance (MJ/sq.m)`='data/crop_ssro2_ann_beam_rad_int.tif',
    # `Compound Topographic Index`='data/crop_ssro2_tci_int.tif',
    # `SAGA TWI`='data/crop_ssro2_saga_twi_int.tif',
    `NLCD Impervious Surface (%))`='data/crop_nlcd_impervious_2011_cropped.tif'
  ),
  categorical=list(
    `Geomorphon Landforms`='data/crop_forms30_region2.tif',
    `Curvature Classes`='data/crop_curvature_classes_30_class_region2.tif',
    `NLCD (2011)`='data/crop_nlcd_2011_cropped.tif',
    `NASS Cropland Data Layer (2017)`='data/crop_Ver2017_30m_cdls_clip.img'
  ),
  circular=list(
    `Slope Aspect (degrees)`='data/crop_Aspect_30m_SSR2.tif'
  )
)

###################
### Map unit data #
###################

##
## Data must be in a projected coordinate system, with units of meters!
##

##
## Geodatabase with many map units, explicit subsetting
## consider sub-setting to SHP if the geodatabase contains more than 2-3 soil survey areas
##

# geodatabase path
# mu.dsn <- 'E:/gis_data/ca630/FG_CA630_OFFICIAL.gdb'
# name of featureclass
# mu.layer <- 'ca630_a'
# map unit symbols / keys to extract
mu.set <- c('5012','3046','7083',"7085","7088")



##
## Typical SDJR style data: SHP with multiple map units
##

# path to parent folder of SHP, no trailing forward slash (/)
mu.dsn <- 'data'
# SHP name, without file extension
mu.layer <- 'ca630_clip'


############################################
### column with map unit ID / key / symbol #
############################################

# could be 'MUKEY', 'MUSYM', or any valid column name
mu.col <- 'MUSYM'


#########################################################
### polygon sampling density (samples / acre / polygon) #
#########################################################

# consider using a sampling density between 1-2 points / ac.
# increase if there are un-sampled polygons
# delineations smaller than 5 ac. may require up to 5 points / ac.
# values > 6-7 points / ac. will only slow things down
pts.per.acre <- 3

###########################
### quantiles of interest #
###########################

# the most important quantiles (percentiles / 100) are: 0.1, 0.5 (median), and 0.9
# optionally reduce the number of quantiles for narrower tables
p.quantiles <- c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1)


##################################
### scale density plots to {0,1} #
##################################

# typically scaling density curves to the interval of {0,1} is helpful: patterns are more clear
# this can cause problems when comparing map units of drastically different areas
# in that case, it might be useful to disable scaling
scaleDensityCurves <- TRUE


#####################################################################################
### output file names (OPTIONAL; uncomment to override defaults)                    #
### default will include a file-specific prefix and full list of MUSYMs summarized  #
### do not include .shp extension for shapefiles; automatically added by writeOGR() #
#####################################################################################
# shp.unsampled.fname <- 'un-sampled-polygons'# shapefile containing any unsampled polygons (usually too small or odd shape)
# shp.stats.fname <- 'polygons-with-stats' # shapefile containing median values / most likely classes by delineation
# shp.qc.fname <- 'poly-qc' # shapefile containing "proportion of samples outside 5-95% quantile range" by delineation

# csv.stats.fname <- 'poly-stats.csv' # comma-separated value file containing median values / most likely classes by delineation
# csv.qc.fname <- 'poly-qc.csv' # comma-separated value file containing "proportion of samples outside 5-95% quantile range" by delineation

########################################################
### Add estimate of confidence to box and whisker plots ###
########################################################

#enabling this feature will double the run time 
#enabling this feature will add "notches" to box and whisker plots
# that are close approximations to a confidence interval around the median
# adjusted for spatial autocorrelation
correct.sample.size <- FALSE


###########################################
### save samples after report has run ? ###
###########################################

# used for tinkering with a report .Rmd and debugging
# this will save samples to a file and subsequent report runs will use the saved samples
# not recommended for routine operation
cache.samples <- FALSE

