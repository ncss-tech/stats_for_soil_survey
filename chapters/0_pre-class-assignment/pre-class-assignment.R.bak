## source('https://raw.githubusercontent.com/ncss-tech/soilReports/master/R/installRprofile.R')
## installRprofile(overwrite=TRUE)

## # helper fuction for installing required packages from CRAN
## # a simple check is done to see if each is already installed
## # p: vector of package names
## # up: logical- upgrade installed packages?
## ipkCRAN <- function(p, up){
##   if (up) {
##     install.packages(p, dependencies = TRUE)
##   } else {
##     new.pkg <- p[! (p %in% installed.packages()[, "Package"])]
##     if (length(new.pkg) > 0) {
##       message('installing packages from CRAN...')
##       install.packages(new.pkg, dependencies = TRUE)
##     }
##   }
## }
## 
## 
## ## list of packages
## packages <- c(
##   # soil
##   "aqp", "soilDB", "sharpshootR", "soiltexture",
##   # gis
##   "sp", "sf", "raster", "rgdal", "gdalUtils", "RSAGA", "rgrass7", "velox",
##   # data management
##   "tidyverse", "devtools", "roxygen2", "Hmisc", "RODBC", "circular", "DT", "remotes",
##   # graphics
##   "latticeExtra", "maps", "spData", "mapview", "plotrix", "rpart.plot",
##   # modeling
##   "car", "rms", "randomForest", "party", "caret", "vegan", "ape", "shape",
##   # sampling
##   "clhs"
##   # graphical user interface: not available to R 3.4.0
##   # "Rcmdr"
##   )
## 
## ## install packages, upgrading as needed
## ipkCRAN(packages, up=TRUE)

## # install previous version of latticeExtra until we are at R >= 3.6.0
## install.packages('https://cran.r-project.org/src/contrib/Archive/latticeExtra/latticeExtra_0.6-28.tar.gz', repos = NULL)
## 
## # temporary CRAN fix - get previous version of caTools
## install.packages('https://cran.microsoft.com/snapshot/2019-04-15/bin/windows/contrib/3.6/caTools_1.17.1.2.zip', repos = NULL)

## remotes::install_github("ncss-tech/aqp", dependencies=FALSE, upgrade=FALSE, build=FALSE)
## remotes::install_github("ncss-tech/soilDB", dependencies=FALSE, upgrade=FALSE, build=FALSE)
## remotes::install_github("ncss-tech/soilReports", dependencies=FALSE, upgrade=FALSE, build=FALSE)
## remotes::install_github("ncss-tech/sharpshootR", dependencies=FALSE, upgrade=FALSE, build=FALSE)

## # load packages into the current session
## library(aqp) # provides "SoilProfileCollection" object & more
## library(soilDB) # provides database access methods
## 
## # get pedons from NASIS selected set
## test <- fetchNASIS(from = 'pedons')
## 
## # inspect the result
## str(test, max.level = 2)
## 
## # make a profile plot
## 
## # set margins smaller than default
## par(mar=c(1,1,1,1))
## 
## # make profile plot of selected set, with userpedonid as label
## plot(test, label='pedon_id')

## # dump list of packages that are loaded into the current session
## sessionInfo()
