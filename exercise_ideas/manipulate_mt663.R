library(aqp)
library(soilDB)

# # populate selected set with 2015MT663% and use fetchNASIS()
# mt663 <- fetchNASIS()
# mt663err <- fetchNASIS(rmHzErrors = FALSE)

# or download the same data from the course page
example.data.dir <- "C:/workspace2"
example.data.path <- file.path(example.data.dir, "mt663.zip")

if(!dir.exists(example.data.dir))
  dir.create(example.data.dir, recursive = TRUE)

download.file("http://ncss-tech.github.io/stats_for_soil_survey/data/book/02/mt663.zip", 
              destfile = example.data.path)
unzip(example.data.path, exdir = example.data.dir, overwrite = TRUE)
load("C:/workspace2/mt663.rda")

## rmHzErrors == TRUE (DEFAULT)
length(mt663)

site(mt663)$ochric.epipedon
sum(!is.na(site(mt663)$ochric.epipedon))

## rmHzErrors == FALSE
length(mt663err)

site(mt663err)$ochric.epipedon
sum(site(mt663err)$ochric.epipedon)

# Geometrically valid
checkHzDepthLogic(mt663err)


