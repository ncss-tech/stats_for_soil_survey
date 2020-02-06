library(aqp)
library(soilDB)

# Load from the the loakercreek dataset
data("loafercreek") 

# Construct generalized horizon designations
n <- c("A",
       "BAt",
       "Bt1",
       "Bt2",
       "Cr",
       "R")
# REGEX rules
p <- c("A",
       "BA|AB",
       "Bt|Bw",
       "Bt3|Bt4|2B|C",
       "Cr",
       "R")

# Compute genhz labels and add to loafercreek dataset
loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

# Extract the horizon table
h <- horizons(loafercreek) 

# Examine the matching of pairing of the genhz label to the hznames
table(h$genhz, h$hzname)

## 
## View(h)
## 


vars <- c("genhz", "clay", "total_frags_pct", "phfield", "effclass")
summary(h[, vars])


# just for factors
levels(h$genhz)

# for characters and factors
sort(unique(h$hzname)) 

## h$hzname <- ifelse(h$hzname == "BT", "Bt", h$hzname)
## 
## # or
## 
## h$hzname[h$hzname == "BT"] <- "Bt"
## 
## # or as a last resort we could manually edit the spreadsheet in R
## 
## edit(h)

# gopheridge rules
n <- c('A', 'Bt1', 'Bt2', 'Bt3','Cr','R')
p <- c('^A|BA$', 'Bt1|Bw','Bt$|Bt2', 'Bt3|CBt$|BCt','Cr','R')

desc <- data.frame(
  Parameter = c("Mean", "Median", "Mode", "Standard Deviation", "Quantiles"),
  NASIS = c("RV ?", "RV", "RV", "L & H ?", "L & H"),
  Description = c("arithmetic average", 
                  "middle value, 50% quantile", 
                  "most frequent value", "variation around mean", 
                  "percent rank of values, such that all values are <= p"
                  ),
  'R function' = c("mean()", "median()", "sort(table(), decreasing = TRUE)[1]", "sd()", "quantile()"),
  check.names = FALSE
  )
knitr::kable(desc, caption = "Short Description of Descriptive Statistics and R Functions")

# first remove missing values and create a new vector
clay <- na.exclude(h$clay)
mean(clay)

# or use the additional na.rm argument
mean(h$clay, na.rm = TRUE)

median(clay)

sort(table(round(h$clay)), decreasing = TRUE)[1] # sort and select the 1st value, which will be the mode

table(h$genhz)

# or

# summary(h$genhz)

## table(h$genhz, h$texcl)

knitr::kable(table(h$genhz, h$texcl))

## # append the table with row and column sums
## addmargins(table(h$genhz, h$texcl))
## 
## # calculate the proportions relative to the rows, margin = 1 calculates for rows, margin = 2 calculates for columns, margin = NULL calculates for total observations
## round(prop.table(table(h$genhz, h$texture_class), margin = 1) * 100)

knitr::kable(addmargins(table(h$genhz, h$texcl)))
knitr::kable(round(prop.table(table(h$genhz, h$texture_class), margin = 1) * 100))

aggregate(clay ~ genhz, data = h, mean)

aggregate(clay ~ genhz, data = h, median)

# or we could use the summary() function to get both the mean and median
aggregate(clay ~ genhz, data = h, summary)

var(h$clay, na.rm=TRUE)

sd(h$clay, na.rm = TRUE)

# or

# sqrt(var(clay))

cv <- sd(clay) / mean(clay) * 100
cv

quantile(clay)

# or

quantile(clay, c(0.05, 0.5, 0.95))

range(clay)

diff(range(clay))

# or

# max(clay) - min(clay)

IQR(clay)

# or

# diff(quantile(clay, c(0.25, 0.75)))

h$hzdepm <- (h$hzdepb + h$hzdept) / 2 # Compute the middle horizon depth

vars <- c("hzdepm", "clay", "sand", "total_frags_pct", "phfield")

round(cor(h[, vars], use = "complete.obs"), 2)





data(metadata)

# Generalized the horizon designations
n <- c("A",
       "BAt",
       "Bt1",
       "Bt2",
       "Cr",
       "R")
# REGEX rules
p <- c("A",
       "BA|AB",
       "Bt|Bw",
       "Bt3|Bt4|2B|C",
       "Cr",
       "R")

loafercreek$genhz <- generalize.hz(loafercreek$hzname, n, p)

h <- horizons(loafercreek)
data("metadata")
h <- within(h, {
  hzdepm = (hzdepb + hzdept) / 2
  texture_class = factor(texture_class, 
                         levels = metadata[metadata$ColumnPhysicalName == "texcl", "ChoiceName"]
                         )
  })

idx <- is.na(h$texture_class)

library(ggplot2)

# bar plot
p_b <- ggplot(h[!idx, ], aes(x = texture_class)) +
  geom_bar() +
  ylab("count (n)") + xlab("texture class") +
  theme(aspect.ratio = 1) +
  ggtitle("Bar Plot")

# histogram
p <- ggplot(data = h, aes(x = clay))

p_h <- p + 
  geom_histogram(bins = nclass.Sturges(h$clay)) + 
  xlab("clay (%)") + ylab("count (n)") +
  theme(aspect.ratio = 1) +
  ggtitle("Histogram")

# density plot
p_d <- p + 
  geom_density(fill = "grey", alpha = 0.5) + 
  xlab("clay (%)") + ylab("density (p)") +
  theme(aspect.ratio = 1) +
  ggtitle("Density Plot")

gridExtra::grid.arrange(p_b, p_h, p_d, ncol = 3)


library(ggplot2)

# bar plot
ggplot(h, aes(x = texcl)) +
  geom_bar()


ggplot(h, aes(x = clay)) +
  geom_histogram(bins = nclass.Sturges(h$clay))



ggplot(h, aes(x = clay)) +
  geom_density()



ggplot(h, (aes(x = genhz, y = clay))) +
  geom_boxplot()



# QQ Plot for Clay
ggplot(h, aes(sample = clay)) + 
  geom_qq() +
  geom_qq_line()

# QQ Plot for Frags
ggplot(h, aes(sample = total_frags_pct)) + 
  geom_qq() +
  geom_qq_line()










# scatter plot
ggplot(h, aes(x = clay, y = hzdepm)) +
  geom_point() +
  ylim(100, 0)

# line plot
ggplot(h, aes(y = clay, x = hzdepm, group = peiid)) +
  geom_line() +
  coord_flip() +
  xlim(100, 0)


# Load the GGally package
library(GGally)

# Create a scatter plot matrix
vars <- c("hzdepm", "clay", "phfield", "total_frags_pct")

ggpairs(h[vars])





# scatter plot
ggplot(h, aes(x = clay, y = hzdepm, color = genhz)) +
  geom_point(size = 3) +
  ylim(100, 0)

# density plot
ggplot(h, aes(x = clay, color = genhz)) +
  geom_density(size = 2)

# bar plot
ggplot(h, aes(x = genhz, fill = texture_class)) +
  geom_bar()

# box plot
ggplot(h, aes(x = genhz, y = clay)) + 
  geom_boxplot()

# heat map (pseudo bar plot)
s <- site(loafercreek)

ggplot(s, aes(x = landform_string, y = pmkind)) + 
  geom_tile(alpha = 0.2) 
  


# convert to long format
df <- reshape2::melt(h, 
                     id.vars = c("peiid", "genhz", "hzdepm"), 
                     measure.vars = c("clay", "phfield", "total_frags_pct")
                     )

ggplot(df, aes(x = genhz, y = value)) +
  geom_boxplot() +
  xlab("genhz") +
  facet_wrap(~ variable, scales = "free_y")


library(aqp)

s <- slice(loafercreek, 0:100 ~ clay + phfield + total_frags_pct)
s <- slab(s, fm = ~ clay + phfield + total_frags_pct, 
          slab.fun = function(x) quantile(x, c(0.1, 0.5, 0.9), na.rm = TRUE)
          )

ggplot(s, aes(x = top, y = X50.)) +
  geom_line() +
  geom_ribbon(aes(ymin = X10., ymax = X90., x = top), alpha = 0.2) +
  xlim(c(100, 0)) +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_x")


# arithmetic mean
log10(mean(1/10^-h$phfield, na.rm = TRUE)) 

# geometric mean
mean(h$phfield, na.rm = TRUE) 

library(circular)

# Extract the site table
s <- site(loafercreek) 

aspect <- s$aspect_field
aspect <- circular(aspect, template="geographic", units="degrees", modulo="2pi")

summary(aspect)

rose.diag(aspect, bins = 8, col="grey")

## # store path as a variable, in case you want to keep it somewhere else
## ch2b.data.path <- 'C:/workspace/chapter-2b'
## 
## # make a place to store chapter 2b example data
## dir.create(ch2b.data.path, recursive = TRUE)
## 
## # download example data from github
## # polygons
## download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/chapter_2b-spatial-data/chapter-2b-mu-polygons.zip', paste0(ch2b.data.path, '/chapter-2b-mu-polygons.zip'))
## 
## # raster data
## download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/chapter_2b-spatial-data/chapter-2b-PRISM.zip', paste0(ch2b.data.path, '/chapter-2b-PRISM.zip'))
## 
## # unzip
## unzip(paste0(ch2b.data.path, '/chapter-2b-mu-polygons.zip'), exdir = ch2b.data.path, overwrite = TRUE)
## unzip(paste0(ch2b.data.path, '/chapter-2b-PRISM.zip'), exdir = ch2b.data.path, overwrite = TRUE)

library(aqp)
library(sp)
library(raster)
library(rgdal)
library(soilDB)

# establish path to example data
ch2b.data.path <- 'C:/workspace/chapter-2b'

# load MLRA polygons
mlra <- readOGR(dsn=ch2b.data.path, layer='mlra-18-15-AEA')

# mean annual air temperature, Deg C
maat <- raster(paste0(ch2b.data.path, '/MAAT.tif'))
# mean annual precipitation, mm
map <- raster(paste0(ch2b.data.path, '/MAP.tif'))
# frost-free days
ffd <- raster(paste0(ch2b.data.path, '/FFD.tif'))
# growing degree days
gdd <- raster(paste0(ch2b.data.path, '/GDD.tif'))
# percent of annual PPT as rain
rain_fraction <- raster(paste0(ch2b.data.path, '/rain_fraction.tif'))
# annual sum of monthly PPT - ET_p
ppt_eff <- raster(paste0(ch2b.data.path, '/effective_preciptitation.tif'))

rs <- stack(maat, map, ffd, gdd, rain_fraction, ppt_eff)
# reset layer names
names(rs) <- c('MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT')

amador <- seriesExtent(s = 'amador')
class(amador)

s <- spsample(amador, n = 100, type = 'hexagonal')

# result is a data.frame
mukeys <- SDA_query("
                    SELECT DISTINCT mukey 
                    FROM component 
                    WHERE compname = 'Amador' 
                    AND majcompflag = 'Yes' ;"
)

# result is a SpatialPointsDataFrame
amador.pts <- fetchSDA_spatial(mukeys$mukey, by.col = 'mukey', method = 'point', chunk.size = 2)

# adjust margins and setup plot device for two columns
par(mar=c(1,1,3,1), mfcol=c(1,2))
# first figure
plot(maat, ext=extent(s), main='PRISM MAAT\n100 Sampling Points from Extent', axes=FALSE)
plot(amador, add=TRUE)
points(s, cex=0.25)

plot(maat, ext=extent(s), main='PRISM MAAT\nPolygon Centroids', axes=FALSE)
points(amador.pts, cex=0.25, pch='.')

# return the result as a data.frame object
e <- extract(rs, s, df=TRUE)
e.pts <- extract(rs, amador.pts, df=TRUE)

# check out the extracted data
summary(e[, -1])

table(mlra$MLRARSYM)

poly.area <- round(sapply(mlra@polygons, slot, 'area') * 0.000247105)
summary(poly.area)
sum(poly.area)

library(sharpshootR)

# the next function requires a polygon ID: each polygon gets a unique number 1--number of polygons
mlra$pID <- 1:nrow(mlra)
s <- constantDensitySampling(mlra, n.pts.per.ac=0.001)

# spatial overlay: sampling points and MLRA polygons
res <- over(s, mlra)

# row / feature order is preserved, so we can directly copy
s$mlra <- res$MLRARSYM

# tabulate number of samples per MLRA
table(s$mlra)

# raster stack extraction at sampling points
e <- extract(rs, s, df=TRUE)

# convert sampling points from SpatialPointsDataFrame to data.frame
s.df <- as(s, 'data.frame')

# join columns from extracted values and sampling points
s.df <- cbind(s.df, e)

# check results
head(s.df)

library(lattice)
library(reshape2)

# reshape from wide to long format
m <- melt(s.df, id.vars = c('mlra'), measure.vars = c('MAAT', 'MAP', 'FFD', 'GDD', 'rain.fraction', 'eff.PPT'))

# check "wide" format
head(m)

# tabular summary of mean values
tapply(m$value, list(m$mlra, m$variable), mean)

tps <- list(box.rectangle=list(col='black'), box.umbrella=list(col='black', lty=1), box.dot=list(cex=0.75), plot.symbol=list(col=rgb(0.1, 0.1, 0.1, alpha = 0.25, maxColorValue = 1), cex=0.25))


bwplot(mlra ~ value | variable, data=m,                 # setup plot and data source
       as.table=TRUE,                                   # start panels in top/left corner
       varwidth=TRUE,                                   # scale width of box by number of obs
       scales=list(alternating=3, relation='free'),     # setup scales
       strip=strip.custom(bg=grey(0.9)),                # styling for strips
       par.settings=tps,                                # apply box/line/point styling
       panel=function(...) {                            # within in panel, do the following
          panel.grid(-1, -1)                            # make grid lines at all tick marks
          panel.bwplot(...)                             # make box-whisker plot
      }
)

## # Install the soilReports package from GitHub
## remotes::install_github("ncss-tech/soilReports", dependencies=FALSE, build=FALSE)

## # Load the soilReports and rmarkdown package
## library(soilReports)
## library(rmarkdown)
## 
## # List reports
## listReports()

## # Copy report to your directory
## copyReport(reportName = "region11/lab_summary_by_taxonname", outputDir = "C:/workspace2/lab_sum")

## # Set report parameters
## series <- "Miami"
## genhz_rules <- "C:/workspace2/lab_sum/Miami_rules.R"
## 
## # report file path
## report_path <- "C:/workspace2/lab_sum/report.Rmd"
## 
## # Run the report
## render(input = report_path,
##        output_dir = "C:/workspace2",
##        output_file = "C:/workspace2/lab_sum.html",
##        envir = new.env(),
##        params = list(series = series,
##                      genhz_rules = genhz_rules
##                      )
##        )
## 

## # set up ch4 path and path for report
## ch4.data.path <- "C:/workspace2/chapter4"
## ch4.mucomp.path <- paste0(ch4.data.path,"/mucomp")
## 
## # create any directories that may be missing
## if(!dir.exists(ch4.mucomp.path))
##   dir.create(ch4.mucomp.path, recursive = TRUE)
## 
## # download raster data, SSURGO clip from CA630, and sample script for clipping your own raster data
## download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/chapter_4-mucomp-data/ch4_mucomp-data.zip', paste0(ch4.mucomp.path, '/chapter_4-mucomp-data.zip'))
## 
## unzip(paste0(ch4.mucomp.path, '/chapter_4-mucomp-data.zip'), exdir = ch4.mucomp.path, overwrite = TRUE)

## # create new instance of reports
## library(soilReports)
## 
## # get report dependencies
## reportSetup('region2/mu-comparison')
## 
## # create report instance
## copyReport('region2/mu-comparison', outputDir = ch4.mucomp.path, overwrite = TRUE)

## # copy config file containing relative paths to rasters downloaded above
## file.copy(paste0(ch4.mucomp.path, "/new_config.R"), paste0(ch4.mucomp.path,"/config.R"), overwrite = TRUE)

## # create new instance of reports
## library(soilReports)
## 
## # set path for shiny-pedon-summary report instance
## ch4.shinyped.path <- "C:/workspace2/chapter4/shiny-pedon"
## 
## # create directories (if needed)
## if(!dir.exists(ch4.shinyped.path))
##   dir.create(ch4.shinyped.path, recursive = TRUE)
## 
## # get report dependencies
## reportSetup('region2/shiny-pedon-summary')
## 
## # copy report contents to target path
## copyReport('region2/shiny-pedon-summary', outputDir = ch4.shinyped.path, overwrite = TRUE)
