## ----setup, echo=FALSE, warning=FALSE, message=FALSE---------------------
# setup
knitr::opts_chunk$set(message=FALSE, warning=FALSE, background='#F7F7F7', fig.align='center', fig.retina=2, dev='png', tidy=FALSE, verbose=FALSE, antialias='cleartype', cache=FALSE)

## ---- warning=FALSE, message=FALSE---------------------------------------
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
p <- c(
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

## ---- eval=FALSE---------------------------------------------------------
## 
## View(h)
## 

## ------------------------------------------------------------------------

vars <- c("genhz", "clay", "total_frags_pct", "phfield", "effclass")
summary(h[vars])


## ------------------------------------------------------------------------
# just for factors
levels(h$genhz)

# for characters and factors
sort(unique(h$hzname)) 

## ---- eval=FALSE---------------------------------------------------------
## h$hzname <- ifelse(h$hzname == "BT", "Bt", h$hzname)
## 
## # or
## 
## h$hzname[h$hzname == "BT"] <- "Bt"
## 
## # or as a last resort we could manually edit the spreadsheet in R
## 
## edit(h)

## ------------------------------------------------------------------------
# gopheridge rules
n <- c('A', 'Bt1', 'Bt2', 'Bt3','Cr','R')
p <- c('^A|BA$', 'Bt1|Bw','Bt$|Bt2', 'Bt3|CBt$|BCt','Cr','R')

## ---- echo = FALSE-------------------------------------------------------
desc <- data.frame(
  Parameter = c("Mean", "Median", "Mode", "Standard Deviation", "Quantiles"),
  NASIS = c(rep("RV", 3), rep("L & H", 2)),
  Description = c("arithmetic average", 
                  "middle value, 50% quantile", 
                  "most frequent value", "variation around mean", 
                  "percent rank of values, such that all values are <= p"
                  ),
  'R function' = c("mean()", "median()", "sort(table(), decreasing = TRUE)[1]", "sd()", "quantile()"),
  check.names = FALSE
  )
knitr::kable(desc, caption = "Short Description of Descriptive Statistics and R Functions")

## ------------------------------------------------------------------------
clay <- na.exclude(h$clay) # first remove missing values and create a new vector

mean(clay)

# or use the additional na.rm argument

mean(h$clay, na.rm = TRUE)

## ------------------------------------------------------------------------
median(clay)

## ------------------------------------------------------------------------
sort(table(round(h$clay)), decreasing = TRUE)[1] # sort and select the 1st value, which will be the mode

## ------------------------------------------------------------------------
table(h$genhz)

# or

# summary(h$genhz)

## ------------------------------------------------------------------------
table(h$genhz, h$texcl)

## ------------------------------------------------------------------------
# append the table with row and column sums

addmargins(table(h$genhz, h$texcl))

# calculate the proportions relative to the rows, margin = 1 calculates for rows, margin = 2 calculates for columns, margin = NULL calculates for total observations

round(prop.table(table(h$genhz, h$texture_class), margin = 1) * 100) 

## ------------------------------------------------------------------------
aggregate(clay ~ genhz, data = h, mean)

## ------------------------------------------------------------------------
aggregate(clay ~ genhz, data = h, median)

# or we could use the summary() function to get both the mean and median

aggregate(clay ~ genhz, data = h, summary)

## ------------------------------------------------------------------------
var(h$clay)

## ------------------------------------------------------------------------
sd(clay)

# or

# sqrt(var(clay))

## ------------------------------------------------------------------------
cv <- sd(clay) / mean(clay) * 100
cv

## ------------------------------------------------------------------------
quantile(clay)

# or

quantile(clay, c(0.05, 0.5, 0.95))

## ------------------------------------------------------------------------
range(clay)

## ------------------------------------------------------------------------
diff(range(clay))

# or

# max(clay) - min(clay)

## ------------------------------------------------------------------------
IQR(clay)

# or

# diff(quantile(clay, c(0.25, 0.75)))

## ------------------------------------------------------------------------
h$hzdepm <- (h$hzdepb + h$hzdept) / 2 # Compute the middle horizon depth

vars <- c("hzdepm", "clay", "sand", "total_frags_pct", "phfield")

round(cor(h[vars], use = "complete.obs"), 2)

## ----graphical descriptions, echo = FALSE--------------------------------
figs <- data.frame(
  'Plot Types' = c("Bar", "Histogram", "Density", "Quantile-Quantile", "Box-Whisker", "Scatter & Line"),
   Description = c("a plot where each bar represents the frequency of observations for a 'group'",
                   "a plot where each bar represents the frequency of observations for a 'given range of values'",
                  "an estimation of the frequency distribution based on the sample data",
                  "a plot of the actual data values against a normal distribution",
                  "a visual representation of median, quartiles, symmetry, skewness, and outliers",
                   "a graphical display of one variable plotted on the x axis and another on the y axis"
                  ),
  check.names = FALSE
  )

knitr::kable(figs, caption = "Short Description of Graphical Methods")

## ----graphical functions, echo = FALSE-----------------------------------
figs <- data.frame(
  'Plot Types' = c("Bar", "Histogram", "Density", "Quantile-Quantile", "Box-Whisker", "Scatter & Line"),
  # Description = c("a bar plot where each bar represents the frequency of observations for a given range of values",
  #                "an estimation of the frequency distribution based on the sample data",
  #                "a plot of the actual data values against a normal distribution",
  #                "a visual representation of median, quartiles, symmetry, skewness, and outliers",
  #                 "a graphical display of one variable plotted on the x axis and another on the y axis",
  #                "plots formatted for the representation of circular data"
  #                ),
  'Base R' = c("barplot()", "hist()", "plot(density())", "qqnorm()", "boxplot()", "plot()"),
  'lattice' = c("barchart()", "histogram()", "densityplot()", "qq()", "bwplot()", "xyplot"),
  'ggplot geoms' = c("geom_bar()", "geom_histogram()", "geom_density()", "geom_qq()", "geom_boxplot()", "geom_point()"),
  check.names = FALSE
  )

knitr::kable(figs, caption = "Comparison of R's 3 Graphing Systems and their Equivalent Functions for Plotting")

## ----distribution comparison, echo=FALSE, fig.dim = c(8, 4)--------------
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


## ----barplot-------------------------------------------------------------
library(ggplot2)

# find missing textures
idx <- is.na(h$texture_class)

# bar plot
ggplot(h[!idx, ], aes(x = texcl)) +
  geom_bar()

## ----histogram-----------------------------------------------------------

ggplot(h, aes(x = clay)) +
  geom_histogram(bins = nclass.Sturges(h$clay))


## ----densityplot---------------------------------------------------------

ggplot(h, aes(x = clay)) +
  geom_density()


## ----boxplots------------------------------------------------------------

ggplot(h, (aes(x = genhz, y = clay))) +
  geom_boxplot()


## ------------------------------------------------------------------------

# QQ Plot for Clay
ggplot(h, aes(sample = clay)) + 
  geom_qq() +
  geom_qq_line()

# QQ Plot for Frags
ggplot(h, aes(sample = total_frags_pct)) + 
  geom_qq() +
  geom_qq_line()


## ---- echo=FALSE, fig.align="center"-------------------------------------

library(ggplot2)

dnorm_limit_1 <- function(x) {
  y <- dnorm(x)
  y[-1 > x | x > 1] <- NA
  return(y)
  }
dnorm_limit_2 <- function(x) {
  y <- dnorm(x)
  y[-2 > x | x > 2] <- NA
  return(y)
  }

df <-data.frame(x = c(-3, 3))

ggplot(df, aes(x = x)) + 
  stat_function(fun=dnorm) +
  stat_function(fun=dnorm_limit_1, geom="area", fill="blue", alpha=0.2) +
  stat_function(fun=dnorm_limit_2, geom="area", fill="orange", alpha=0.2) +
  annotate("text", x = 0, y = 0.2, label = "1 sd = 68%") +
  annotate("text", x = -1.5, y = 0.03, label = "2 sd = 95%")


## ---- echo = FALSE, warning=FALSE----------------------------------------

r <- data.frame(y = dunif(seq(-1, 2, 0.1)), x = 1:31)

ggplot(r, aes(x = x, y = y)) +
  geom_line() +
  geom_polygon(fill = "grey", alpha = 0.5) +
  ggtitle("Uniform Distribution: Minimum = 0, Maximum = 1")


## ----clay vs frags, echo=FALSE, fig.dim = c(8, 4)------------------------

p   <- c(0.025, 0.25, 0.5, 0.75, 0.975)

avg <- mean(h$clay, na.rm = TRUE)
std <- sd(h$clay,   na.rm = TRUE)

clay <- rbind(
  data.frame(
    value = c(avg - 2 * std, avg, avg + 2 * std),
    variable = "mean & sd",
    stringsAsFactors = TRUE
    ),
  data.frame(
    value    = quantile(h$clay, p, na.rm = TRUE),
    variable = "median & pct",
    stringsAsFactors = TRUE
    )
  )

avg <- mean(h$total_frags_pct, na.rm = TRUE)
std <- sd(h$total_frags_pct,   na.rm = TRUE)

frags <- rbind(
  data.frame(
    variable = "mean & sd",
    value = c(avg - 2 * std, avg, avg + 2 * std),
    stringsAsFactors = TRUE
    ),
  data.frame(
    variable = "median & pct", 
    value    = quantile(h$total_frags_pct, p, na.rm = TRUE),
    stringsAsFactors = TRUE
    )
  )

p1 <- ggplot(h, aes(x = clay)) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_vline(data = clay, aes(xintercept = value, lty = variable)) +
  xlab("clay (%)") +
  #xlim(-5, max(h$total_frags_pct)) +
  theme(aspect.ratio = 1) +
  ggtitle("Clay")

p2 <- ggplot(h, aes(x = total_frags_pct)) +
  geom_density(fill = "grey", alpha = 0.5) +
  geom_vline(data = frags, aes(xintercept = value, lty = variable)) +
  xlab("rock fragments (%)") +
  #xlim(-5, max(h$total_frags_pct)) +
  theme(aspect.ratio = 1) +
  ggtitle("Rock Fragments")

gridExtra::grid.arrange(p1, p2, ncol = 2)


## ---- echo=FALSE, fig.dim = c(8, 4)--------------------------------------

# scatter plot
p_s <- ggplot(h, aes(x = clay, y = hzdepm)) +
  geom_point() +
  ylim(100, 0) +
  ylab("depth (cm)") + xlab("clay (%)") +
  theme(aspect.ratio = 1) +
  ggtitle("Scatter Plot")

# line graph

p_l <- ggplot(h) +
  geom_smooth(aes(y = clay, x = hzdepm), se = FALSE) +
  geom_step(aes(y = clay, x = hzdept, group = peiid), direction = "vh", alpha = 0.5) +
  xlim(100, 0) +
  xlab("depth (cm)") + ylab("clay (%)") +
  coord_flip() +
  theme(aspect.ratio = 1) +
  ggtitle("Line Plot")

gridExtra::grid.arrange(p_s, p_l, ncol = 2)


## ------------------------------------------------------------------------
# scatter plot
ggplot(h, aes(x = clay, y = hzdepm)) +
  geom_point() +
  ylim(100, 0)

# line plot
ggplot(h, aes(y = clay, x = hzdepm, group = peiid)) +
  geom_line() +
  coord_flip() +
  xlim(100, 0)


## ------------------------------------------------------------------------
# Load the GGally package
library(GGally)

# Create a scatter plot matrix
vars <- c("hzdepm", "clay", "phfield", "total_frags_pct")

ggpairs(h[vars])


## ---- echo=FALSE, fig.dim = c(8, 4)--------------------------------------
h$clay2 <- ifelse(is.na(h$clay), 0, h$clay)

ggplot(h[!idx, ], aes(y = clay, x = hzdepm, col = genhz)) +
  geom_point(size = 2) +
  geom_smooth(se = FALSE, col = "black") +
  xlim(100, 0) + ylim(min(h$clay, na.rm = TRUE), max(h$clay, na.rm = TRUE)) +
  xlab("depth (cm)") + ylab("clay (%)") +
  coord_flip() +
  theme(aspect.ratio = 1)

## ----color, fig.dim = c(8, 4)--------------------------------------------

idx <- h$genhz %in% c("Cr", "R", "not-used")

# scatter plot
ggplot(h[!idx, ], aes(x = clay, y = hzdepm, color = genhz)) +
  geom_point(size = 3) +
  ylim(100, 0)

# density plot
ggplot(h[!idx, ], aes(x = clay, color = genhz)) +
  geom_density(size = 2)

# bar plot
ggplot(h, aes(x = genhz, fill = texture_class)) +
  geom_bar()

# box plot
ggplot(h[!idx, ], aes(x = genhz, y = clay)) + 
  geom_boxplot()

# heat map (pseudo bar plot)
s <- site(loafercreek)

ggplot(s, aes(x = landform_string, y = pmkind)) + 
  geom_tile(alpha = 0.2) 
  

## ----facets, fig.dim = c(8, 4)-------------------------------------------

# convert to long format
df <- reshape2::melt(h, 
                     id.vars = c("peiid", "genhz", "hzdepm"), 
                     measure.vars = c("clay", "phfield", "total_frags_pct")
                     )

idx <- df$genhz %in% c("Cr", "R", "not-used")

ggplot(df[!idx, ], aes(x = genhz, y = value)) +
  geom_boxplot() +
  xlab("genhz") +
  facet_wrap(~ variable, scales = "free_y")


## ---- fig.dim=c(8, 4)----------------------------------------------------

library(aqp)

s <- slice(loafercreek, 1:100 ~ clay + phfield + total_frags_pct)
s <- slab(s, fm = ~ clay + phfield + total_frags_pct, 
          slab.fun = function(x) quantile(x, c(0.1, 0.5, 0.9), na.rm = TRUE)
          )

ggplot(s, aes(x = top, y = X50.)) +
  geom_line() +
  geom_ribbon(aes(ymin = X10., ymax = X90., x = top), alpha = 0.2) +
  xlim(c(100, 0)) +
  coord_flip() +
  facet_wrap(~ variable, scales = "free_x")


## ------------------------------------------------------------------------
# arithematic mean
log10(mean(1/10^-h$phfield, na.rm = TRUE)) 

# geometric mean
mean(h$phfield, na.rm = TRUE) 

## ---- warning=FALSE, message=FALSE---------------------------------------

# Extract the site table
s <- site(loafercreek) 

library(circular)

aspect <- s$aspect_field
aspect <- circular(aspect, template="geographic", units="degrees", modulo="2pi")

summary(aspect)

## ------------------------------------------------------------------------
rose.diag(aspect, bins = 8, col="grey")

## ---- eval=FALSE---------------------------------------------------------
## # Install the soilReports package from GitHub
## devtools::install_github("ncss-tech/soilReports", dependencies=FALSE, upgrade_dependencies=FALSE)

## ---- eval=FALSE---------------------------------------------------------
## # Load the soilReports and rmarkdown package
## library(soilReports)
## library(rmarkdown)
## 
## # List reports
## listReports()

## ---- eval=FALSE---------------------------------------------------------
## # Copy report to your directory
## copyReport(reportName = "region11/lab_summary_by_taxonname", outputDir = "C:/workspace2/lab_sum")

## ---- eval=FALSE---------------------------------------------------------
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

## ----r_poly_extract, eval = FALSE----------------------------------------
## # load rgdal package
## library(rgdal)
## 
## # import CA794 map unit polygons
## ca794 <- readOGR(dsn = "E:/geodata/project_data/8VIC/ca794", layer = "ca794")
## 
## # reproject
## ca794 <- spTransform(ca794, CRS("+init=epsg:5070"))
## 
## # convert MUKEY to  an integer
## ca794$mukey2 <- as.integer(as.character(ca794$MUKEY))
## 
## # export shapefile
## writeOGR(ca794,
##          dsn = "C:/workspace",
##          layer = "ca794",
##          driver = "ESRI Shapefile",
##          overwrite_layer = TRUE
##          )
## 
## # load RSAGA package
## library(RSAGA)
## 
## # set rsaga path
## myenv <- rsaga.env(path = "C:/Program Files/QGIS Essen/apps/saga")
## 
## # load DEM
## ned <- raster("E:/geodata/project_data/8VIC/sdat/ned30m_8VIC.sdat")
## 
## # create a blank raster that matches the DEM
## test <- raster(extent(ca794), ext = extent(ned), crs = crs(ned), res = res(ned))
## 
## # export the raster
## writeRaster(test,
##             file = "E:/geodata/project_data/8VIC/sdat/ca794.sdat",
##             format = "SAGA",
##             progress = "text",
##             overwrite = TRUE
##             )
## 
## # Convert the CA794 shapefile to a rsaga raster
## rsaga.geoprocessor("grid_gridding", 0, env = myenv, list(
##   INPUT = "C:/workspace/ca794.shp",
##   FIELD = "mukey2",
##   OUTPUT = "2",
##   TARGET = "0",
##   GRID_TYPE = "2",
##   USER_GRID = "E:/geodata/project_data/8VIC/sdat/ca794.sgrd",
##   USER_XMIN = extent(test)[1] + 15,
##   USER_XMAX = extent(test)[2] - 15,
##   USER_YMIN = extent(test)[3] + 15,
##   USER_YMAX = extent(test)[4] - 15,
##   USER_SIZE = res(test)[1]
##   ))
## 
## # Extract the zonal statistics for 2 rasters
## rsaga.geoprocessor("statistics_grid", 5, env = myenv, list(
##   ZONES = "E:/geodata/project_data/8VIC/sdat/ca794.sgrd",
##   STATLIST = paste(c("E:/geodata/project_data/8VIC/sdat/ned30m_8VIC.sgrd", "E:/geodata/project_data/8VIC/sdat/ned30m_8VIC_slope5.sgrd"), collapse = ";"),
##   OUTTAB = "C:/workspace/github/stats_for_soil_survey/trunk/data/ca794_zonal.csv"
##   ))

## ----poly extract2-------------------------------------------------------
# import the results
test <- read.csv("C:/workspace2/github/stats_for_soil_survey/trunk/data/ca794_zonal.csv") 

# rename the mukey column
names(test)[1] <- "mukey" 

# examine mukey 2480977
subset(test, mukey == 2480977) 

## ---- eval = FALSE-------------------------------------------------------
## # Take a stratified random sample
## s <- spsample(ca794, n = 1000, type = "stratified")
## 
## setwd("E:/geodata/project_data/8VIC/ca794")
## 
## # Create a raster stack
## rs <- stack(c(
##   elev = "ned30m_8VIC.tif",
##   slope = "ned30m_8VIC_slope5.tif")
##   )
## # Set the spatial projection
## proj4string(rs) <- CRS("+init=epsg:5070")
## 
## # Extract the map unit polygon value
## test1 <- over(s, ca794)
## 
## # Extract the raster values
## test2 <- data.frame(extract(rs, s))
## 
## # Combine the two datasets
## test2 <- cbind(test1, test2)
## 
## # Cache/save the results
## save(test2, file = "C:/workspace/github/stats_for_soil_survey/trunk/data/ch2_sample.Rdata")

## ------------------------------------------------------------------------
# Load the cache/saved results
load(file = "C:/workspace2/github/stats_for_soil_survey/trunk/data/ch2_sample.Rdata")

# Examine summary for mukey 2480977
summary(subset(test2, MUKEY == 2480977)) 

