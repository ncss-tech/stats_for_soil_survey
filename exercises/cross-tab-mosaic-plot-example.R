library(aqp)
library(soilDB)
library(sharpshootR)

# example data
data("loafercreek")

# cross-tabulation
tab <- table(
  paralithic.contact = loafercreek$paralithic.contact, 
  hillslope.pos = loafercreek$hillslopeprof
)

## more ideas:
# https://cran.r-project.org/web/packages/vcdExtra/vignettes/mosaics.html
# https://friendly.github.io/psy6136/lectures/04-Loglin.pdf
# https://friendly.github.io/psy6136/schedule.html

# mosaic plot
mosaicplot(tab, color = TRUE, shade = TRUE, las = 1, cex.axis = 1.25)



## hillslope position proportions -> cross-tab -> mosaic plot

# get OSD data + extended summaries
o <- fetchOSD(c('peters', 'pentz', 'amador', 'pardee'), extended = TRUE)

# view 2D hillslope proportions
# rows sum to 1
hp <- o$hillpos
vizHillslopePosition(hp)

# convert to table class object
tab <- as.table(as.matrix(hp[, 2:6]))
dimnames(tab) <- list(hp$series, names(hp)[2:6])

## assumes equal-weights (component frequency in SSURGO) across series
## not always reasonable
## consider converting to table-wise proportions
# tab <- tab / sum(tab)


# convert row-wise proportions into row-wise counts
tab <- round(sweep(tab, MARGIN = 1, STATS = hp$n, FUN = '*'))
tab

# check that row sums are equal or close to equal number of components
rowSums(tab) - hp$n

# visual indication of independence
mosaicplot(tab, shade = TRUE, color = TRUE, las = 1, main = 'Hillslope Position vs. Series')

