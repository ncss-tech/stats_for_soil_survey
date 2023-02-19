
## you may have to install some of these
library(aqp)
library(e1071)
library(tactile)
library(rms)
library(lattice)
library(visreg)
library(ormPlot)


# get example data
tf <- tempfile()
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/CA792-MAST-STR-modeling.rds', destfile = tf)

# load from .RDS
x <- readRDS(tf)

# check
str(x)



# note that these data contain training data AND predictions


# investigate source data
bwplot(STR ~ seki_elev, data = x)

# fit proportional-odd logistic regression model
# suitable for ordinal (categories with order) data

# may have to subset extraneous columns
# adjust as needed
vars <- c('peiid', 'STR', 'seki_elev', 'weight')
x.sub <- x[, vars]

# annoying quirk of rms package functions
dd <- datadist(x.sub)
options(datadist = "dd")

# looks OK
orm.fit <- orm(STR ~ seki_elev, data = x.sub, x = TRUE, y = TRUE)
orm.fit


## useful starting point, some times confidence envelope is strange 
# output from ormplot provided plot-method
# https://cran.r-project.org/web/packages/ormPlot/vignettes/ormPlot.html
plot(orm.fit, 'seki_elev')


## investigate probability vs. elev for each STR
## note rms-specific strategy

# generate Pr(STR)
pp <- as.data.frame(predict(orm.fit, type='fitted.ind'))

# fix names
names(pp) <- paste0('Pr_', gsub('STR=', '', names(pp), fixed = TRUE))

# eval most likely STR
pp$pred_STR <- levels(x$STR)[apply(pp, 1, which.max)]

# init levels
pp$pred_STR <- factor(pp$pred_STR, levels = levels(x$STR))

# copy over to source data, row-order is the same
g <- cbind(x.sub, pp)

# reshape a subset of columns
m <- reshape2::melt(g, id.var=c('seki_elev', 'STR'), measure.vars=c("Pr_cryic", "Pr_frigid", "Pr_mesic", "Pr_thermic"))

xyplot(value ~ seki_elev, groups=variable, data=m, auto.key=list(columns=4, lines=TRUE, points=FALSE), panel=panel.plsmo, method='supsmu', label.curves=FALSE, datadensity=TRUE, trim=0, type='l', par.settings=tactile.theme())

anova(orm.fit)



## evaluation of predictions, contained within 'x'

# Shannon entropy for STR predictions
# by row
x$H <- apply(
  x[, c('Pr_cryic', 'Pr_frigid', 'Pr_mesic', 'Pr_thermic')], 
  MARGIN = 1, 
  FUN = shannonEntropy
)


# compare with equal-class threshold (log_2) = 2
# shannonEntropy(rep(1, times = 4) / 4)

bwplot(
  STR ~ H, 
  data = x, 
  xlim = c(-0.1, 2.1),
  par.settings = tactile.theme(), 
  xlab = 'Shannon Entropy (base 2)', 
  panel = function(...) {
    panel.abline(v = 2, lty =2)
    panel.bwplot(...)
  })


head(x)


## confusion matrix
(tab <- table(observed = x$STR, predicted = x$pred_STR))

## PCC / Kappa
classAgreement(tab, match.names = TRUE)


## Tau

# equal prior probabilities / proportions
tauW(tab)$tau

# specified prior probabilities / proportions
priors <- apply(tab, 2, sum)/sum(tab)
tauW(tab, P = priors)$tau


# weight matrix...
weights <- matrix(data=c(1.00,0.05,0.05,0.15,0.05,0.15,
                         0.05,1.00,0.05,0.05,0.05,0.35,
                         0.05,0.05,1.00,0.20,0.15,0.15,
                         0.15,0.05,0.25,1.00,0.10,0.25,
                         0.05,0.10,0.15,0.10,1.00,0.15,
                         0.20,0.30,0.10,0.25,0.20,1.00),
                  nrow=6, byrow=TRUE)





## Brier Scores

# prepare input data
B <- data.frame(
  actual = x$STR,
  x[, c('Pr_cryic', 'Pr_frigid', 'Pr_mesic', 'Pr_thermic')]
)

names(B)[2:5] <- levels(x$STR)

# compute Brier scores, lower is better
brierScore(B, classLabels = levels(x$STR), actual = 'actual')



## raster prediction hints


# raster stack functions

# re-scale probabilities to 0-100 for data storage reduction
pred.fun <- function(model, data) {
  v <- predict(model, data, type='fitted.ind')
  return(round(v * 100))
}


# most likely layer, by pixel
# return index to ML layer
most.likely.layer <- function(i, ...) {
  
  # this will return an empty vector when all NA
  i.max <- which.max(i)
  
  # pad with NA
  if(length(i.max) < 1)
    i.max <- NA
  
  return(i.max)
}

## TODO: NODATA encoding problem NA / 0 ?
# note: probabilities have been rescaled to 0-100
entropy <- function(i, na.rm=TRUE) {
  # rescale to probabilities
  i <- i / 100
  # add a very small fudge factor to remove 0
  i <- i + 1e-15
  # shannon entropy
  return(-sum(i * log(i), na.rm=na.rm))
}


## predictions

# `rs` is a raster stack containing rasters with the same names as predictors using in model fit

# predicts probabilities for the 4 possible classes fit by model
seki.str <- predict(rs, orm.fit, index=1:4, fun=pred.fun, progress='text', filename='results.tif', datatype='INT1U', overwrite=TRUE)



## most-likely STR
seki.str.ml <- stackApply(seki.str, indices=rep(1, times=nlayers(seki.str)), fun = most.likely.layer)

# convert to factor
seki.str.ml <- ratify(seki.str.ml)

# init RAT
rat <- levels(seki.str.ml)[[1]]
rat$STR <- c('cryic', 'frigid', 'mesic', 'thermic')
levels(seki.str.ml) <- rat



