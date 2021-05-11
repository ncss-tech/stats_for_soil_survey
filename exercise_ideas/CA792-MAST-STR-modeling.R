library(aqp)
library(e1071)
library(lattice)
library(tactile)

# get data
tf <- tempfile()
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/CA792-MAST-STR-modeling.rds', tf)

# load from .RDS
x <- readRDS(tf)

# check
str(x)

# Shannon entropy for STR predictions
x$H <- apply(x[, c('Pr_cryic', 'Pr_frigid', 'Pr_mesic', 'Pr_thermic')], 1, shannonEntropy)

bwplot(STR ~ H, data = x, par.settings = tactile.theme(), xlab = 'Shannon Entropy (base 2)')


head(x)


## PCC / Kappa

(tab <- table(observed = x$STR, predicted = x$pred_STR))
classAgreement(tab, match.names = TRUE)


## Tau

# weight matrix...

# prior proportions
priors <-  apply(tab, 2, sum)/sum(tab)

# see ?tauW
tauW(tab, P = priors)




## Brier Scores

# prepare input data
B <- data.frame(
  actual = x$STR,
  x[, c('Pr_cryic', 'Pr_frigid', 'Pr_mesic', 'Pr_thermic')]
)

names(B)[2:5] <- levels(x$STR)

# compute Brier scores, lower is better
brierScore(B, classLabels = levels(x$STR), actual = 'actual')

