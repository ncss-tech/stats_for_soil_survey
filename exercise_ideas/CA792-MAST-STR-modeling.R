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

