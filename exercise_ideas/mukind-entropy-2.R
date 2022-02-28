library(aqp)
library(soilDB)
library(purrr)
library(furrr)

library(lattice)
library(tactile)

## previously computed Shannon H by map unit, for all of SSURGO
tf <- tempfile()
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/mukind-entropy-calc.rds', destfile = tf)

all.H <- readRDS(tf)

## interesting...
bwplot(
  mukind ~ H, 
  data = all.H, 
  par.settings = tactile.theme(), 
  varwidth = TRUE,
  main = 'All SSURGO Components',
  xlab = 'Shannon Entropy (base 2)',
  ylab = '',
  scales = list(x = list(tick.number = 10)),
  panel = function(...) {
    panel.grid(-1, -1)
    panel.bwplot(...)
  }
)

###################### stop here #####################


##
## compute Shannon H by map unit kind for all of SSURGO!
## iteration of SSA in parallel
##

MU_entropy <- function(i) {
  
  H <- shannonEntropy(x = i$comppct_r / 100)
  Hn <- shannonEntropy(x = i$comppct_r / 100, b = nrow(i))
  
  res <- data.frame(
    areasymbol = i$areasymbol[1],
    mukey = i$mukey[1],
    mukind = i$mukind[1],
    H = H,
    Hn = Hn
  )
  
  return(res)
  
}

getH <- function(ssa) {
  
  qq <- sprintf("SELECT 
areasymbol, muname, mukind, mapunit.mukey, cokey, compname, comppct_r
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
INNER JOIN component on mapunit.mukey = component.mukey
WHERE
-- single SSA
legend.areasymbol = '%s'", ssa)
  
  # run the query
  suppressMessages(d <- SDA_query(qq))
  
  d$mukind <- factor(d$mukind, levels = c("Association", "Consociation", "Complex", "Undifferentiated group"))
  
  dd <- split(d, d$mukey)
  
  z <- lapply(dd, MU_entropy)
  z <- do.call('rbind', z)
  
  return(z)
  
}

q <- "SELECT areasymbol, saverest FROM sacatalog WHERE areasymbol ! = 'US' ;"
x <- SDA_query(q)
head(x)


## sequential processing with progress display
# library(pbapply)
# all.H <- pblapply(x$areasymbol[1:10], safely(getH))

## parallel processing
# init parallel processing, works on macos and windows
plan(multisession)

# ~ 4.5 minutes
system.time(all.H <- future_map(x$areasymbol, safely(getH), .progress=TRUE))

# stop back-ends
plan(sequential)



## flatten
all.H <- map(all.H, pluck, 'result')
all.H <- do.call('rbind', all.H)

## save to a local file for later use
# saveRDS(all.H, file = 'mukind-entropy-calc.rds')




