library(aqp)
library(soilDB)
library(purrr)
library(furrr)

library(lattice)
library(tactile)


MU_entropy <- function(i) {
  
  H <- shannonEntropy(i$comppct_r / 100)
  
  res <- data.frame(
    mukey = i$mukey[1],
    mukind = i$mukind[1],
    H = H
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

bwplot(
  mukind ~ H, 
  data = all.H, 
  par.settings = tactile.theme(), 
  varwidth = TRUE,
  main = '',
  xlab = 'Shannon Entropy (base 2)',
  ylab = ''
)


