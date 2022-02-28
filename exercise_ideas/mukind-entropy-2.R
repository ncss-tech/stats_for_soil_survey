

## Excerpt from Soil Survey Handbook
#
# Consociations
# 
# In a consociation, delineated areas use a single name from the dominant component in the map unit. Dissimilar components are minor in extent.
# 
# Complexes and associations
# 
# Complexes and associations consist of two or more dissimilar components that occur in a regularly repeating pattern. The total amount of other dissimilar components is minor in extent.
# 
# Undifferentiated groups
# 
# Undifferentiated groups consist of two or more components that are not consistently associated geographically and, therefore, do not always occur together in the same map delineation.




library(lattice)
library(tactile)

## previously computed Shannon H by map unit, for all of SSURGO
tf <- tempfile()
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/mukind-entropy-calc.rds', destfile = tf)

all.H <- readRDS(tf)


# tabulate map unit kinds for all SSURGO map unit keys
sort(
  round(
    prop.table(
      table(all.H$mukind)
    ),
    digits = 2
  ), decreasing = TRUE
)




# interesting...
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



# using ggplot style visualization

library(ggdist)
library(ggplot2)

all.H <- na.omit(all.H)


ggplot(all.H, aes(x = H, y = mukind)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal', size = 5) + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(size = 12, face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 3) +
  scale_color_brewer(palette = 'Blues') + 
  xlab('Shannon Entropy (base 2)') + ylab('') +
  labs(title = 'All SSURGO Components', color = 'Interval')








###################### re-create the example data #####################


library(aqp)
library(soilDB)
library(purrr)
library(furrr)

##
## compute Shannon H by map unit kind for all of SSURGO!
## iteration over SSA in parallel
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

## all of SSURGO areasymbols
# != 'US' excludes STATSGO
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

# FY22: 318,671 map unit keys
nrow(all.H)

## save to a local file for later use
saveRDS(all.H, file = '../data/mukind-entropy-calc.rds')




