library(aqp)
library(soilDB)
library(lattice)
library(tactile)

# more at: https://ncss-tech.github.io/AQP/soilDB/soil-series-query-functions.html

soils <- c('cecil', 'altavista', 'lloyd', 'wickham', 'wilkes',  'chewacla', 'congaree')
x <- fetchOSD(soils, extended = TRUE)

knitr::kable(x$hillpos)


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


# 4, equal prob
(x <- rep(1, times = 4) / 4)
shannonEntropy(x)

# 5, equal prob
(x <- rep(1, times = 5) / 5)
shannonEntropy(x)

# normalized H
x <- rep(1, times = 5) / 5
shannonEntropy(x, b = length(x))


# complex or association
x <- c(45, 40, 3, 3, 3, 3) / 100
shannonEntropy(x)
shannonEntropy(x, b = length(x))

# consociation 1
x <- c(76, 9, 5, 5, 3, 2) / 100
shannonEntropy(x)
shannonEntropy(x, b = length(x))

# consociation 2
x <- c(85, 10, 5) / 100
shannonEntropy(x)
shannonEntropy(x, b = length(x))




## get component data from SDA
qq <- "SELECT 
areasymbol, muname, mukind, mapunit.mukey, cokey, compname, comppct_r
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
INNER JOIN component on mapunit.mukey = component.mukey
WHERE
-- all SSA in California
-- legend.areasymbol LIKE 'CA%'
-- specific SSA
legend.areasymbol IN ('CA790', 'CA630', 'MO071')
;
"

# run the query
x <- SDA_query(qq)

# check
nrow(x)
head(x)

# factor levels
x$mukind <- factor(x$mukind, levels = c("Association", "Consociation", "Complex", "Undifferentiated group"))

# investigate proportions of map unit kind by areasymbol
x.mu <- unique(x[, c('areasymbol', 'mukey', 'mukind')])

# cross-tab
tab <- table(x.mu$areasymbol, x.mu$mukind)

# convert to row-wise proportions
# round to 2 decimal places
round(prop.table(tab, margin = 1), 2)


# ~ median of 4 components / MU
summary(tapply(x$mukind, x$mukey, length))



# simple function applied to collections of components
MU_entropy <- function(i) {
  
  # Shannon H, base 2 (default and commonly used)
  H <- shannonEntropy(x = i$comppct_r / 100)
  
  # normalized H, base n (number of components)
  Hn <- shannonEntropy(x = i$comppct_r / 100, b = nrow(i))
  
  # combine with IDs
  res <- data.frame(
    areasymbol = i$areasymbol[1],
    mukey = i$mukey[1],
    mukind = i$mukind[1],
    H = H,
    Hn = Hn,
    n = nrow(i)
  )
  
  return(res)
}


# split into list object
xx <- split(x, x$mukey)

# check
xx[[1]]

# iterate over map units
z <- lapply(xx, MU_entropy)

# flatten to data.frame
z <- do.call('rbind', z)


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



# graphical checks
bwplot(areasymbol ~ H, data = z, par.settings = tactile.theme(), varwidth = TRUE)
bwplot(areasymbol ~ Hn, data = z, par.settings = tactile.theme(), varwidth = TRUE)

bwplot(mukind ~ H, data = z, par.settings = tactile.theme(), varwidth = TRUE)
bwplot(mukind ~ Hn, data = z, par.settings = tactile.theme(), varwidth = TRUE)

bwplot(areasymbol ~ H | mukind, data = z, par.settings = tactile.theme(), varwidth = TRUE, as.table = TRUE)
bwplot(areasymbol ~ Hn | mukind, data = z, par.settings = tactile.theme(), varwidth = TRUE, as.table = TRUE)



# investigate "high" Hn map units
subset(z, subset = Hn > 0.95)


## note: this will only work with SSA specified in my example

# complex
x[x$mukey == '466340', ]
z[z$mukey == '466340', ]

# complex
x[x$mukey == '2374651', ]
z[z$mukey == '2374651', ]

# association
x[x$mukey == '1540935', ]
z[z$mukey == '1540935', ]

# consociation
x[x$mukey == '2537554', ]
z[z$mukey == '2537554', ]





