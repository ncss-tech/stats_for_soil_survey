library(aqp)
library(soilDB)
library(lattice)
library(tactile)


# complex
x <- c(45, 40, 3, 3, 3, 3) / 100
shannonEntropy(x)

# consociation 1
x <- c(76, 9, 5, 5, 3, 2) / 100
shannonEntropy(x)

# consociation 2
x <- c(85, 10, 5) / 100
shannonEntropy(x)






MU_entropy <- function(i) {
  
  H <- shannonEntropy(i$comppct_r / 100)
  
  res <- data.frame(
    areasymbol = i$areasymbol[1],
    mukey = i$mukey[1],
    mukind = i$mukind[1],
    H = H
  )
  
  return(res)
  
}


qq <- "SELECT 
areasymbol, muname, mukind, mapunit.mukey, cokey, compname, comppct_r
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
INNER JOIN component on mapunit.mukey = component.mukey
WHERE
-- subset SSA
-- legend.areasymbol LIKE 'CA%'
legend.areasymbol IN ('CA790', 'CA630', 'MO071')
;
"

# run the query
x <- SDA_query(qq)

nrow(x)
head(x)

x$mukind <- factor(x$mukind, levels = c("Association", "Consociation", "Complex", "Undifferentiated group"))

xx <- split(x, x$mukey)

xx[[1]]

z <- lapply(xx, MU_entropy)
z <- do.call('rbind', z)

bwplot(areasymbol ~ H, data = z, par.settings = tactile.theme(), varwidth = TRUE)

bwplot(mukind ~ H | areasymbol, data = z, par.settings = tactile.theme(), varwidth = TRUE)




