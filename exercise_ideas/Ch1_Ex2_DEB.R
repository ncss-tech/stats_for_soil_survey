## inspired by conversation with Andrew this afternoon, originally brought up last year

library(soilDB)
library(dplyr)


# method 1, no joins required

# super-simple if we introduce convenience functions... but that happens in ch2

W <- paste0("areasymbol IN ", format_SQL_in_statement(c("CA630", "CA649")))

x <- get_mapunit_from_SDA(WHERE = W)

group_by(x, areasymbol) |>
  summarize(sum(muacres * pct_hydric / 100))


# method 2, joins in SQL

# harder, now we have SQL + R

q <- "SELECT legend.areasymbol, mu.mukey, muacres, cokey, compname, comppct_r, hydricrating
    FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    INNER JOIN component co ON mu.mukey = co.mukey
    WHERE legend.areasymbol IN ('CA630', 'CA649') 
    AND hydricrating = 'Yes' 
    ORDER BY areasymbol, mukey ;"

x <- SDA_query(q)

x$comppct_r <- x$comppct_r / 100

head(x, 10)

x$hydric_ac <- x$muacres * x$comppct_r

tapply(x$hydric_ac, x$areasymbol, sum)

group_by(x, areasymbol) |>
  summarize(sum(hydric_ac))


# method 3, using source data as CSV, joins in R

# perhaps a fine compromise if the SDA results are saved to CSV files

q.mu <- "SELECT legend.areasymbol, mu.mukey, muacres
    FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    WHERE legend.areasymbol IN ('CA630', 'CA649') 
    ORDER BY areasymbol, mukey ;"

mu <- SDA_query(q.mu)


q.co <- "SELECT mu.mukey, cokey, compname, comppct_r, hydricrating
    FROM legend
    INNER JOIN mapunit mu ON mu.lkey = legend.lkey
    INNER JOIN component co ON mu.mukey = co.mukey
    WHERE legend.areasymbol IN ('CA630', 'CA649') 
    ORDER BY areasymbol, mukey ;"

co <- SDA_query(q.co)

## save mu and co to CSV files that people can source
## no need to go into the intricacies of SDA and NASIS WWW reports

## read.csv() directly from the GH repo to start the exercise, 
## we offer this as the starting point

nrow(mu)
nrow(co)

# inner join
x <- merge(mu, co, by.x = 'mukey', by.y = 'mukey', sort = FALSE)
nrow(x)

x <- subset(x, subset = hydricrating == 'Yes')
nrow(x)

x$hydric_ac <- x$muacres * (x$comppct_r / 100)

tapply(x$hydric_ac, x$areasymbol, sum)

group_by(x, areasymbol) |>
  summarize(sum(hydric_ac))

