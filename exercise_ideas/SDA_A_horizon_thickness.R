library(aqp)
library(soilDB)

# select some series names to query SSURGO components for
series_names <- c("Musick","Holland","Shaver","Chawnakee")

# fetch
res <- fetchSDA(WHERE = sprintf("compname IN %s", format_SQL_in_statement(series_names)), duplicates = TRUE)
mu <- get_mapunit_from_SDA(WHERE = sprintf("mukey IN %s", format_SQL_in_statement(unique(res$mukey))))
lg <- get_legend_from_SDA(WHERE = sprintf("mu.lkey IN %s", format_SQL_in_statement(unique(mu$lkey))))

# join mapunit and legend to site
site(res) <- mu
site(res) <- lg

# calculate O+A horizon thickness w/ aqp::getSurfaceHorizonDepth (uses REGEX)
res$ahzthk <- profileApply(res, getSurfaceHorizonDepth, pattern = "^A|H1|O") #H1 conventionally ~ A, where present

# inspect distribution
plot(density(res$ahzthk, from=0))

# which component name has 109cm thick A?
subset(res, ahzthk == 109)$compname


# split by areasymbol and component name
reslist <- split(res, f = list(res$areasymbol, res$compname))

# calculate `ahzthk` quantiles for each group
ahzthkqtiles <- do.call('rbind', lapply(reslist, function(x) {
  subspc <- subset(res, cokey %in% x$cokey)
  if(length(subspc) > 0) {
    quantile(subspc$ahzthk, na.rm = TRUE)
  }
}))


# example: inspect Musick from CA630 
reslist$CA630.Musick$is_a_horizon <- factor(grepl("^A|H1|O", reslist$CA630.Musick$hzname))
plotSPC(reslist$CA630.Musick, color = "is_a_horizon", label = "ahzthk")
ahzthkqtiles[rownames(ahzthkqtiles) == "CA630.Musick",]

## the "roll-your-own" approach (without fetchSDA) might look like this:
# 
# q <- sprintf("select * from component 
#                 inner join chorizon on component.cokey = chorizon.cokey
#                 inner join mapunit on component.mukey = mapunit.mukey
#                 inner join legend on legend.lkey = mapunit.lkey
#               where compname IN %s and areasymbol != 'US'", format_SQL_in_statement(series_names))
# 
# res <- SDA_query(q)
# 
# # make soil profile collection
# depths(res) <- cokey ~ hzdept_r + hzdepb_r
# 
# hzdesgnname(res) <- "hzname"
# 
# # normalize split vars to site level (only needed for query where all stuff is joined to chorizon)
# site(res) <- ~ areasymbol + compname
# 
# ...
