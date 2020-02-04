## Tasks: 
# * explore the various interfaces to NCSS/NRCS soil survey databases
# * tinker with R objects
# * learn about SoilProfileCollection objects
# * learn about the AQP suite of R packages
# * adapt to your own tasks

## When in doubt, documentation is here:
# http://ncss-tech.github.io/AQP/


# packages
library(aqp)
library(soilDB)
library(sharpshootR)
library(latticeExtra)



## We will start with the soil series and OSD

# how about this one
# https://casoilresource.lawr.ucdavis.edu/sde/?series=drummer
soil <- 'DRUMMER'

# standard request for OSD data
osd <- fetchOSD(soils = soil)
str(osd, 1)

# extended request
osd <- fetchOSD(soils = soil, extended = TRUE)
str(osd, 1)

# singleton SPC, ick
plotSPC(osd$SPC)

# better
par(mar=c(0, 1, 0, 1))
plotSPC(osd$SPC, cex.names=1, axis.line.offset = -5, width=0.1, x.idx.offset = 0.1, name='hzname')

# horizon thickness stats
hz.thick <- with(horizons(osd$SPC), bottom - top)
summary(hz.thick)
sd(hz.thick)

# hmm: simulate 8 Drummer pedons from OSD and stats on hz thickness
s <- sim(osd$SPC, n = 8, hz.sd=6)
str(s, 2)

# combine SoilProfileCollection objects
# original OSD + simulated
s <- aqp::union(list(osd$SPC, s))

# sketches
par(mar=c(0, 0, 3, 1))
plotSPC(s, name='hzname', cex.names=0.8, n.depth.ticks = 8)
title('OSD + 8 Realizations')

# ok that is neat, what about the rest?
str(osd, 1)

# !!!
osd$climate.annual
osd$climate.monthly
osd$mlra
osd$hillpos
osd$geomcomp
osd$pmkind

# these are all snapshots
osd$soilweb.metadata


## STOP! 
# Take 10 minutes and tinker with another soil series, or collection of soil series
# or, try some of the following:
# * http://ncss-tech.github.io/AQP/sharpshootR/OSD-dendrogram.html



## Break ##


# competing series
# https://ncss-tech.github.io/AQP/soilDB/competing-series.html
head(osd$competing)
osd$competing$competing

# get competing series OSD data
spc <- fetchOSD(c(soil, osd$competing$competing))

# this will only work for established series, e.g. those that have been "mapped" somewhere
idx <- which(spc$series_status == 'established')
spc <- spc[idx, ]

# save family taxa and set of series names for later
fm.name <- unique(na.omit(spc$family))
s.names <- unique(site(spc)$id)


par(mar=c(0.25,0,1,1))
plot(spc)
mtext(fm.name, side = 3, at = 0.5, adj = 0, line = -1, font=4)
mtext('source: Official Series Descriptions', side = 1, at = 0.5, adj = 0, line = -1, font=3, cex=1)


# now compare lab data

# wait, how?
kssl.data <- fetchKSSL(series = 'Drummer')
str(kssl.data, 2)
str(horizons(kssl.data))
str(site(kssl.data))

# sketches
par(mar=c(0,0,3,1))
plot(kssl.data, color='clay')

# paired morphologic data
kssl.data <- fetchKSSL(series = 'Drummer', simplifyColors = TRUE, returnMorphologicData = TRUE)
str(kssl.data, 2)

# moist colors
plot(kssl.data$SPC, color='moist_soil_color')

# pretty, we will do more with color later
previewColors(kssl.data$SPC$moist_soil_color)


# back to the competing series...
kssl.data <- lapply(s.names, fetchKSSL)

str(kssl.data, 1)

# some may not have any data, filter those out
not.null <- which(! sapply(kssl.data, is.null))
# combine into single SPC
kssl.data <- union(kssl.data[not.null])

# normalize soil series names
table(kssl.data$taxonname)
for(i in s.names)
  kssl.data$taxonname[grep(i, kssl.data$taxonname, ignore.case = TRUE)] <- toupper(i)

table(kssl.data$taxonname)


# aggregate over entire collection, marginal quantiles of select properties
agg <- slab(kssl.data, ~ clay + sand + estimated_ph_h2o + cec7 + bs82)
levels(agg$variable) <- c('Clay (%)', 'Sand (%)', 'pH 1:1 H2O', 'CEC at pH 7 (cmol[+]/kg)' ,'Base Saturation at pH 8.2 (%)')

# define plotting style
tps <- list(superpose.line=list(col=c('RoyalBlue', 'DarkRed', 'DarkGreen'), lwd=2))

xyplot(top ~ p.q50 | variable, data=agg, ylab='Depth', main=fm.name, sub='source: KSSL',
       xlab='median bounded by 25th and 75th percentiles',
       lower=agg$p.q25, upper=agg$p.q75, ylim=c(205,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       cf=agg$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(5,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps,
       auto.key=list(columns=3, lines=TRUE, points=FALSE)
)


# compare a single profile to the group-level aggregate values
idx <- which(kssl.data$taxonname == soil)
drummer.agg <- slab(kssl.data[idx, ], ~ clay + sand + estimated_ph_h2o + cec7 + bs82)

# fix variable names, order same as specified above
levels(drummer.agg$variable) <- c('Clay (%)', 'Sand (%)', 'pH 1:1 H2O', 'CEC at pH 7 (cmol[+]/kg)' ,'Base Saturation at pH 8.2 (%)')


# manually update the group column
drummer.agg$group <- soil
# a$p.q25 <- NA
# a$p.q75 <- NA
agg$group <- 'ALL'

# combine into a single data.frame:
g <- rbind(agg, drummer.agg)
g$group <- factor(g$group)

xyplot(top ~ p.q50 | variable, groups=group, data=g, ylab='Depth', main=fm.name,
       xlab='median bounded by 25th and 75th percentiles', sub='source: KSSL' ,
       lower=g$p.q25, upper=g$p.q75, ylim=c(205,-5),
       panel=panel.depth_function, alpha=0.25, sync.colors=TRUE,
       prepanel=prepanel.depth_function,
       # cf=g$contributing_fraction,
       par.strip.text=list(cex=0.8),
       strip=strip.custom(bg=grey(0.85)),
       layout=c(5,1), scales=list(x=list(alternating=1, relation='free'), y=list(alternating=3)),
       par.settings=tps,
       auto.key=list(columns=2, lines=TRUE, points=FALSE)
)


## STOP: try getting / comparing some KSSL data
# * repeat some of the above, or try:
# * http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html
# * http://ncss-tech.github.io/AQP/soilDB/fetchKSSL-VG-demo.html
# * https://ncss-tech.github.io/AQP/soilDB/competing-series.html

## Break ##


## siblings?
# https://ncss-tech.github.io/AQP/soilDB/siblings.html

# tinker with this!
siblings(soil, only.major = TRUE)
sibs <- siblings(soil, only.major = FALSE)

# get the data and make a quick visualization
sib.data <- fetchOSD(sibs$sib$sibling)
SoilTaxonomyDendrogram(sib.data, width=0.2)

# wait, missing DRUMMER: add it to the list
sib.data <- fetchOSD(c(soil, sibs$sib$sibling), extended = TRUE)

# recall there is a lot packed into the result
SoilTaxonomyDendrogram(sib.data$SPC, width=0.2)

# do something with the hillslope and geomcomp proportions
vizHillslopePosition(sib.data$hillpos)
vizGeomorphicComponent(sib.data$geomcomp)

# do something with the annual climate data
vizAnnualClimate(sib.data$climate.annual, s = soil)

# you try it!
# https://ncss-tech.github.io/AQP/soilDB/siblings.html


## Break ##


## SDA
# http://ncss-tech.github.io/AQP/soilDB/SDA-tutorial.html

# one-off, what hydrologic groups is Drummer usually associated with?
SDA_query("SELECT hydgrp, COUNT(hydgrp) AS n from component where compname = 'Drummer' AND hydgrp IS NOT NULL GROUP BY hydgrp ;")

# does the variation follow the local phase?
# no
x <- SDA_query("SELECT localphase, hydgrp from component where compname = 'Drummer' AND hydgrp IS NOT NULL;")
table(x$localphase, x$hydgrp)

# drainage class?
# no
x <- SDA_query("SELECT drainagecl, hydgrp from component where compname = 'Drummer' AND hydgrp IS NOT NULL;")
table(x$drainagecl, x$hydgrp)


# whats up with the hydgroup B components?
x <- fetchSDA(WHERE="compname = 'Drummer' AND hydgrp = 'B'")
str(x, 2)

# are these really all the "same"?
# extract all horizon level attributes
nm <- horizonNames(x)

# generate an index to non-ID columns
# cokey and chkey are SSURGO IDs
# hzID is part of the SoilProfileCollection
idx <- grep(pattern='cokey|chkey|hzID', x = nm, invert = TRUE)

# check names
nm[idx]

# check data using column-indexing
head(horizons(x)[, nm[idx]])

# result is an index to unique profiles, using the named
# all the same
aqp::unique(x, vars=nm[idx])

# hmm... worth looking into, later
site(x)[, c('nationalmusym', 'cokey', 'compname', 'comppct_r', 'majcompflag')]




## Break ##



## NASIS
# http://ncss-tech.github.io/AQP/soilDB/fetchNASIS-mini-tutorial.html
# http://ncss-tech.github.io/AQP/soilDB/NASIS-component-data.html

# setup your selected set!

# get all NASIS pedons correlated to DRUMMER
pedons <- fetchNASIS(from='pedons', nullFragsAreZero=TRUE)

# lots of feedback!
get('sites.missing.pedons', envir=soilDB.env)
get('bad.pedon.ids', envir=soilDB.env)
get('missing.bottom.depths', envir=soilDB.env)
get('top.bottom.equal', envir=soilDB.env)

# .. maybe fix, or at least be aware of

# this is a SoilProfileCollection object
str(pedons, 2)

## track progress of pedons collected and correlated to DRUMMER 
# make two sequences: x-axis is the date
dx <- pedons$obs_date
class(dx)
# y-axis: sequence of 1, repeated as many times as there are pedons
dy <- rep(1, times=length(pedons))

# sort the dates
dx <- sort(dx)
# accumulate each pedon in sequence, what?
dy <- cumsum(dy)

# plot it using "base graphics"
plot(x=dx, y=dy, type='S', las=1, cex=1, cex.axis=0.8, 
     ylab='Cumulative Pedons',
     xlab='', main='NASIS Pedons Correlated to DRUMMER')
grid()



## what else is in there?
horizonNames(pedons)
siteNames(pedons)



# hz frequency
sort(table(pedons$hzname), decreasing=TRUE)

# geomorphic data
table(pedons$hillslopeprof)
table(pedons$hillslopeprof, pedons$slope_shape)

# graphically
dotchart(sort(table(pedons$pmkind)))
dotchart(sort(table(pedons$hillslopeprof)))

table(pedons$texture_class)


n <- c('Ap','BA', 'Btg', '2tBg','2Cg')
# REGEX rules
pat <- c('A|p',
         'BA|AB',
         '^Bt[g0-9].*(?!A)|B2|B(?!t)g|B1',
         '2B|IIB',
         '2C|IIC|C'
)



pedons$genhz <- generalize.hz(pedons$hzname, new = n, pat = pat, perl=TRUE)
table(pedons$genhz)


table(pedons$genhz, pedons$texture_class)




# special function for pulling RMF child tables
rmf <- get_RMF_from_NASIS_db()
lapply(rmf, head)






## Lets play with color
# http://ncss-tech.github.io/AQP/aqp/investigating-soil-color.html
# http://ncss-tech.github.io/AQP/aqp/color-contrast.html




## another set of pedons
# setup SS via user site ID ~ %CA792%







## .. and then?




##############################

# advanced, annotate top/bottom depths of horizon with minimum Munsell Chroma

# going to need a function
maxChroma <- function(i) {
  # extract horizons
  h <- horizons(i)
  # index the highest Munsell chroma
  idx <- which.max(h$chroma)
  
  # horizon depth names
  hd <- horizonDepths(i)
  # profile ID name
  id <- idname(i)
  
  # compile into data.frame:
  # current profile ID, top/bottom depths
  # horizon with lowest chroma
  res <- h[idx, c(id, hd)]
  
  # re-name top/bottom so as not to conflict with existing attributes
  names(res) <- c(id, 'max_chroma_top', 'max_chroma_bottom')
  
  # send back
  return(res)
}

maxChroma(s[1, ])

# apply to all profiles
mc <- profileApply(s, maxChroma, frameify = TRUE)

# merge back into source data
site(s) <- mc
# its in there
str(site(s))

par(mar=c(1,0,2,1), mfrow=c(1,1))
plotSPC(s, name='hzname', cex.names = 0.8)

lines(x=1:length(s), y=s$max_chroma_top, lty=3)
lines(x=1:length(s), y=s$max_chroma_bottom, lty=3)

