library(aqp)
library(soilDB)
library(sharpshootR)
library(latticeExtra)


### Working on Drummer

soil <- 'DRUMMER'

## think about OSD
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

# leave room for a summary of the simulated thickness
par(mar=c(0, 0, 3, 1))
plotSPC(s, name='hzname', cex.names=0.8, n.depth.ticks = 8)
title('OSD + 8 Realizations')


# ok that is neat, what about the rest?
str(osd, 1)

# competing series
# https://ncss-tech.github.io/AQP/soilDB/competing-series.html
head(osd$competing)


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


# now compare




## siblings?
# https://ncss-tech.github.io/AQP/soilDB/siblings.html


## SDA

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




q <- "SELECT component.cokey, compname, mrulename, interplr, interplrc
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
INNER JOIN component ON component.mukey = mapunit.mukey
INNER JOIN cointerp ON component.cokey = cointerp.cokey
WHERE
-- exclude STATSGO
areasymbol != 'US'
AND compname = 'Drummer'
AND seqnum = 0
AND mrulename IN ('ENG - Construction Materials; Topsoil', 
'ENG - Sewage Lagoons', 'ENG - Septic Tank Absorption Fields', 
'ENG - Unpaved Local Roads and Streets')
AND interplr IS NOT NULL;"

x <- SDA_query(q)
bwplot(mrulename ~ interplr, data=x)


## color stuff
# http://ncss-tech.github.io/AQP/aqp/investigating-soil-color.html
# http://ncss-tech.github.io/AQP/aqp/color-contrast.html


# a vector of named soil series
# the search is case insensitive
soils <- c('amador', 'pentz', 'pardee', 'auburn', 'loafercreek', 'millvilla')

# moist colors are converted from Munsell -> sRGB by default
s <- fetchOSD(soils)
# also convert dry colors
s.dry <- fetchOSD(soils, colorState = 'dry')

# quickly compare moist to dry colors
par(mar=c(1,0,2,1), mfrow=c(2,1))
plot(s) ; title('Moist Colors')
plot(s.dry) ; title('Dry Colors')


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






# get all NASIS pedons correlated to DRUMMER
pedons <- fetchNASIS(from='pedons', nullFragsAreZero=TRUE)

str(pedons, 2)


dx <- pedons$obs_date
dy <- rep(1, times=length(pedons))

dx <- sort(dx)
dy <- cumsum(dy)

plot(dx, dy, type='S', las=1, cex=1, cex.axis=0.8)
grid()

rmf <- get_RMF_from_NASIS_db()
lapply(rmf, head)



## KSSL
# http://ncss-tech.github.io/AQP/soilDB/KSSL-demo.html




