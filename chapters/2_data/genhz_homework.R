knitr::opts_chunk$set(echo = TRUE)

library(aqp)
library(soilDB)

# load sample `loafercreek` data from the soilDB package
data("loafercreek")

# keep only the first 20 pedons
pedons <- loafercreek[1:20, ]

# plot profile sketches
par(mar=c(0,0,2,1))
plot(pedons, name='hzname', print.id=FALSE)

# tabulate hzname
table(pedons$hzname)

# these are the _unique_ horizon designations in our subset `pedons`
unique(pedons$hzname)

l <- fetchOSD('loafercreek')
l$hzname

# create 4 generalized horizon labels: A, upper transitional, argillic and bedrock
prototype.labels <- c('A',
                      'BA',
                      'Bt',
                      'Cr')

# REGEX rules describing mapping from field data to prototype.labels
patterns.to.match <- c('^A',
                      '^B.*[^Ct]$',
                      '.*B.*t.*',
                      'Cr|R')

pedons$newgenhz <- generalize.hz(x=pedons$hzname, new=prototype.labels, pat=patterns.to.match)

oldvsnew <- addmargins(table(pedons$newgenhz, pedons$hzname))
oldvsnew

# find which columns are greater than zero in row 'not-used'
col.idx.not.used <- which(oldvsnew['not-used',] > 0)

# what column indexes (field horizon designations) did not get mapped onto a row (generalized hz label)?
col.idx.not.used

# show just those columns
oldvsnew[, col.idx.not.used]

# create 5 generalized horizons: A, upper transitional, argillic, lower-transitional and bedrock
prototype.labels.v2 <- c('A',
                         'BA',
                         'Bt',
                         'BC',
                         'Cr')

# REGEX rules describing mapping from field data to prototype.labels
patterns.to.match.v2 <- c('^A',
                          '^B.*[^Ct]$',
                          '.*B.*t.*',
                          'C[^t]*',
                          'Cr|R')

# use generalize.hz() to apply a set of patterns and paired labels
# to the `pedons$hzname` character vector containing field designations
pedons$newgenhz2 <- generalize.hz(x=pedons$hzname, new=prototype.labels.v2, pat=patterns.to.match.v2)

# create a second cross-tabulation, using the updated genhz
oldvsnew2 <- addmargins(table(pedons$newgenhz2, pedons$hzname))

# find which table columns are greater than zero in row 'not-used'
col.idx.not.used <- which(oldvsnew2['not-used',] > 0)

# show just those columns
oldvsnew2[, col.idx.not.used]

## # check for equality (assignment 1 versus assignment 2)
## pedons$newgenhz == pedons$newgenhz2

# plot profile sketches - first 20 profiles; color by gen hz.
par(mar=c(0,0,2,1))
plotSPC(pedons, name='hzname', color='newgenhz2', print.id=FALSE)

# original field data (27 levels)
length(unique(pedons$hzname))

# new generalized data (6 levels, including not-used)
length(unique(pedons$newgenhz2))

# get the horizon data frame out of the SPC
hzdata <- horizons(pedons)

# make a list of data frames from horizons, split based on the Generalized Horizon Labels (`f`)
genhz.list <- split(hzdata, f = hzdata$newgenhz2)

# use lapply() to apply a function to each element of `genhz.list`
#  the anonymous function calculates some summary statistics on each subset dataframe (`d`)
res <- lapply(genhz.list, FUN = function(d) {
  # the variable 'd' contains the dataframe with all the data for a particular  Generalized Horizon Label
  
  # calculate mean clay content, removing NA and rounding to one decimal
  # we suppressWarnings() for the cases where all d$clay are NA (O horizons, bedrock)
  suppressWarnings(clay.mean <- round(mean(d$clay, na.rm=T),1))
  
  # calculate standard deviation of clay content, removing NA and rounding to one decimal
  suppressWarnings(clay.sd <- round(sd(d$clay, na.rm=T),1))
  
  # calculate min clay content, removing NA
  suppressWarnings(clay.min <- min(d$clay, na.rm=T))
  
  # calculate max clay content, removing NA
  suppressWarnings(clay.max <- max(d$clay, na.rm=T))
  
  # calculate some selected quantiles (5th, median, 95th)
  suppressWarnings(clay.q <- quantile(d$clay, 
                                      probs=c(0.05,0.5,0.95), 
                                      na.rm=T)) 
  
  # What other summary statistics could you calculate? 
  # e.g. quantile() for use 5th 50th 95th percentiles 
  
  # CHECK FOR NON-NaN (NOT a NUMBER) mean result; 
  # if NaN, na.rm removed all records. Return NA
  if(!is.nan(clay.mean)) {
    return(data.frame(claymean=clay.mean, claysd=clay.sd, 
                      claymin=clay.min, claymax=clay.max,
                      clayq5=clay.q[1], clayq50=clay.q[2], 
                      clayq95=clay.q[3], n.obs=length(d$clay)))
  } else { 
    return(data.frame(claymean=NA, claysd=NA, 
                      claymin=NA, claymax=NA, 
                      clayq5=NA, clayq50=NA, 
                      clayq95=NA, n.obs=length(d$clay)))
  }
})

# take each list element (a data frame) and rbind them together to make one data frame
res.df <- do.call('rbind', res)

# show results
res.df

## # save a text-based (comma-separated) version of the result table
## write.csv(res.df, file = "Your_RIC_table_output.csv")
## 
## # save a binary file representation of the R object containing result table
## save(res.df, file = "Your_RIC_table_output.Rda")

# set output path
rules.file <- 'C:/data/horizon_agg.txt'

# write blank output (gets rid of any old assignments saved in the file)
write.table(data.frame(), file=rules.file, row.names=FALSE,
            quote=FALSE, na='', col.names=FALSE, sep='|')

# extract horizon data.frame
h <- horizons(pedons)

# strip-out 'not-used' genhz labels and retain horizon ID and genhz assignment
h <- h[which(h$newgenhz != 'not-used'), c('phiid', 'newgenhz')]

# append to NASIS import file
write.table(h, file=rules.file, row.names=FALSE, quote=FALSE,
            na='', col.names=FALSE, sep='|', append=TRUE)

## # then load data from the NASIS selected set into an R object called `pedons`
## pedons <- fetchNASIS(from='pedons')

## # optionally subset the data, FOR INSTANCE: by taxon name - replace Loafercreek with your taxon name
## pedons <- pedons[grep(pattern='Loafercreek', x = f$taxonname, ignore.case=TRUE), ]
