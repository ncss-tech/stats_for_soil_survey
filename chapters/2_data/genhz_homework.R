knitr::opts_chunk$set(echo = TRUE)

library(aqp)
library(soilDB)

# load sample `loafercreek` data from the soilDB package
data("loafercreek")

## STEP 2
# keep only the first 20 pedons
pedons <- loafercreek[1:20, ]

# plot profile sketches
par(mar=c(0,0,2,1))
plot(pedons, name='hzname', print.id=FALSE)

## # after loading your data as a SoilProfileCollection, save it
## save(pedons, "my_pedons.Rda")

## STEP 3

# tabulate hzname
table(pedons$hzname)

# these are the _unique_ horizon designations in our subset `pedons`
unique(pedons$hzname)

l <- fetchOSD('loafercreek')
l$hzname

## STEP 4

# create 4 GHLs: A, upper transitional, argillic and bedrock
prototype.labels <- c('A',
                      'BA',
                      'Bt',
                      'Cr')

## STEP 5

# REGEX rules describing mapping from field data to prototype.labels
patterns.to.match <- c('^A',
                      '^B[^Ct]*$',
                      '.*B.*t.*',
                      'Cr|R')

# this aqp function applies prototype labels to horizons matching `pat`
pedons$newgenhz <- generalize.hz(x=pedons$hzname, new=prototype.labels, pat=patterns.to.match)

oldvsnew <- addmargins(table(pedons$newgenhz, pedons$hzname))
oldvsnew

# find which columns are greater than zero in row 'not-used'
col.idx.not.used <- which(oldvsnew['not-used',] > 0)

# what column indexes (field horizon designations) did not get mapped onto a row (generalized hz label)?
col.idx.not.used

# show just those columns
oldvsnew[, col.idx.not.used]

## REPEAT STEPS 4 AND 5

# create 5 generalized horizons: A, upper transitional, argillic, lower-transitional and bedrock
prototype.labels.v2 <- c('A',
                         'BA',
                         'Bt',
                         'BC',
                         'Cr')

# REGEX rules describing mapping from field data to prototype.labels
patterns.to.match.v2 <- c('^A',
                          '^B[^Ct]*$',
                          '.*B.*t.*',
                          'C[^t]*',
                          'Cr|R')

# use generalize.hz() to apply a set of patterns and paired labels
# to the `pedons$hzname` character vector containing field designations
pedons$newgenhz2 <- generalize.hz(x=pedons$hzname, new=prototype.labels.v2, pat=patterns.to.match.v2)

## REPEAT STEP 6

# create a second cross-tabulation, using the updated genhz
oldvsnew2 <- addmargins(table(pedons$newgenhz2, pedons$hzname))

# find which table columns are greater than zero in row 'not-used'
col.idx.not.used <- which(oldvsnew2['not-used',] > 0)

# show just those columns
oldvsnew2[, col.idx.not.used]

## # check for equality (assignment 1 versus assignment 2)
## pedons$newgenhz == pedons$newgenhz2

## RESULT #1

# plot profile sketches - first 20 profiles; color by gen hz.
par(mar=c(0,0,3,1))
plotSPC(pedons, name='hzname', color='newgenhz2', print.id=FALSE)

# original field data (27 levels)
length(unique(pedons$hzname))

# new generalized data (6 levels, including not-used)
length(unique(pedons$newgenhz2))

## STEP 9

# get the horizon data frame out of the SPC
hzdata <- horizons(pedons)

# make a list of data.frame from horizons, 
# split based on the GHLs (`f`)
genhz.list <- split(hzdata, f = hzdata$newgenhz2)

# use lapply() to apply a function to each element of `genhz.list`
#  anonymous function calcs some summary statistics on each subset dataframe (`d`)
res <- lapply(genhz.list, FUN = function(d) {
  # the variable 'd' contains the dataframe with all data for a GHL
  
  # calculate mean clay content, remove NA, round to one decimal
  suppressWarnings(clay.mean <- round(mean(d$clay, na.rm=TRUE),1))
  # note :suppressWarnings() for cases where all d$clay are NA (O horizons, bedrock)
  
  # calculate standard deviation of clay content, remove NA, round to one decimal
  suppressWarnings(clay.sd <- round(sd(d$clay, na.rm=TRUE),1))
  
  # calculate min clay content, removing NA
  suppressWarnings(clay.min <- min(d$clay, na.rm=TRUE))
  
  # calculate max clay content, removing NA
  suppressWarnings(clay.max <- max(d$clay, na.rm=TRUE))
  
  # calculate some selected quantiles (5th, median, 95th)
  suppressWarnings(clay.q <- quantile(d$clay, 
                                      probs=c(0.05,0.5,0.95), 
                                      na.rm=TRUE)) 
  
  # What other summary statistics could you calculate? 
  
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

## RESULT #2

# show result table
res.df

## # save result #2 to file
## 
## # save a text-based (comma-separated) version of the result table
## write.csv(res.df, file = "Your_RIC_table_output.csv")
## 
## # save a binary file representation of the R object containing result table
## save(res.df, file = "Your_RIC_table_output.Rda")

## # set output path
## genhz.file <- 'C:/data/horizon_agg.txt'
## 
## # update genhz.var if you change the site(pedons) column with labels
## genhz.var <- 'newgenhz'
## 
## # write blank output (gets rid of any old assignments saved in the file)
## write.table(data.frame(), file=genhz.file, row.names=FALSE,
##             quote=FALSE, na='', col.names=FALSE, sep='|')
## 
## # extract horizon data.frame
## h <- horizons(pedons)
## 
## # strip-out 'not-used' genhz labels and retain horizon ID and genhz assignment
## h <- h[which(h[[genhz.var]] != 'not-used'), c('phiid', genhz.var)]
## 
## # append to NASIS import file
## write.table(h, file=rules.file, row.names=FALSE, quote=FALSE,
##             na='', col.names=FALSE, sep='|', append=TRUE)

## # after updating genhz, save a new copy of the data
## save(pedons, "my_pedons_genhz.Rda")

## # then load data from the NASIS selected set into an R object called `pedons`
## pedons <- fetchNASIS(from='pedons')

## # optionally subset the data, FOR INSTANCE: by taxon name - replace Loafercreek with your taxon name
## pedons <- pedons[grep(pattern='Loafercreek', x = f$taxonname, ignore.case=TRUE), ]
