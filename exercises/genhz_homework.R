library(aqp, warn.conflicts = FALSE)
library(soilDB)

## STEP 1 ----

# load sample `loafercreek` data from the soilDB package
data("loafercreek")

## STEP 2  ----
# keep only the first 20 pedons
pedons <- loafercreek[1:20, ]

# plot profile sketches
par(mar = c(0, 0, 2, 1))
plot(pedons, name = 'hzname', print.id = FALSE)

# after loading your data as a SoilProfileCollection, save it
save(pedons, file = "my_pedons.Rda")

## STEP 3 ----

# tabulate hzname
table(pedons$hzname)

# these are the _unique_ horizon designations in our subset `pedons`
unique(pedons$hzname)

l <- fetchOSD('loafercreek')
l$hzname

## STEP 4 ----

# create 4 GHLs: A, upper transitional, argillic and bedrock
prototype.labels <- c('A',
                      'BA',
                      'Bt',
                      'Cr')

## STEP 5 ----

# REGEX rules describing mapping from field data to prototype.labels
patterns.to.match <- c('^A',
                       '^B[^Ct]*$',
                       'B.*t',
                       'Cr|R')

# apply prototype labels `new` to horizons matching `pat`
pedons$newgenhz <- generalize.hz(x = pedons$hzname, new = prototype.labels, pat = patterns.to.match)

## STEP 6 ----
# cross-tabulate results
oldvsnew <- addmargins(table(pedons$newgenhz, pedons$hzname))
oldvsnew

## STEP 7 ----
# find which columns are greater than zero in row 'not-used'
col.idx.not.used <- which(oldvsnew['not-used',] > 0)

# what column indexes (field horizon designations) did not get mapped onto a row (generalized hz label)?
col.idx.not.used

# show just those columns
oldvsnew[, col.idx.not.used]

## REPEAT STEPS 4 AND 5 ----

# create 5 generalized horizons: A, upper transitional, argillic, lower-transitional and bedrock
prototype.labels.v2 <- c('A',
                         'BA',
                         'Bt',
                         'BC',
                         'Cr')

# REGEX rules describing mapping from field data to prototype.labels
patterns.to.match.v2 <- c('^A',
                          '^B[^Ct]*$',
                          'B.*t',
                          'C[^t]*',
                          'Cr|R')

# use generalize.hz() to apply a set of patterns and paired labels
# to the `pedons$hzname` character vector containing field designations
pedons$newgenhz2 <- generalize.hz(x = pedons$hzname,
                                  new = prototype.labels.v2,
                                  pat = patterns.to.match.v2)

## REPEAT STEP 6 ----

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
par(mar = c(0, 0, 3, 1))
plotSPC(pedons,
        name = 'hzname',
        color = 'newgenhz2',
        print.id = FALSE)

# original field data (27 levels)
length(unique(pedons$hzname))

# new generalized data (6 levels, including not-used)
length(unique(pedons$newgenhz2))

## STEP 9 ----

# get the horizon data frame out of the SPC
hzdata <- horizons(pedons)

library(dplyr, warn.conflicts = FALSE)

# summarize horizon groups with single summary statistics 
#   using mean, sd, min, max, quantile
res_df <- hzdata %>% 
            group_by(newgenhz2) %>%
            summarize(clay_mean = mean(clay, na.rm = TRUE),
                      clay_sd = sd(clay, na.rm = TRUE),
                      clay_min = min(clay, na.rm = TRUE),
                      clay_max = max(clay, na.rm = TRUE),
                      clay_Q05 = quantile(clay, probs = 0.05, na.rm = TRUE),
                      clay_Q50 = quantile(clay, probs = 0.5, na.rm = TRUE),
                      clay_Q95 = quantile(clay, probs = 0.95, na.rm = TRUE))

## res_df

knitr::kable(res_df, caption = "Summary Statistics for Generalized Horizons")

## # save result #2 to file
## 
## # save a text-based (comma-separated) version of the result table
## write.csv(res_df, file = "Your_RIC_table_output.csv")
## 
## # save a binary file representation of the R object containing result table
## save(res_df, file = "Your_RIC_table_output.Rda")

## # set output path
## genhz.file <- 'C:/data/horizon_agg.txt'
## 
## # update genhz.var if you change the site(pedons) column with labels
## genhz.var <- 'newgenhz'
## 
## # write blank output (gets rid of any old assignments saved in the file)
## write.table(
##   data.frame(),
##   file = genhz.file,
##   row.names = FALSE,
##   quote = FALSE,
##   na = '',
##   col.names = FALSE,
##   sep = '|'
## )
## 
## # extract horizon data.frame
## h <- horizons(pedons)
## 
## # strip-out 'not-used' genhz labels and retain horizon ID and genhz assignment
## h <- h[which(h[[genhz.var]] != 'not-used'), c('phiid', genhz.var)]
## 
## # append to NASIS import file
## write.table(
##   h,
##   file = genhz.file,
##   row.names = FALSE,
##   quote = FALSE,
##   na = '',
##   col.names = FALSE,
##   sep = '|',
##   append = TRUE
## )

# after updating genhz, save a new copy of the data
save(pedons, file = "my_pedons_genhz.Rda")

## # then load data from the NASIS selected set into an R object called `pedons`
## pedons <- fetchNASIS(from = 'pedons')

## # optionally subset the data, FOR INSTANCE: by taxon name - replace Loafercreek with your taxon name
## pedons <- pedons[grep(pattern = 'Loafercreek', x = f$taxonname, ignore.case = TRUE),]
