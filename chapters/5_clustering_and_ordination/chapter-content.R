# setup
library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)

opts_chunk$set(message=FALSE, warning=FALSE, background='#F7F7F7', fig.align='center', fig.retina=2, dev='png', tidy=FALSE, verbose=FALSE, antialias='cleartype', cache=FALSE)

# options for R functions
options(width=100, stringsAsFactors=FALSE)

# captions added to figures
knit_hooks$set(htmlcap = function(before, options, envir) {
  if(!before) {
    paste('<p class="caption" style="font-size:85%; font-style: italic; font-weight: bold;">',options$htmlcap,"</p><hr>",sep="")
    }
    })

# compaire pair-wise distances between 3 individuals
distPlot <- function(ex, vars, individuals, id, scale.data=FALSE, show.distances=TRUE, ...) {
  par(mar=c(5,5,1,1))
  # optionally scale
  if(scale.data) {
    ex.scaled <- scale(ex[, vars], center = TRUE, scale = TRUE)
    ex[[vars[1]]] <- ex.scaled[, 1]
    ex[[vars[2]]] <- ex.scaled[, 2]
  }
    
  
  x.data <- ex[[vars[1]]]
  y.data <- ex[[vars[2]]]
  
  d <- dist(ex[, vars])
  m <- round(as.matrix(d), 1)
  dimnames(m) <- list(ex[[id]], ex[[id]])
  
  plot(ex[[vars[1]]], ex[[vars[2]]], las=1, type='n', ...)
  # plot(x.data, y.data, las=1, type='n')
  grid()
  
  if(show.distances) {
    arrows(x.data[individuals[1]], y.data[individuals[1]], x.data[individuals[2]], y.data[individuals[2]], lwd=2, col='RoyalBlue', length = 0.1, code = 3)
    arrows(x.data[individuals[3]], y.data[individuals[3]], x.data[individuals[2]], y.data[individuals[2]], lwd=2, col='Orange', length = 0.1, code = 3)
    
    segments(x.data[individuals[3]], y.data[individuals[3]], x.data[individuals[3]], y.data[individuals[2]], lwd=1, lty=2, col='Orange')
    segments(x.data[individuals[3]], y.data[individuals[2]], x.data[individuals[2]], y.data[individuals[2]], lwd=1, lty=2, col='Orange')
    segments(x.data[individuals[1]], y.data[individuals[1]], x.data[individuals[2]], y.data[individuals[1]], lwd=1, lty=2, col='RoyalBlue')
    segments(x.data[individuals[2]], y.data[individuals[1]], x.data[individuals[2]], y.data[individuals[2]], lwd=1, lty=2, col='RoyalBlue')
    
    legend('topright', legend=c(m[individuals[1], individuals[2]], m[individuals[3], individuals[2]]), col=c('RoyalBlue', 'Orange'), lty=1, lwd=2, bty='n', title = 'Distance', cex=1.5)
  }
  
  text(x.data, y.data, ex[[id]], col='black', cex=1.5, font=1, pos = 4)
  points(x.data, y.data, pch=16, col='black', cex=0.75)
  
  return(m)
}

# load libs for examples
library(aqp)
library(cluster)
library(ape)
library(RColorBrewer)
library(MASS)
library(lattice)
library(viridis)

data('sp4', package = 'aqp')
sp4 <- sp4[1:4, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
kable_styling(kable(sp4, align = 'c', format='html'), full_width = FALSE)

m <- distPlot(sp4, vars=c('sand', 'clay'), individuals=c(1,2,4), id='name', xlim=c(10, 60), ylim=c(10, 60), xlab='Sand (%)', ylab='Clay (%)')

m <- distPlot(sp4, vars=c('Ca', 'clay'), individuals=c(1,2,4), id='name', xlim=c(0, 60), ylim=c(0, 60), xlab='Exchangeable Ca (cmol/kg)', ylab='Clay (%)')

sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))
kable_styling(kable(sp4.scaled, align = 'c', format='html'), full_width = FALSE)

m <- distPlot(sp4, vars=c('Ca', 'clay'), individuals=c(1,2,4), id='name', scale=TRUE, xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5), xlab='Exchangeable Ca (cmol/kg)', ylab='Clay (%)')

d <- dist(sp4.scaled[, -1])
m <- as.matrix(d)
dimnames(m) <- list(sp4.scaled$name, sp4.scaled$name)

# remove upper triangle
m[upper.tri(m)] <- NA

# "nice" colors
cols <- viridis(10)
col.palette <- colorRampPalette(cols)

levelplot(round(m, 2), col.regions=col.palette, colorkey=list(tick.number=10), xlab='', ylab='', main='Pair-Wise Distances', scales=list(alternating=3), panel=function(x, y, z, ...) {
  panel.levelplot(x, y, z, ...)
  idx <- which(!is.na(z))
  panel.text(x[idx], y[idx], z[idx], font=2, col='red')
  panel.abline(h=seq(from=0.5, to=length(y), by=1), col=grey(0.45))
  panel.abline(v=seq(from=0.5, to=length(x), by=1), col=grey(0.45))
})

m <- m / max(m, na.rm = TRUE)
levelplot(round(m, 2), col.regions=col.palette, colorkey=list(tick.number=10), xlab='', ylab='', main='Pair-Wise Distances', scales=list(alternating=3), panel=function(x, y, z, ...) {
  panel.levelplot(x, y, z, ...)
  idx <- which(!is.na(z))
  panel.text(x[idx], y[idx], z[idx], font=2, col='red')
  panel.abline(h=seq(from=0.5, to=length(y), by=1), col=grey(0.45))
  panel.abline(v=seq(from=0.5, to=length(x), by=1), col=grey(0.45))
})

par(mar=c(1,1,1,0), mfcol=c(1,2))

m <- distPlot(sp4, vars=c('sand', 'clay'), individuals=c(1,2,4), id='name', show.distances=FALSE, xlim=c(10, 60), ylim=c(10, 60), xlab='Sand (%)', ylab='Clay (%)')
d <- as.dist(m)
dd <- diana(d)
h <- as.hclust(dd)
p <- as.phylo(h)

plot(p, font=2, label.offset=0.5, adj=0.5, direction='down', srt=90)
# axis(2, las=1, line=1.5)
mtext('Dendrogram Representation of Distance Matrix\n(sand and clay %)', side=1, line=2)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))

# distance matrix
d <- dist(sp4.scaled[, -1])
m <- as.matrix(d)
dimnames(m) <- list(sp4.scaled$name, sp4.scaled$name)
d <- as.dist(m)
# dendrogram from divisive clustering
dd <- diana(d)
h <- as.hclust(dd)
p <- as.phylo(h)

# define colors based on natural groupings
cols <- brewer.pal(9, 'Set1')[cutree(h, 4)]

par(mar=c(0,0,0,0), mfcol=c(1,2))
plot(p, label.offset=0.125, direction='right', font=1, cex=0.85)
abline(v=2.3, col='orange', lty=2, lwd=2)
# mtext('Dendrogram Representation of Distance Matrix\n(all characteristics, standardized)', side=1, line=1)

plot(p, label.offset=0.125, direction='right', font=1, cex=0.85)
abline(v=2.3, col='orange', lty=2, lwd=2)
tiplabels(pch=15, col=cols)
# mtext('Dendrogram Representation of Distance Matrix\n(all characteristics, standardized)', side=1, line=1)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'Mg', 'Ca')]
sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))

# use pam to generate 3 clusters
cl <- pam(sp4.scaled[, -1], k = 3, stand = FALSE)

# use kmeans to generate centroids for demo
km <- kmeans(sp4.scaled[, -1], 3, nstart = 10, iter.max = 100)

# define colors based on hard clustering
col.set <- brewer.pal(9, 'Set1')
cols <- col.set[cl$clustering]

# setup plot
par(mar=c(4,4,0,0))
plot(sp4.scaled$Mg, sp4.scaled$Ca, asp=1, ylab='Exchangeable Mg (cmol [+]/kg), Standardized', xlab='Exchangeable Ca (cmol [+]/kg), Standardized', type='n')
abline(h=0, v=0, col='black')
grid()

# add k-means centroids
points(km$centers[, 1], km$centers[, 2], cex=2, lwd=1, pch=8, col='black')

# add medoids
points(cl$medoids, col=col.set, lwd=2, cex=3, pch=0, lend=2)

# add original obs
points(sp4.scaled$Mg, sp4.scaled$Ca, bg=cols, col='black', cex=1.25, pch=21)

# legend
legend('topright', legend=c('Observation', 'Medoid', 'Centroid'), pch=c(21, 0, 8), lwd=c(1, 2, 1), pt.cex=c(1.25, 3, 2), lty=NA, bty='n', cex=1.25)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))

# distance matrix
d <- dist(sp4.scaled[, -1])
m <- as.matrix(d)
dimnames(m) <- list(sp4.scaled$name, sp4.scaled$name)
d <- as.dist(m)
# dendrogram from divisive clustering
dd <- diana(d)
h <- as.hclust(dd)
p <- as.phylo(h)

# define colors based on natural groupings
cols <- brewer.pal(9, 'Set1')[cutree(h, 4)]

# Sammon's non-linear mapping
s <- MASS::sammon(d)

par(mar=c(3,0,0,3), mfcol=c(1,2))
plot(p, label.offset=0.125, direction='right', font=1, cex=0.85)
tiplabels(pch=15, col=cols)
abline(v=2.3, col='orange', lty=2, lwd=2)
mtext('Dendrogram Representation of Distance Matrix\n(all characteristics, standardized)', side=1, line=1)

plot(s$points, asp=1, type='n', axes=FALSE, xlab='', ylab='')
# abline(v=0, h=0, col='black')
grid()
text(s$points, rownames(s$points), cex=0.75, col=cols, font=2)
axis(1, cex.axis=0.95, line=-2)
axis(4, cex.axis=0.95, line=0.5, las=1)
mtext('Ordination of Distance Matrix\n(all characteristics, standardized)', side=1, line=1)

# re-init sp4, copy clustering colors to hz attribute
data('sp4', package = 'aqp')
sp4$cl <- cols
depths(sp4) <- id ~ top + bottom

par(mar=c(0,0,0,0))
plot(sp4, color='cl', cex.names=0.75)
title('Clustering Results in Context', line=-1)

library(sharpshootR)
# init example data
data(sp4)
depths(sp4) <- id ~ top + bottom

# eval dissimilarity:
# using Ex-Ca:Mg and CEC at pH 7
# with no depth-weighting (k=0)
# to a maximum depth of 40 cm
d <- profile_compare(sp4, vars=c('ex_Ca_to_Mg', 'CEC_7'), k=0, max_d=40)

# cluster via divisive method
clust <- diana(d)

# vizualize dissimilarity matrix via hierarchical clustering
par(mar=c(0,0,3,0))
plotProfileDendrogram(sp4, clust, dend.y.scale = max(d), scaling.factor = (1/max(d) * 10), y.offset = 2, width=0.15, cex.names=0.45, color='ex_Ca_to_Mg', col.label='Exchageable Ca to Mg Ratio')

# load libraries
library(aqp)
library(soilDB)
library(sharpshootR)
library(cluster)
library(ape)
library(RColorBrewer)
library(vegan)
library(MASS)
library(colorspace)
library(viridis)

# example data
data("gopheridge", package = "soilDB")

# compute data completeness
gopheridge$data.complete <- evalMissingData(gopheridge, vars = c('clay', 'sand', 'phfield'), name = 'hzname', p = 'Cr|R|Cd')

# check range in missing data
summary(gopheridge$data.complete)

# rank
new.order <- order(gopheridge$data.complete)

# plot along data completeness ranking
par(mar=c(3,0,1,1))
plot(gopheridge, plot.order=new.order, print.id=FALSE, name='')

# add axis, note re-ordering of axis labels
axis(side=1, at=1:length(gopheridge), labels = round(gopheridge$data.complete[new.order], 2),
line=-2, cex.axis=0.65, las=2)

title('Gopheridge pedons sorted according to data completeness (clay, sand, pH)')

# view missing data as a fraction
res <- missingDataGrid(gopheridge, max_depth=100, vars=c('clay', 'sand', 'phfield'), filter.column='hzname', filter.regex = 'Cr|R|Cd', main='Fraction of missing data (clay, sand, pH)', cols = viridis(10))

# check results
head(res)

# be sure to read the manual page for this function
gopheridge.complete <- subsetProfiles(gopheridge, s = "data.complete > 0.99")

# another way
idx <- which(gopheridge$data.complete > 0.99)
gopheridge.complete <- gopheridge[idx, ]

# looks good
par(mar=c(0,0,3,1))
plot(gopheridge.complete, color='clay', id.style='side', label='pedon_id')

# get some example data from the aqp package
data('sp4', package = 'aqp')
# subset select rows and columns
sp4 <- sp4[1:4, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
row.names(sp4) <- sp4$name

# compare distance functions

# Euclidean distance, no standardization
round(dist(sp4[, -1], method = 'euclidean'))

# Euclidean distance, standardization
round(daisy(sp4[, -1], stand = TRUE, metric = 'euclidean'), 2)

# Gower's generalized distance metric (includes standardization)
round(vegdist(sp4[, -1], method = 'gower'), 2)

# load some example NASIS data
data(loafercreek, package='soilDB')

# cut-down to a subset, first 20 pedons
loafercreek <- loafercreek[1:20, ]

# get depth class
sdc <- getSoilDepthClass(loafercreek)
site(loafercreek) <- sdc

# diagnostic properties to consider, no need to convert to factors
v <- c('lithic.contact', 'paralithic.contact', 'argillic.horizon',
       'cambic.horizon', 'ochric.epipedon', 'mollic.epipedon', 'very.shallow',
       'shallow', 'mod.deep', 'deep', 'very.deep')

# do the analysis and save the results to object 'x'
x <- diagnosticPropertyPlot(loafercreek, v, k=5, grid.label='bedrckkind', dend.label = 'taxonname')

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]

# distance matrix
d <- daisy(sp4[, -1], metric = 'euclidean', stand = TRUE)

# hierachical clustering with base function hclust
sp4.h <- hclust(d, method = 'ward.D')
sp4.h$labels <- sp4$name

# plot with basic plotting method... not many options here
par(mar=c(2,4,2,2))
plot(sp4.h, font=2, cex=0.85)

# ID clusters after cutting tree
rect.hclust(sp4.h, 4)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]

# distance matrix
d <- daisy(sp4[, -1], metric = 'euclidean', stand = TRUE)

# divising clustering
dd <- diana(d)

# convert to ape class, via hclust class
h <- as.hclust(dd)
h$labels <- sp4$name
p <- as.phylo(h)

# define some nice colors
col.set <- brewer.pal(9, 'Set1')

# cut tree into 4 groups
groups <- cutree(h, 4)

# make color vector based on groups
cols <- col.set[groups]

par(mar=c(1,1,1,1), mfcol=c(2,2))
plot(p, label.offset=0.125, direction='right', font=1, cex=0.85, main='dendrogram')
tiplabels(pch=15, col=cols)

plot(p, type='radial', font=1, cex=0.85, main='radial')
tiplabels(pch=15, col=cols)

plot(p, type='fan', font=1, cex=0.85, main='fan')
tiplabels(pch=15, col=cols)

plot(p, type='unrooted', font=1, cex=0.85, main='unrooted')
tiplabels(pch=15, col=cols)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]

# distance matrix
d <- daisy(sp4[, -1], metric = 'euclidean', stand = TRUE)

# hierarchical clustering based on several strategies
# agglomerative
h.avg <- agnes(d, method='average')
h.single <- agnes(d, method='single')
h.complete <- agnes(d, method='complete')
h.ward <- agnes(d, method='ward')
h.flexible <- agnes(d, method='gaverage', par.method = 0.01)

# divisive
h.div <- diana(d)

# agglomerative hierarchical clustering with various linkage criteria
corr.avg <- cor(d, cophenetic(h.avg))
corr.single <- cor(d, cophenetic(h.single))
corr.complete <- cor(d, cophenetic(h.complete))
corr.ward <- cor(d, cophenetic(h.ward))
corr.flexible <- cor(d, cophenetic(h.flexible))

# divisive hierarchical clustering
corr.div <- cor(d, cophenetic(h.div))

res <- data.frame(
  method=c('average', 'single', 'complete', 'ward', 'flexible UPGMA', 'divisive'), 
  cophenetic.correlation=c(corr.avg, corr.single, corr.complete, corr.ward, corr.flexible, corr.div),
  agg_div.coefficient=c(h.avg$ac, h.single$ac, h.complete$ac, h.ward$ac, h.flexible$ac, h.div$dc)
)

# re-order
res <- res[order(res$cophenetic.correlation, decreasing = TRUE), ]

kable_styling(kable(res, row.names = FALSE, format = 'html', digits = 3), full_width = FALSE)

par(mar=c(0,0.25,1.5,0.25), mfrow=c(2,3))

plot(as.phylo(as.hclust(h.avg)), label.offset=0.125, direction='right', font=1, cex=0.65, main='Average')
tiplabels(pch=15, col=col.set[cutree(h.avg, 4)])

plot(as.phylo(as.hclust(h.single)), label.offset=0.125, direction='right', font=1, cex=0.65, main='Single')
tiplabels(pch=15, col=col.set[cutree(h.single, 4)])

plot(as.phylo(as.hclust(h.complete)), label.offset=0.125, direction='right', font=1, cex=0.65, main='Complete')
tiplabels(pch=15, col=col.set[cutree(h.complete, 4)])

plot(as.phylo(as.hclust(h.ward)), label.offset=0.125, direction='right', font=1, cex=0.65, main='Ward')
tiplabels(pch=15, col=col.set[cutree(h.ward, 4)])

plot(as.phylo(as.hclust(h.flexible)), label.offset=0.125, direction='right', font=1, cex=0.65, main='Flexible (0.01)')
tiplabels(pch=15, col=col.set[cutree(h.flexible, 4)])

plot(as.phylo(as.hclust(h.div)), label.offset=0.125, direction='right', font=1, cex=0.65, main='Divisive')
tiplabels(pch=15, col=col.set[cutree(h.div, 4)])

# init a sequence spanning -1 to 1
beta <- seq(from=-1, to=1, length.out = 100)
# init an empty vector to store results
r <- vector(mode='numeric', length = length(beta))

# iterate over values of beta and compute cophenetic correlation
for(i in 1:length(r)) {
  r[i] <- cor(d, cophenetic(agnes(d, method='gaverage', par.method = beta[i])))
}

# plot
par(mar=c(5,5,1,1))
plot(beta, r, type='l', xlab='beta parameter', ylab='cophenetic correlation', pch=16, las=1)
# locate the max coph. corr.
idx <- which.max(r)
# mark this point and label
points(beta[idx], r[idx], col='red', cex=2, lwd=2)
text(beta[idx], r[idx], labels = round(beta[idx], 3), pos=4)

# load sample dataset from aqp package
data(sp3)

# promote to SoilProfileCollection
depths(sp3) <- id ~ top + bottom

# compute dissimilarity using different sets of variables
# note that these are rescaled to the interval [0,1]
d.1 <- profile_compare(sp3, vars=c('clay', 'L'), k=0, max_d=100, rescale.result=TRUE)

# cluster via divisive hierarchical algorithm
# convert to 'phylo' class
p.1 <- as.phylo(as.hclust(diana(d.1)))

# graphically compare diana() to agnes() using d.2
dueling.dendrograms(as.phylo(as.hclust(diana(d.1))), 
as.phylo(as.hclust(agnes(d.1, method='average'))), lab.1='divisive', lab.2='agglomerative: average linkage')

# nice colors for later
col.set <- brewer.pal(9, 'Set1')

# 2D example
x <- rbind(matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")

par(mfrow=c(3,3), mar=c(1,1,1,1))
for(i in 1:9) {
  cl <- kmeans(x, centers=3)
  plot(x, col = col.set[cl$cluster], axes=FALSE)
  grid()
  points(cl$centers, col = col.set, pch = 8, cex = 2, lwd=2)
  box()
}

par(mfrow=c(3,3), mar=c(1,1,1,1))
for(i in 1:9) {
  cl <- kmeans(x, centers=3, nstart = 10, iter.max = 100)
  plot(x, col = col.set[cl$cluster], axes=FALSE)
  grid()
  points(cl$centers, col = col.set, pch = 8, cex = 2, lwd=2)
  box()
}

par(mfrow=c(2,3), mar=c(1,1,1,1))
for(i in 2:7) {
  cl <- pam(x, k = i, stand = TRUE)
  plot(x, col = col.set[cl$clustering], axes=FALSE)
  grid()
  points(cl$medoids, col = col.set, pch = 0, cex = 2, lwd=2)
  box()
}

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4.std <- data.frame(sp4[, c('id', 'name', 'top', 'bottom')], scale( sp4[, c('Mg', 'Ca')]))

# perform fuzzy clustering
cl <- fanny(sp4.std[, c('Mg', 'Ca')], k = 3, stand = FALSE)

# get membership matrix
m <- cl$membership

# convert to colors by interpreting membership as R,G,B proportions
cols <- rgb(m)

# setup plot
par(mar=c(4,4,0,0))
plot(sp4.std$Mg, sp4.std$Ca, asp=1, ylab='Exchangeable Mg (cmol/kg), Standardized', xlab='Exchangeable Ca (cmol/kg), Standardized', type='n')
abline(h=0, v=0, col='black')
grid()

# add original obs
points(sp4.std$Mg, sp4.std$Ca, bg=cols, col='black', cex=1.5, pch=21)

sp4.std$colors <- cols
depths(sp4.std) <- id ~ top + bottom

par(mar=c(0,0,0,0))
plot(sp4.std, color='colors', cex.names=0.75)
title('Fuzzy Clustering Results in Context', line=-1)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4.std <- data.frame(sp4[, c('id', 'name', 'top', 'bottom')], scale( sp4[, c('Mg', 'Ca')]))

# perform hard clustering
sil.widths <- vector(mode='numeric')
for(i in 2:10) {
  cl <- pam(sp4.std[, c('Mg', 'Ca')], k = i, stand = FALSE)
  sil.widths[i] <- cl$silinfo$avg.width
}

par(mar=c(4,4,3,1))
plot(sil.widths, type='b', xlab='Number of Clusters', ylab='Average Silhouette Width', las=1, lwd=2, col='RoyalBlue', cex=1.25, main='Finding the "Right" Number of Clusters')
grid()

# perform fuzzy clustering
cl <- pam(sp4.std[, c('Mg', 'Ca')], k = 3, stand = FALSE)

# setup plot
par(mar=c(4,4,0,0))
plot(sp4.std$Mg, sp4.std$Ca, asp=1, ylab='Exchangeable Mg (cmol/kg), Standardized', xlab='Exchangeable Ca (cmol/kg), Standardized', type='n')
abline(h=0, v=0, col='black')
grid()

# add original obs
points(sp4.std$Mg, sp4.std$Ca, bg=cl$clustering, col='black', cex=1.25, pch=21)

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))

# PCA
# note that we are leaving out the first column: the horizon names
# note the syntax used to extract the principal components
# note that PCA doesn't use the distance matrix
pca <- predict(princomp(sp4.scaled[, -1]))

## perform clustering to highlight structure in the PCA
# distance matrix
d <- dist(sp4.scaled[, -1])
m <- as.matrix(d)
dimnames(m) <- list(sp4.scaled$name, sp4.scaled$name)
d <- as.dist(m)
# dendrogram from divisive clustering
dd <- diana(d)
h <- as.hclust(dd)
p <- as.phylo(h)

# define colors based on cutting a divisive hierarchical clustering into 4 groups
cols <- brewer.pal(9, 'Set1')[cutree(h, 4)]

# plot first 2 PC
plot(pca[, 1:2], asp=1, type='n', axes=FALSE, xlab='', ylab='', main="Principal Components 1 and 2")
grid()
text(pca[, 1:2], sp4.scaled$name, cex=0.75, col=cols, font=2)
box()

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))

# distance matrix
d <- dist(sp4.scaled[, -1])
m <- as.matrix(d)
dimnames(m) <- list(sp4.scaled$name, sp4.scaled$name)
d <- as.dist(m)
# dendrogram from divisive clustering
dd <- diana(d)
h <- as.hclust(dd)
p <- as.phylo(h)

# define colors based on cutting a divisive hierarchical clustering into 4 groups
cols <- brewer.pal(9, 'Set1')[cutree(h, 4)]

# nMDS from distance matrix
s <- sammon(d)

# plot
par(mar=c(3,1,3,1))
plot(s$points, asp=1, type='n', axes=FALSE, xlab='', ylab='', main="nMDS by Sammon's Non-Linear Mapping")
# abline(v=0, h=0, col='black')
grid()
text(s$points, rownames(s$points), cex=0.75, col=cols, font=2)
box()

# re-make data, this time with all profiles
data('sp4', package = 'aqp')
sp4 <- sp4[, c('name', 'clay', 'sand', 'Mg', 'Ca', 'CEC_7')]
sp4.scaled <- data.frame(name=sp4[, 1], round(scale( sp4[, -1]), 2))

# define colors based on natural groupings
cols <- brewer.pal(9, 'Set1')

# distance calc + ordination
s <- metaMDS(sp4.scaled[, -1], distance = 'gower', autotransform = FALSE, wascores=FALSE)

## this is used to generate 4 classes from a divisive hierarchical clustering
# manually compute distance matrix
d <- dist(sp4.scaled[, -1])
m <- as.matrix(d)
dimnames(m) <- list(sp4.scaled$name, sp4.scaled$name)
d <- as.dist(m)
# dendrogram from divisive clustering
dd <- diana(d)
h <- as.hclust(dd)

# define colors based on cutting a divisive hierarchical clustering into 4 groups
cols <- brewer.pal(9, 'Set1')[cutree(h, 4)]

# plot ordination
par(mar=c(3,1,3,1))
fig <- ordiplot(s, type='none', cex.axis=0.75, axes=FALSE, xlab='', ylab='', main='nMDS by metaMDS()')
abline(h=0, v=0, lty=2, col='grey')
text(fig$sites, sp4$name, cex=0.75, font=2, col=cols)
# ordicluster(fig, agnes(daisy(sp4.scaled[, -1]), method='ward'), prune=3, col = "orange")
box()

# init example data
data(sp4)
depths(sp4) <- id ~ top + bottom

# eval dissimilarity:
# using Ex-Ca:Mg and CEC at pH 7
# with no depth-weighting (k=0)
# to a maximum depth of 40 cm
d <- profile_compare(sp4, vars=c('ex_Ca_to_Mg', 'CEC_7'), k=0, max_d=40)

# check distance matrix:
round(d, 1)

# cluster via divisive method
clust <- diana(d)

# vizualize dissimilarity matrix via hierarchical clustering
par(mar=c(0,0,3,0))
plotProfileDendrogram(sp4, clust, dend.y.scale = max(d), scaling.factor = (1/max(d) * 10), y.offset = 2, width=0.15, cex.names=0.45, color='ex_Ca_to_Mg', col.label='Exchageable Ca to Mg Ratio')

# define a vector of series
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'drummer', 'musick', 'zook', 'argonaut', 'PALAU')

# get and SPC object with basic data on these series
s <- fetchOSD(s.list)

# graphical check
par(mar=c(0,0,2,0))
plot(s) ; title('Selected Pedons from Official Series Descriptions', line=0)

# check structure of some site-level attributes
head(site(s))[, c('id', 'soilorder', 'suborder', 'greatgroup', 'subgroup')]

par(mar=c(0,1,1,1))
# plot dendrogram + profiles
d <- SoilTaxonomyDendrogram(s, scaling.factor = 0.01)

## print(d)

# manually convert Munsell -> sRGB
rgb.data <- munsell2rgb(s$hue, s$value, s$chroma, return_triplets = TRUE)
s$r <- rgb.data$r
s$g <- rgb.data$g
s$b <- rgb.data$b

# eval color signature
pig <- soilColorSignature(s, RescaleLightnessBy = 5, method='depthSlices')

# display results as table
knitr::kable(head(pig), digits = 3, row.names = FALSE)

# move row names over for distance matrix
row.names(pig) <- pig[, 1]
d <- daisy(pig[, -1])
dd <- diana(d)

# plot
par(mar=c(0,0,0.25,1))
plotProfileDendrogram(s, dd, dend.y.scale = max(d) * 2, scaling.factor = 0.3, y.offset = 6, width=0.2, cex.names=0.45)

# extract horizon data from select OSDs in above example
h <- horizons(s)

# convert Munsell color notation to sRGB
# these are moist colors
rgb.data <- munsell2rgb(h$hue, h$value, h$chroma, return_triplets = TRUE)

# check
head(rgb.data)

# remove NA
rgb.data <- na.omit(rgb.data)

# retain unique colors
rgb.data <- unique(rgb.data)

# convert sRGB colors to CIE LAB color system
lab.data <- as(with(rgb.data, sRGB(r, g, b)), 'LAB')

# visualize colors in LAB coordinates
pairs(lab.data@coords, col='white', bg=rgb(rgb.data), pch=21, cex=2)

# create distance matrix from LAB coordinates
d <- daisy(lab.data@coords, stand = TRUE)

# divisive heirarcical clustering
d.hclust <- as.hclust(diana(d))

# convert to phylo class for nicer plotting
p <- as.phylo(d.hclust)

# perform nMDS on distance matrix
d.sammon <- sammon(d)

# setup multi-figure page
par(mfcol=c(1,2), mar=c(0,0,2,0), bg=grey(0.95))

# plot fan-style dendrogram
plot(p, font=2, cex=0.5, type='fan', show.tip.label=FALSE, main='Dendrogram Representation')
# add colors at dendrogram tips
tiplabels(pch=21, cex=4, col='white', bg=rgb(rgb.data))

# plot nMDS ordination
plot(d.sammon$points, type='n', axes=FALSE, xlab='', ylab='', asp=1, main='nMDS Ordination')
abline(h=0, v=0, col='black', lty=3)
points(d.sammon$points, bg=rgb(rgb.data), pch=21, cex=3.5, col='white')

library(reshape2)
# set list of component names, same as soil color example
s.list <- c('amador', 'redding', 'pentz', 'willows', 'pardee', 'yolo', 
            'hanford', 'cecil', 'sycamore', 'KLAMATH', 'MOGLIA', 'drummer', 
            'musick', 'zook', 'argonaut', 'PALAU')

# set list of relevant interpretations
interp.list <- c('ENG - Construction Materials; Topsoil', 
'ENG - Sewage Lagoons', 'ENG - Septic Tank Absorption Fields', 
'ENG - Unpaved Local Roads and Streets')

# compose query
q <- paste0("SELECT UPPER(compname) as compname, mrulename, AVG(interplr) as interplr_mean
FROM component INNER JOIN cointerp ON component.cokey = cointerp.cokey
WHERE compname IN ", format_SQL_in_statement(s.list), "
AND seqnum = 0
AND mrulename IN ", format_SQL_in_statement(interp.list), "
AND interplr IS NOT NULL
GROUP BY compname, mrulename;")

# send query
x <- SDA_query(q)

# reshape long -> wide
x.wide <- dcast(x, compname ~ mrulename, value.var = 'interplr_mean')
knitr::kable(x.wide, digits = 3, caption="Mean Fuzzy Ratings for Select Soil Series")

# note: component name and series name have been converted to upper case
# sort rows of fuzzy ratings based on profiles from OSDs
new.order <- match(x.wide$compname, profile_id(s))
x.wide <- x.wide[new.order, ]

# copy ids to row.names so that they are preserved in distance matrix
row.names(x.wide) <- x.wide$compname

# create distance matrix
d <- daisy(x.wide[, -1])

# divisive hierarchical clustering
clust <- diana(d)

par(mar=c(2,0,2,0))
plotProfileDendrogram(s, clust, dend.y.scale = 1.5, scaling.factor = 0.004, y.offset = 0.1, width=0.15, cex.names=0.45)
title('Component Similarity via Select Fuzzy Ratings')
mtext('Profile Sketches are from OSDs', 1)

library(MASS)
library(vegan)
library(cluster)
library(RColorBrewer)

# get example data
# init a temp file
tf <- tempfile()

# download compressed CSV to temp file
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/clustering_and_ordination/MLRA-raster-data-example.csv.gz', destfile = tf, quiet = TRUE)
# read-in from compressed CSV to data.frame object
d.sub <- read.csv(gzfile(tf), stringsAsFactors = FALSE)

# check: note that the column names are quite strange
head(d.sub)

# set factor levels
mu.set <- c('15', '18', '22A', '22B')
d.sub$.id <- factor(d.sub$.id, levels = mu.set)

# define some nice colors
cols <- brewer.pal(9, 'Set1') 
# remove light colors
cols <- cols[c(1:5,7,9)]

# http://stackoverflow.com/questions/16225530/contours-of-percentiles-on-level-plot
kdeContours <- function(i, prob, cols, m, ...) {
  
  # need more than 2 rows of data
  if(nrow(i) < 2) {
    return(NULL)
  }
  
  # keep track of colors
  this.id <- unique(i$.id)
  this.col <- cols[match(this.id, m)]
  
  # estimate density
  dens <- kde2d(i$x, i$y, n=200)
  
  # differentiate to estimate quantiles
  dx <- diff(dens$x[1:2])
  dy <- diff(dens$y[1:2])
  sz <- sort(dens$z)
  c1 <- cumsum(sz) * dx * dy
  levels <- sapply(prob, function(x) {
    approx(c1, sz, xout = 1 - x)$y
  })
  
  # add contours if possible
  if(!is.na(levels))
    contour(dens, levels=levels, drawlabels=FALSE, add=TRUE, col=this.col, ...)
}

## NOTE: data with very low variability will cause warnings
# eval numerical distance, removing first 3 columns of IDs
d.dist <- daisy(d.sub[, -c(1:3)], stand=TRUE)

## map distance matrix to 2D space via principal coordinates
d.betadisper <- betadisper(d.dist, group=d.sub$.id, bias.adjust = TRUE, sqrt.dist = TRUE, type='median')
d.scores <- scores(d.betadisper)

# split data into a list by ID
s <- data.frame(x=d.scores$sites[, 1], y=d.scores$sites[, 2], .id=d.sub$.id)
s <- split(s, s$.id)

# plot
par(mar=c(1,1,3,1))
plot(d.scores$sites, type='n', axes=FALSE)
abline(h=0, v=0, lty=2, col='grey')

# add contours of prob density
# NOTE: lines are not added if data are too densely spaced for evaluation of requested prob. level 
res <- lapply(s, kdeContours, prob=c(0.75), cols=cols, m=levels(d.sub$.id), lwd=1, lty=3)
res <- lapply(s, kdeContours, prob=c(0.5), cols=cols, m=levels(d.sub$.id), lwd=1, lty=2)
res <- lapply(s, kdeContours, prob=c(0.25), cols=cols, m=levels(d.sub$.id), lwd=2, lty=1)

# add sample points
points(d.scores$sites, cex=0.45, col=cols[as.numeric(d.sub$.id)], pch=16)

# note special indexing for cases when low-var MU have been removed
ordilabel(d.betadisper, display='centroids', col=cols[match(mu.set, levels(d.sub$.id))])
title('Ordination of Raster Samples (cLHS Subset) with 25%, 50%, 75% Density Contours')
box()

library(plyr)
library(reshape2)
library(vegan)
library(cluster)

# init a temp file
tf <- tempfile()

# download compressed CSV to temp file
download.file('https://github.com/ncss-tech/stats_for_soil_survey/raw/master/data/clustering_and_ordination/seki-mu-gis-samples.csv.gz', destfile = tf, quiet = TRUE)
# read-in from compressed CSV to data.frame object
x <- read.csv(gzfile(tf), stringsAsFactors = FALSE)

# check
head(x)

# get terrain variable names
vars <- c('elev', 'solar', 'slope', 'tci', 'ppt', 'maat', 'tpi', 'tri', 'pi')

# covnvert wide to long format for summary by MY symbol
x.long <- melt(x, id.vars = 'MUSYM', measure.vars = vars)
x.summary <- ddply(x.long, c('MUSYM', 'variable'), .fun=plyr::summarize, m=median(value, na.rm = TRUE))
head(x.summary)

# retain median values, convert back to wide format
x.wide <- dcast(x.summary, MUSYM ~ variable, value.var = 'm')
row.names(x.wide) <- x.wide$MUSYM
head(x.wide, 2)

# generate lookup table of MUSYM - MU groups
lut <- unique(x[, c('MUSYM', 'gensym')])

# join data matrix with lut
x.wide <- join(x.wide, lut, by='MUSYM', type='left', match='first')
x.wide$gensym <- factor(x.wide$gensym, levels = c("riparian", "xx10", "xx20", "xx30", "xx40", "xx50"), labels = c("riparian", "basins and cirques", "glacial valleys", "bedrock controlled mountain slopes and valley walls", "non bedrock controlled slopes", "plateaus, till plains, outwash plains"))

# perform non-metric MDS
nmds <- metaMDS(x.wide[, vars], distance = 'gower', autotransform = FALSE, wascores=FALSE)

# setup colors
cols <- c('RoyalBlue', 'black', 'DarkGreen', 'orange3', 'red', 'brown')
site.colors <- cols[as.numeric(x.wide$gensym)]

# plot ordination
par(mar=c(1,1,1,1))
fig <- ordiplot(nmds, type='none', cex.axis=0.75, axes=FALSE)
abline(h=0, v=0, lty=2, col='grey')
text(fig, "sites", cex=0.65, font=2, col=site.colors)
legend('bottomleft', legend=levels(x.wide$gensym), col=cols, pch=15, pt.cex=2, bty='n', cex=1)
title('Map Unit Terrain Signatures')
