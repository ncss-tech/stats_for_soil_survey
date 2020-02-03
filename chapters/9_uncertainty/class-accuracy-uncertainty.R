library(knitr, quietly=TRUE)
library(kableExtra, quietly=TRUE)
opts_chunk$set(message=FALSE, warning=FALSE, background='#F7F7F7', fig.retina=2, dev='png', tidy=FALSE, verbose=FALSE, fig.align='center', echo=FALSE)
options(width=100, stringsAsFactors=FALSE)

library(igraph)
library(latticeExtra)
library(plyr)
library(reshape2)
library(wesanderson)
library(grid)
library(aqp)

# functions specific to this document
source('local-functions.R')

s <- list()
n <- 1000

alpha.1 <- c(4,10,5,15,16)
s[['Case 1']] <- simulatePredictions(n=n, alpha=alpha.1)

alpha.2 <- c(3,4,5,6,12)
s[['Case 2']] <- simulatePredictions(n=n, alpha=alpha.2)

alpha.3 <- c(2,2,2,2,40)
s[['Case 3']] <- simulatePredictions(n=n, alpha=alpha.3)

## class probabilities
ss <- ldply(s, function(i) i$predictions.long)
names(ss)[1] <- 'example'
ss$example <- factor(ss$example)

cols <- brewer.pal(9, 'Set1')
tps <- list(superpose.line=list(col=cols, lwd=1, alpha=0.85))

p.1 <- densityplot( ~ value | example, groups=variable, data=ss, as.table=TRUE, layout=c(3,1),
pch=NA, xlim=c(-0.1, 1.1), scales=list(alternating=3, x=list(tick.number=5)), xlab='Class Probability',
strip=strip.custom(bg=grey(0.85)), auto.key=list(columns=5, lines=TRUE, points=FALSE),
par.settings=tps, panel=function(...) {
  gs <- seq(0,1, by=0.1)
  panel.abline(v=gs, lty=3, col='grey')
  panel.densityplot(...)
})


## distribution of stats
ss <- ldply(s, function(i) i$stats.long)
names(ss)[1] <- 'example'
ss$example <- factor(ss$example)


# uncertainty metrics
cols <- wes_palette("Zissou1")[c(1, 5)]
tps <- list(superpose.line=list(col=cols, lwd=2, alpha=0.85))

p.2 <- densityplot( ~ data | example, groups=which, data=ss, as.table=TRUE, layout=c(3,1), pch=NA, auto.key=list(columns=2, lines=TRUE, points=FALSE), strip=strip.custom(bg=grey(0.85)), scales=list(alternating=3, y=list(rot=0), x=list(tick.number=5)), xlab='', par.settings=tps, panel=function(...) {
  gs <- seq(0, 2.5, by=0.25)
  panel.abline(v=gs, lty=3, col='grey')
  panel.densityplot(...)
})


## scatter plot of H vs. CI
ss <- ldply(s, function(i) i$stats)
names(ss)[1] <- 'example'
ss$example <- factor(ss$example)

# xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),

# how do these two compare in general?
p.3 <- xyplot(Shannon.H ~ CI | example, data=ss, auto.key = list(columns=3, lines=FALSE, points=TRUE), as.table=TRUE, layout=c(3,1),
scales=list(alternating=1),  xlab='Confusion Index', ylab='Shannon H',
strip=strip.custom(bg=grey(0.85)), par.settings=list(plot.symbol=list(col='royalblue', pch=16, cex=0.85, alpha=0.25)), 
panel=function(x, y, subscripts=subscripts, ...) {
  panel.grid(-1, -1, lty=3, col='grey')
  l <- lm(y ~ x)
  med.H <- round(median(y), 2)
  med.CI <- round(median(x), 2)
  iqr.H <- round(IQR(y), 2)
  iqr.CI <- round(IQR(x), 2)
  cor.xy <- round(cor(x, y, method = 'spearman'), 2)
  ann <- paste0('H: ', med.H, ' (', iqr.H, ')\n', 
                'CI: ', med.CI, ' (', iqr.CI, ')\n',
                'cor: ', cor.xy)
  
  panel.points(ss$CI, ss$Shannon.H, col=grey(0.85), alpha=0.1)
  panel.xyplot(x, y, subscripts=subscripts, ...)
  panel.abline(l, col='black', lwd=1.5, ...)
  panel.abline(0, 1, lty=2)
  grid.text(ann, x = unit(0.05, 'npc'), unit(0.95, 'npc'), just = c('left', 'top'), gp = gpar(cex=0.75, font=2))
})


# examples of three cases
print(p.1)

# examples of three cases
print(p.1)
pp <- ldply(s, performance)
names(pp)[1] <- 'example'

kable_styling(kable(pp, row.names = FALSE, digits = 2, format='html'), full_width = FALSE)

ex <- ldply(s, extractExample, n=1)
names(ex)[1] <- 'example'
ex$CI <- NULL
ex$actual <- NULL

add_header_above(kable_styling(kable(ex, row.names = FALSE, digits = 2, format='html'), full_width = FALSE), header=c(" " = 1, "Class Probabilities" = 5, "Uncertainty" = 1))

library(aqp)

# example data
d <- structure(list(A = c(0.0897243494322252, 0.0537087411977284, 
0.0643087579284512, 0.0582791533521884, 0.0655491726966812, 0.0878056947034425, 
0.0550727743006022, 0.10724015754623, 0.0332599961787985, 0.0555131608754956
), B = c(0.191110141078936, 0.187244044389649, 0.119214057525671, 
0.198461646003737, 0.161851348940294, 0.172157251906694, 0.113611770097243, 
0.178697159594029, 0.194607795787689, 0.188977055949146), C = c(0.121941735763077, 
0.0770539012535731, 0.0977753159795662, 0.0774293724263895, 0.072198187957068, 
0.0366921003115242, 0.151033286139089, 0.0974443429098862, 0.124876574685048, 
0.0864142563046045), D = c(0.351108807309283, 0.322120077305279, 
0.440632731639948, 0.401063395801608, 0.312647702445919, 0.304193047630158, 
0.270239142407351, 0.258895264130713, 0.422747316475851, 0.252724366285052
), E = c(0.246114966416479, 0.359873235853771, 0.278069136926363, 
0.264766432416077, 0.387753587960038, 0.399151905448182, 0.410043027055715, 
0.357723075819142, 0.224508316872614, 0.416371160585702), id = c("1", 
"10", "100", "1000", "101", "102", "103", "104", "105", "106"
), actual = c("D", "B", "D", "E", "D", "D", "E", "E", "D", "E"
)), .Names = c("A", "B", "C", "D", "E", "id", "actual"), row.names = c(NA, 
10L), class = "data.frame")

# check it out
# predictions, and actual, observed class
head(d)

# compute Brier score from all predictions
brierScore(d, classLabels = c('A', 'B', 'C', 'D', 'E'), actual = 'actual')

# shannon entropy for first row, could be a single pixel or obs. point
shannonEntropy(d[1, c('A', 'B', 'C', 'D', 'E')])

# compute shannon entropy for all rows
apply(d[, c('A', 'B', 'C', 'D', 'E')], 1, shannonEntropy)
