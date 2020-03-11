library(randomForest)
library(rms)
library(visreg)
library(rpart)

# load cached data
load('cached-data.rda')

# down-grade to data.frame
m <- as.data.frame(s)

# add factor for O horizon presence
m$o.hz <- factor((m$o.hz.thick > 1))

# important !! reset row names
row.names(m) <- 1:nrow(m)

# fake data
# m$fake <- rnorm(n=nrow(m), mean = 0, sd = 2) + (1:nrow(m) / 5) + sin(m$elev)


# http://pbreheny.github.io/visreg/basic.html

l <- lm(MAST ~ elev + solar + tci + o.hz, data=m, weights = m$complete.yrs)

l.ols <- ols(MAST ~ rcs(elev, 3) + solar + tci + o.hz, data=m, weights = m$complete.yrs)

rf <- randomForest(MAST ~ elev + solar + tci + o.hz, data=m)

rp <- rpart(MAST ~ elev + solar + tci + o.hz, data=m, method='anova', weights = m$complete.yrs)


png(file='modeling-frameworks-visreg.png', width=900, height=800, res=96, antialias = 'cleartype')

par(mar=c(4.5, 4.5, 2, 2), mfrow=c(4, 4))

visreg(l, main='lm')

# trick for setting plot arguments
p <- visreg(l.ols, plot=FALSE)
plot(p, ylab='MAST', main='rms::ols')

p <- visreg(rp, plot=FALSE)
plot(p, ylab='MAST', main='rpart')

visreg(rf, main='randomForest')

dev.off()

# https://github.com/pbreheny/visreg/issues/83
