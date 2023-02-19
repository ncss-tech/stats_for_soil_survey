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

rp <- rpart(MAST ~ elev + solar + tci + o.hz, data=m, method='anova', weights = m$complete.yrs)

rf <- randomForest(MAST ~ elev + solar + tci + o.hz, data=m, mtry=3, trees=1000)

png(file='modeling-frameworks-visreg.png', width=1000, height=900, res=120)

par(mar=c(4.5, 4.5, 2, 2), mfrow=c(4, 4))

visreg(l, main='lm', ylim=c(0, 20))

# trick for setting plot arguments
p <- visreg(l.ols, plot=FALSE)
plot(p, ylab='MAST', main='rms::ols', ylim=c(0, 20))

p <- visreg(rp, plot=FALSE)
plot(p, ylab='MAST', main='rpart', ylim=c(0, 20))

visreg(rf, main='randomForest', ylim=c(0, 20))

dev.off()

# https://github.com/pbreheny/visreg/issues/83
