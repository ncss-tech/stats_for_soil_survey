library(aqp)
library(soilDB)

## soil colors -> computer colors, how?
# example data
x <- fetchOSD('musick', colorState = 'dry')
y <- fetchOSD('musick', colorState = 'moist')

# these are SoilProfileCollection objects
# e.g.
x$hue

h <- horizons(x)

# combine Munsell notation back into 
m1 <- sprintf("%s %s/%s", x$hue, x$value, x$chroma)
m2 <- sprintf("%s %s/%s", y$hue, y$value, y$chroma)

m1

# convert Munsell notation into hex-encoded sRGB (e.g. computer screen ready)
parseMunsell(m1)

# compute color contrast metrics
# 
cc <- colorContrast(m1, m2)
cc

# graphical explanation
colorContrastPlot(m1, m2, labels=c('Dry', 'Moist'), d.cex = 0.9)
