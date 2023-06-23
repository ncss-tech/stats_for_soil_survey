# correlation from:
# 1. diagnostic features
# 2. site data
# 3. horizon data
# 4. horizon child tables (RMF, artifacts, etc.)


## steps
# setup selected set, excluding soils we don't care about
# decide on diagnostic/site/horizon characteristics that matter
# weed out soils missing those characteristics
# flag / code suites of characteristics that matter
# flag components of horizon designations that matter
# thickness of diagnostic / genetic hz
# depth to diagnostic / genetic hz


library(aqp)
library(soilDB)
library(sharpshootR)
library(cluster)


x <- fetchNASIS()
length(x)

x <- x[1:15, ]

table(x$taxonname)
table(x$taxclname)

# look at properties

par(mar = c(0, 0, 3, 1))
plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.8, width = 0.3)

# artifacts
plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.8, width = 0.3, color = 'total_art_pct')

# fragments
plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.8, width = 0.3, color = 'total_frags_pct')

plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.8, width = 0.3, color = 'clay')

plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.8, width = 0.3, color = 'sand')

# encode <2mm texture class as ordered factor
x$texture_class <- factor(x$texture_class, levels = SoilTextureLevels(), ordered = TRUE)

plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.8, width = 0.3, color = 'texture_class', offset = -0.5)


# add diagnostic brackets
par(mar = c(0, 0, 0, 1))
plotSPC(x, name.style = 'center-center', label = 'pedon_id', cex.names = 0.5, width = 0.3)


addDiagnosticBracket(x, kind = 'anthropic epipedon', offset = -0.55, col = 'orange', lwd = 2, tick.length = 0)

addDiagnosticBracket(x, kind = 'human-transported material', offset = -0.5, col = 'red', lwd = 2, tick.length = 0)

addDiagnosticBracket(x, kind = 'redox depletions with chroma 2 or less', offset = -0.45, col = 'royalblue', lwd = 2, tick.length = 0)

## make really big / detailed figures
# pdf(file = 'pedons.pdf', width = 20, height = 10)
# 
# dev.off()

# annotate at 50cm depth
abline(h = 50, lty = 3)

# extract diagnostic hz
d <- diagnostic_hz(x)
table(d$featkind)

# flag HTM
htm <- d[d$featkind == 'human-transported material', ]
site(x)$HTM <- FALSE

# find those pedon record IDs with HTM
idx <- which(site(x)$peiid %in% htm$peiid)

# set those to TRUE
x$HTM[idx] <- TRUE

# sort graphically
groupedProfilePlot(x, groups = 'HTM', name.style = 'center-center', label = 'pedon_id', cex.names = 0.5, width = 0.3)

## ideas here: 
# https://ncss-tech.github.io/AQP/soilDB/competing-series.html
# https://ncss-tech.github.io/AQP/aqp/genhz-distance-eval.html



# depth / thickness of
# ?depthOf
depthOf(x, pattern = 'g', top = TRUE)
minDepthOf(x, pattern = 'g', top = TRUE)

depthOf(x, pattern = '\\^', top = TRUE)
maxDepthOf(x, pattern = '\\^', top = FALSE)



## distance calculations
x.dist <- NCSP(x, vars = c('texture_class'))

# divisive hierarchical clustering
h <- as.hclust(diana(x.dist))

par(mar = c(1, 0, 3, 0))
plotProfileDendrogram(x, clust = h, scaling.factor = 1.5, y.offset = 3, color = 'texture_class', width = 0.3, name.style = 'center-center', col.label = 'Generalized Horizon Label', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.65, label = 'pedon_id')





# "extended" data from NASIS, usually child tables / summaries
e <- get_extended_data_from_NASIS_db()
str(e, 1)

e$art_summary
