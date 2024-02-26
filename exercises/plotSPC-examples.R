library(aqp)
library(soilDB)

s <- c('leon', 'musick', 'clarksville', 'pardee', 'lucy', 'pierre', 'drummer', 'zook', 'san joaquin')

osds <- fetchOSD(s)

# encode horizon boundarydistinctness via vertical offset
osds$hd <- hzDistinctnessCodeToOffset(osds$distinctness)

# encode horizon boundary topography via vertical offset
osds$hzto <- hzTopographyCodeToOffset(osds$topography)

# also encode horizon boundary topography las line type
osds$hzto.lty <- hzTopographyCodeToLineType(osds$topography)



osds$bnd.code <- sprintf(
  "%s%s",
  substr(osds$distinctness, 1, 1),
  substr(osds$topography, 1, 1)
)

# remove missing (NA) labels
osds$bnd.code <- gsub('NANA', '', osds$bnd.code)

# ok
par(mar = c(0, 0, 0, 0))

plot(osds)

plotSPC(osds)

plotSPC(osds, id.style = 'side')

plotSPC(osds, id.style = 'top', width = 0.1)
plotSPC(osds, id.style = 'top', width = 0.5)

plotSPC(osds, id.style = 'top', width = 0.35)

plotSPC(osds, id.style = 'top', width = 0.35, name.style = 'center-center')

plotSPC(osds, id.style = 'top', width = 0.35, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE)

plotSPC(osds, id.style = 'top', width = 0.35, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.66, cex.id = 0.5)

plotSPC(osds, id.style = 'top', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.66, cex.id = 0.5, fixLabelCollisions = TRUE)

plotSPC(osds, id.style = 'top', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.66, cex.id = 0.5, fixLabelCollisions = TRUE, hz.depths.offset = 0.05)


# png(filename = 'figure.png', width = 1200, height = 900)

# pdf(file = 'poster.pdf', width = 96, height = 42)

plotSPC(osds, id.style = 'top', width = 0.3, name.style = 'center-center', plot.depth.axis = FALSE, hz.depths = TRUE, cex.names = 0.66, cex.id = 0.5, fixLabelCollisions = TRUE, hz.depths.offset = 0.05, shrink = TRUE)

# dev.off()



par(mar = c(0, 0, 3, 0))

plotSPC(osds, id.style = 'top', width = 0.35, name.style = 'center-center', color = 'value', col.label = 'Munsell Value (moist)', cex.names = 0.66, cex.id = 0.5, shrink = TRUE)

plotSPC(osds, id.style = 'top', width = 0.35, name.style = 'center-center', color = 'hue', col.label = 'Munsell Value (moist)', cex.names = 0.66, cex.id = 0.5, shrink = TRUE)


huePosition(returnHues = TRUE)
huePositionCircle()

osds$hue_ordered <- factor(osds$hue, levels = huePosition(returnHues = TRUE))

plotSPC(osds, id.style = 'top', width = 0.35, name.style = 'center-center', color = 'hue_ordered', col.label = 'Munsell Value (moist)', cex.names = 0.66, cex.id = 0.5, shrink = TRUE)





par(mar = c(0, 0, 0, 2))

plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', cex.id = 0.5, cex.names = 0.66, axis.line.offset = -1) 

plotSPC(osds, width = 0.3, hz.topography.offset = 'hzto', cex.id = 0.5, cex.names = 0.66, axis.line.offset = -1) 

plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.5, cex.names = 0.66, name = 'bnd.code', axis.line.offset = -1) 


plotSPC(osds, width = 0.3, hz.distinctness.offset = 'hd', hz.topography.offset = 'hzto', cex.id = 0.5, cex.names = 0.66, name = 'bnd.code', hz.boundary.lty = 'hzto.lty') 

legend('bottomleft', horiz = TRUE, legend = c('Smooth', 'Wavy', 'Irregular', 'Broken'), lty = 1:4, inset = 0.05, bty = 'n', cex = 0.85)


