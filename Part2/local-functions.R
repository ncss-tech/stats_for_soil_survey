

## TODO (DEB): consider placing this in sharpshootR

# compare pair-wise distances between 3 individuals
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
