## Functions used by chapters in Book / Part2
##
##





##### used in Numerical Taxonomy ###########

## TODO (DEB): consider placing this in sharpshootR

# from 2023 NCSS poster on NCSP
makeCP <- function(.dist, new.order = NULL, .cp = hcl.colors(n = 25, palette = 'Zissou 1'), mar = c(0.1, 0, 0.5, 0.8), order = 'original', ...) {
  
  # convert reduced distance matrix to matrix
  .m <- as.matrix(.dist)
  
  # optionally re-order
  if(!is.null(new.order)) {
    .m <- .m[new.order, new.order]
  }
  
  .res <- corrplot(
    .m, 
    col = .cp, 
    is.corr = FALSE, 
    diag = FALSE,
    col.lim = c(0, 1), 
    method = "color",
    type = "upper", 
    tl.pos = "td",
    tl.cex = 0.8,
    tl.col = 'black',
    # cl.pos = "t",
    order = order,
    ...
  ) 
  
  invisible(.res)
}


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






##### Uncertainty Appendix  #######

# simulate predicted class probabilities via draws from dirichlet 
# (n pixel * k classes)
# n: number of simulated "pixels" 
# alpha: dirichlet hyperparameter (class likelihood)
simulatePredictions <- function(n, alpha) {
  # number of classes
  k <- length(alpha)
  class.labels <- toupper(letters[1:k])
  
  # generate simulated probabilities
  # igraph function
  x <- sample_dirichlet(n, alpha = alpha)
  x <- t(x)
  
  # add class labels and id
  d <- as.data.frame(x)
  names(d) <- class.labels
  d$id <- rownames(d)
  
  # simulate actual classes using predicted probabilities
  d <- ddply(d, 'id', function(i) {
    i$actual <- sample(class.labels, size = 1, prob = i[, class.labels])
    return(i)
  })
  
  # reshape for plotting
  m <- melt(d, id.vars = 'id', measure.vars = class.labels)
  
  # compute H and CI for each simulated case
  H <- apply(x, 1, shannonEntropy, b=2)
  # H.norm <- apply(x, 1, shannonEntropy, b=k)
  CI <- apply(x, 1, confusionIndex)
  
  # reshape for plotting
  z <- data.frame(Shannon.H=H, CI=CI)
  z.long <- make.groups(Shannon.H=z$Shannon.H, CI=z$CI)
  
  return(list(predictions=d, predictions.long=m, stats=z, stats.long=z.long, classes=class.labels))
}

# get an example of predictions and associated uncertainty stats
extractExample <- function(x, n=1) {
  p <- x$predictions[1:n, ]
  p$id <- NULL
  stats <- x$stats[1:n, ]
  d <- data.frame(p, stats)
  return(d)
}


# generate some performance metrics
# x: results from simulatePredictions()
performance <- function(x, w=NULL) {
  # predictions
  p <- x$predictions
  
  # default weights matrix
  if(is.null(w)) {
    w <- outer(1:length(x$classes), 1:length(x$classes))
    w[] <- 0
    diag(w) <- 1
    dimnames(w) <- list(x$classes, x$classes)
  }
  
  
  # confusion matrix
  tab <- crossTabProbs(x)
  
  # simple brier score
  # x$classes is the unique set of class labels
  bs <- brierScore(p, x$classes)
  
  # un-weighted tau
  # equal priors, the default
  tau.equal.res <-tauW(tab)
  # priors from actual observations
  tau.actual.res <-tauW(tab, P=prop.table(table(p$actual)))
  
  
  res <- data.frame(brier.score=bs, tau.equal=tau.equal.res$tau, tau.actual=tau.actual.res$tau, PCC=tau.equal.res$overall.naive)
  return(res)
}

# generate confusion matrix from simulatePredictions()
# matrix is based on this most likely class vs. actual class
# x: output from simulatePredictions()
crossTabProbs <- function(x) {
  p <- x$predictions
  
  # most likely class lables
  max.pr.class <- x$classes[apply(p[, x$classes], 1, which.max)]
  
  # upgrade to factors for cross tabulation
  max.pr.class <- factor(max.pr.class, levels = x$classes)
  p$actual <- factor(p$actual, levels = x$classes)
  
  # confusion matrix, rows are predictions, columns are actual
  tab <- table(predictions=max.pr.class, actual=p$actual)
  
  return(tab)
}




