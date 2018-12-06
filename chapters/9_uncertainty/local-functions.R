
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
