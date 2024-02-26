## explore the effect of the nodesize parameter to randomForest() and how it is related to the bias-variance tradeoff

library(av)
library(randomForest)
library(rpart)
library(rms)

# convenience function for root-mean-square-error 
rmse <- function(a, b) {
  sqrt(mean((a - b)^2))
}

# ensure the same results each time
set.seed(101001)

# fake data with a linear trend + noise
x <- 1:100
y <- x + rnorm(n = 50, mean = 15, sd = 15)

# check
plot(y ~ x)

# more complex trend with curvature, save for later
# y <- x * sin(x/10) + rnorm(n = 50, mean = 15, sd = 15)





# 
fitAndPlot <- function(node.size) {
  
  # linear model
  m <- lm(y ~ x)
  
  # random forest, variable node size (e.g. number of obs / terminal branch)
  rf <- randomForest(y ~ x, nodesize = node.size)
  
  # predictions will go here
  d <- data.frame(
    x = -100:200
  )
  
  # generate predictions
  d$lm <- predict(m, newdata = d)
  d$rf <- predict(rf, newdata = d)
  
  # RMSE
  rmse.lm <- round(rmse(y, predict(m)), 1)
  rmse.rf <- round(rmse(y, predict(rf)), 1)
  
  # legend text
  leg.txt <- sprintf("%s: %s", c('lm()', 'randomForest()'), c(rmse.lm, rmse.rf))
  
  # plot + annotate
  par(mar = c(0, 0, 2, 0), bg = 'black', fg = 'white')
  plot(y ~ x, xlim = c(-25, 125), ylim = c(-50, 150), type = 'n', axes = FALSE)
  title(sprintf('nodesize: %s', node.size), col.main = 'white')
  grid()
  points(y ~ x, cex = 1, pch = 16, las = 1, col = 'white')
  lines(lm ~ x, data = d, col = 2, lwd = 2)
  lines(rf ~ x, data = d, col = 4, lwd = 2)
  legend('topleft', legend = leg.txt, lwd = 2, lty = 1, col = c(2, 4, 3), title = 'RMSE')
  
}

# animate over nodesize argument
.animate <- function() {
  for(i in c(100:1)) {
    fitAndPlot(i)
  }
}


# check
fitAndPlot(1)
fitAndPlot(5)
fitAndPlot(25)
fitAndPlot(60)

# output file
f <- 'randomForest-nodesize-animation.mp4'

# animate and save as video (MP4)
av::av_capture_graphics(.animate(), output = f, width = 800, height = 650, framerate = 30, res = 90, vfilter = 'framerate=fps=30')

# open file when done
utils::browseURL(f)





