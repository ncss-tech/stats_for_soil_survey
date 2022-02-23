library(randomForest)
library(rpart)
library(rms)

rmse <- function(a, b) {
  sqrt(mean((a - b)^2))
}

set.seed(101001)
x <- 1:100
y <- x + rnorm(n = 50, mean = 15, sd = 15)
# y <- x * sin(x/10) + rnorm(n = 50, mean = 15, sd = 15)


# plot(y ~ x)

m <- lm(y ~ x)

m.rcs <- ols(y ~ rcs(x))

rf <- randomForest(y ~ x, nodesize = 5)
rp <- rpart(y ~ x)

d <- data.frame(
  x = -100:200
)

d$lm <- predict(m, newdata = d)
d$rf <- predict(rf, newdata = d)
d$rp <- predict(rp, newdata = d)
d$rcs <- predict(m.rcs, newdata = d)

rmse.lm <- round(rmse(y, predict(m)), 1)
rmse.rf <- round(rmse(y, predict(rf)), 1)
rmse.rp <- round(rmse(y, predict(rp)), 1)
rmse.rcs <- round(rmse(y, predict(m.rcs)), 1)

# leg.txt <- sprintf("%s (%s)", c('lm', 'randomForest', 'rpart', 'lm + RCS'), c(rmse.lm, rmse.rf, rmse.rp, rmse.rcs))
leg.txt <- sprintf("%s (%s)", c('lm', 'randomForest'), c(rmse.lm, rmse.rf))


par(mar = c(0, 0, 0, 0), bg = 'black', fg = 'white')
plot(y ~ x, xlim = c(-25, 125), ylim = c(-50, 150), type = 'n', axes = FALSE)
grid()
points(y ~ x, cex = 1, pch = 16, las = 1, col = 'white')
lines(lm ~ x, data = d, col = 2, lwd = 2)
lines(rf ~ x, data = d, col = 4, lwd = 2)
# lines(rp ~ x, data = d, col = 3, lwd = 2)
# lines(rcs ~ x, data = d, col = 6, lwd = 2)
legend('bottom', legend = leg.txt, lwd = 2, lty = 1, col = c(2, 4, 3), horiz = TRUE, title = 'RMSE')

