knitr::opts_chunk$set(message = FALSE, warning = FALSE, background='#F7F7F7', fig.align='center', fig.retina=2, dev='png', tidy=FALSE, verbose=FALSE, antialias='cleartype', cache=FALSE)

library(ggplot2)

# set random seed, so that we all get the same results
set.seed(123)

# create a sample dataset with 10 values between 20 - 60
d1 <- data.frame(
  soil = "alpha",
  depth = sample(20:60, size = 10, replace = TRUE)
  )

# create a sample dataset with 100 values between 20 - 60
d2 <- data.frame(
  soil = "beta",
  depth = sample(20:60, size = 100, replace = TRUE)
  )

# combine d1 and d2
d3 <- rbind(d1, d2)

aggregate(depth ~ soil, data = d3, quantile)

# examine box plots
ggplot(d3, aes(x = soil, y = depth)) + geom_boxplot()


# calculate the mean of depth
m <- mean(d2$depth)
 
# subtract mean from each value and square (i.e. residuals)
d2$S <- (d2$depth - m)^2
 
#calculate overall sum of squares
SS <- sum(d2$S)
 
#calculate sample variance (length gives us the total number of sample/observations)
SS / (length(d2$depth) - 1)

aggregate(depth ~ soil, data = d3, var)

SE <- function(x) sd(x) / sqrt(length(x))

aggregate(depth ~ soil, data = d3, SE)

# this bootstrap is estimating the uncertainty associated with the variance of d2$depth
# an example of getting a confidence interval through bootstrapping (no assumption of a normal distribution)

# abbreviate our data to simply the commands
d <- d2$depth
n <- length(d)

# set number of iterations
N <- 50

# create a data frame to store the results
boot_stats <- data.frame(
  vars = numeric(N),
  means = numeric(N)
  )

# for each instance (i) in the set from 1 to N (50 in this case)
for (i in 1:N) {
  # create a new variable dB from each bootstrap sample of d
  boot.sample = sample(d, n, replace = TRUE) 
  boot_stats$means[i] = mean(boot.sample)
  boot_stats$vars[i] = var(boot.sample)
  }

quantile(boot_stats$vars)

stripchart(boot_stats$vars)


# Traditional Approach
ci <- c(
  # lower 5th
  l = mean(d) - 1.96 * sd(d) / sqrt(n),
  # upper 95th
  u = mean(d) + 1.96 * sd(d) / sqrt(n)
  )

# Compare Bootstrap to Confidence Interval
quantile(boot_stats$means, c(0.05, 0.95))

ci

### Numeric error metrics

### Linear model example

# Create a Ficticous Data-set
d_num <- data.frame(
  depth = 21:60 + rnorm(40, mean = 0, sd = 10),
  slope = 60:21
  )

num_lm <- lm(depth ~ slope, data = d_num, y = TRUE)

# R2
summary(num_lm)$r.squared

# or

predicted <- num_lm$fitted.values
observed <- num_lm$y

cor(predicted, observed)^2

# ME
mean(predicted - observed)

# RMSE
sqrt(mean((predicted - observed)^2))


# Create a Ficticous Data-set
r <- runif(175)
idx <- sample(which(r > 0.5), 75)
r <- r[-idx]

d_cat = data.frame(
  predicted = r, 
  observed  = (r + rnorm(100, sd = 0.2)) > 0.5
  )

# Compute Confusion Matrix
cm <- table(predicted = d_cat$predicted > 0.5, observed = d_cat$observed)
print(cm)

# or

library(caret)
confusionMatrix(cm, positive = "TRUE")


# Examine threshoold
ggplot(d_cat, aes(x = predicted, color = observed)) +
  geom_density(lwd = 1.5)


# Trade Precision for Sensitivity by Varying the Threshold 
cm <- table(predicted = d_cat$predicted > 0.4, observed = d_cat$observed)
confusionMatrix(cm, positive = "TRUE")


### Linear model example
# Create folds
folds <- createFolds(d_num$depth, k = 10)

# Cross validate
lm_cv <- lapply(folds, function(x) {
  train   = d_num[-x,]
  test    = d_num[x,]
  model   = lm(depth ~ ., data = train, y = TRUE)
  actual  = test$depth
  predict = predict(model, test)
  RMSE    = sqrt(mean((actual - predict)^2, na.rm = TRUE))
  R2      = cor(actual, predict, use = "pairwise")^2
  return(c(RMSE = RMSE, R2 = R2))
  }
  )

# Convert to a data.frame
lm_cv <- do.call(rbind, lm_cv)

# Summarize results
summary(lm_cv)
