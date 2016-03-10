---
title: "Uncertainty_Validation"
author: "Skye Wills"
date: "December 22, 2015"
output:
  html_document:
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_depth: 3
    toc_float:
      collapsed: yes
      smooth_scroll: no
---

![Statistics for pedologists course banner image](figure/logo.jpg)

# Introduction
 
Validating and assessing the uncertainy of a model is just as, if not more important, than generating the model itself. Validation quantifies the model's ability to explain variance in the data while uncertainty quantifies the confidence of model prediction. Uncertainty and validation assessments enable the end user to better understand model error, the nature and distribution of the input data, and the overall accuracy and spatial applicability of the model. Within a soil model, there are several sources of error:  

- Measurement errors
- Interpretation errors
- Digitization errors
- Classification errors
- Generalization errors
- Interpolation errors

Errors are simply the difference between reality and our representation of reality. Assessing the data structure with simple statistical measures such as mean, median and mode can be useful for understanding the central tendency of the data, but more complicated calculations are needed to get at dispersion or the variation of a property within a population to further assess error and uncertainty (Zar, 1999).

**Measure of Dispersion**
  - Range: The difference between the highest and lowest values measured or observed. Not always reliable because it can include outliers, error, or misclassified data. 
  - Quantiles: These refer to 25% increments in the rank of observations. Typically, the 25th and 75th quantiles are used to represent the spread of the most typical values around the central tendency.

**Measure of Variation**
  * Variance: The deviation of from the mean is calculated as sum of squares (SS) to use absolute deviation (eliminate any distinction between negative and positive correlation). $variance (sample) = \frac{SS}{n-1}$
  
  * $SS = \sum{(X - x)^2}$
   
  * Standard deviation: Used to return variance to the original units $sd = \sqrt{\frac{SS}{n-1}}$
  
  * Coefficient of variation: Scale standard deviation with mean so that multiple properties can be compared $CV = \frac{SD}{x}$

Create and example dataset and evaluate dispersion.

```r
#set random seed so that we all get the same results
set.seed(3)
# we are creating a sample set with 10 values between 20 - 60
Depth <- sample(x = 20:60, size=10, replace = FALSE)

range(Depth)
```

```
## [1] 24 52
```

```r
quantile(Depth)
```

```
##    0%   25%   50%   75%  100% 
## 24.00 30.50 37.00 40.75 52.00
```

```r
#we are creating a sample set with values between 40 - 50
Depth2 <- sample(x=40:50, size=10, replace = FALSE)

range(Depth2)
```

```
## [1] 40 50
```

```r
quantile(Depth2)
```

```
##    0%   25%   50%   75%  100% 
## 40.00 43.25 45.50 47.75 50.00
```

```r
# compare with box plots
boxplot(list(example.1=Depth, example.2=Depth2), ylab='Depth (cm)', boxwex = 0.5)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)
    
## Exercise

1. Export depth as a csv file
2. Calculate Square Error for each row.
3. Summarize sum of Squares, Variance, and Standard Deviation
  
Dispersion or variance is a characteristic of the population being evaluated. While more or better sample collection might give you better precision of those estimates, we would not expect them to change the dispersion calculated. Conversely, measures of certainty of the central tendency (how sure are you of the typical value reported) depends both on the characteristic dispersion/variance and the number of samples collected.

**Measures of certainty**

- Standard Error: represents the variance of the mean that would be found with repeated sampling. Estimated by dividing standard deviation by the square root of n. The concept of standard error is important for hypothesis testing.
- Confidence interval: Interval in which you are confident that a given percentage (known as the confidence limit 95, 80, 75%) of the population lie. If a normal distribution is assumed, for a 95% confidence interval, it can be estimated that the value is 95% likely to fall between as SDx1.96 +/- mean.    
  
## Exercise 

4. Calculate Standard Error
5. Calculate Condfidence interval (95%)
6. Resample 10 times, each time calculate mean and variance.



```r
#set seed so that we all get the same results
set.seed(3)

#simulate a dataset with two soil names with depth values from 20 - 60 (some values occur more than once)
d <- data.frame(
  soil <- sample(c("A", "B"), size=100, replace = TRUE),
  depth <- sample(20:60, size=100, replace = TRUE)
)

range(d$depth)
```

```
## [1] 21 59
```

```r
quantile(d$depth)
```

```
##   0%  25%  50%  75% 100% 
##   21   30   39   49   59
```

```r
boxplot(depth ~ soil, data=d, horizontal = TRUE, las=1)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
#calculate the mean of depth
m <- mean(d$depth)
 
#subtract mean from each value and square
d$S <- (d$depth - m)^2
 
#calculate overall sum of squares
SS <- sum(d$S)
 
#calculate sample vaiance (lenth gives us the total number of sample/observations)
samp_var <- SS / length(d$depth) - 1
 
# Note the differences in range and variance calcualted for Depth in both examples (10 samples vs. 100)

quantile(Depth)
```

```
##    0%   25%   50%   75%  100% 
## 24.00 30.50 37.00 40.75 52.00
```

```r
quantile(d$depth)
```

```
##   0%  25%  50%  75% 100% 
##   21   30   39   49   59
```

```r
var(Depth)
```

```
## [1] 70.98889
```

```r
var(d$depth)
```

```
## [1] 119.5688
```

```r
# Now Compare Standard Error (standard deviation / n- 1)
sd(Depth) / sqrt(length(Depth))
```

```
## [1] 2.664374
```

```r
sd(d$depth) / sqrt(length(d$depth))
```

```
## [1] 1.093475
```

```r
# Why are standard errors different?
```


# Theory of Uncertainty
  
At it's most basic level, uncertainty is simply a lack of certainty. In soil survey, uncertainty encompasses both of these aspects: 1) you've gathered multiple observations and you need to describe them in relation to one another, and 2) you must predict a property or characteristic at unobserved locations. It is difficult to quantify the knowledge we have about data and information uncertainty. While we may have good data of the accuracy of our GPS, how likely are we to include that in our estimates of model error? How important is it? In other disciplines, they spend a lot of time quantifying and tracking measurement error. In soil science, we tend to treat measurement as having an exact known location and value. Given the unknowns in mapping and predicting soil properties, this is a reasonable treatment of relatively small levels of error.

When using secondary information as data (or data that is actually a prior prediction or result of a model, including soil components), considering incorporated error can be crucial. One way to deal with this is through resampling an alternate way is to through error propagation theory. The most common way to deal with this in soil survey and digital soil mapping is to assess error through model validation.
	
**Explanatory vs. Predictive Modelling**

While explanatory and predictive modeling can use the same types of models, data and even questions, the errors and uncertainty are important for different reasons

**Explanatory or Descriptive** - data are collected and analyzed in order to test causal hypothesis and observe correlations and relationships between data element. Often used at the beginning phases of soil-landscape exploration. How does the soil relate to each of the soil forming factors?

**Predictive** - applying a model or algorithm to data for the purpose of making a prediction (in new or unknown locations) (Shueli, 2010).
  
  
# Resampling to Estimate Uncertainty
  
When calculating many basic statistical parameters, the assumption is that the data is parametric. That implies that the data is continuous (ratio or interval) and normally distributed. This is rarely the case with soil data. Soil properties are often not normally distributed (you cannot have less that 0% organic matter, for instance) and often we are trying to predict soil taxa or other nominal classes. 

Resampling is a general term that defines any procedure to repeatedly draw samples form a given dataset. You are essentially pretending to collect a series of separate samples from your sample set then calculating a statistic on that sample. Resampling techniques can be used on known and unknown data distributions for uncertainty estimation and validation (Good, 2001).  


```r
#this bootstrap is estimating the uncertainty associated with the variance of d$depth

#abbreviate our data to simply the commands
d <- d$depth
n <- length(d)

#set number of iterations
N <- 50

#create a vector to store results
stat <- numeric(N)

for (i in 1:N){ # for each instance (i) in the set from 1 to N (50 in this case)
  dB <- sample(d, n, replace = T) #create a new variable dB from each bootstrap sample of d
  stat[i] <- var(dB)
}

quantile(stat)
```

```
##       0%      25%      50%      75%     100% 
## 102.2792 109.8320 118.6610 124.2920 136.6355
```

```r
stripchart(stat)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
boxplot(stat)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png)

```r
#an example of getting a confidence interval through bootstrapping (no assumption of a normal distribution)

N <- 100
boot.means <- numeric(N)

for (i in 1:N){
  boot.sample <- sample(d, 100, replace = T)
  boot.means[i] <- mean(boot.sample)
  }

quantile(boot.means, c(0.05, 0.95))
```

```
##      5%     95% 
## 37.9295 41.1310
```

```r
#traditional approach
#lower
mean(d) - 1.96 * sd(d) / sqrt(n)
```

```
## [1] 37.22679
```

```r
#upper
mean(d) + 1.96 * sd(d) / sqrt(n)
```

```
## [1] 41.51321
```

# Validation
  
Validation refers to the process and the result of a process where the validity of a model is tested. That is, how well does the model represent reality? There are varying degrees of formality and thoroughness that can be used in validation. While multiple stages of the modeling process can be validated, usually it's the output of the model that is investigated and reported. You can group initial validation into three broad groups: Expert evaluation, Theoretical Analysis and Prediction Accuracy.  

**Expert Evaluation:**  In this case, the model output is inspected by an expert user. The first evaluator will be you (the developer), but ideally an outside expert will be utilized. This is often a step in an iterative process. Evaluate the model output, does it make sense, do you see things that need to be improved? Then make changes to the model to improve the output.  

**Theoretical Analysis:** compare the results of the model to what is theoretically possible. In systems modeling, this might include diagnostics statistics including residual analysis, cross-correlation of variables and outputs, sensitivity analysis and model analysis such as Akaike Information Criterion (AIC). This can also include simple comparison of output to known possible values. This is especially important for linear regression where the slope of the model is assumed to be steady no matter the values of the dependent variables.  

**Prediction Accuracy:**  The correctness of the parameter being predicted by the model (soil taxa, property etc.).  Ideally this is done with an independent set of data. 

**Error Metrics**

- Numerical
    - Correlation (r2)
    - Scatter plots (visual inspection)
    - Mean Error (ME) = mean(predicted - actual)
    - Root Mean Square Error (RMSE) = sqrt((mean(predicted - actual)^2))
- Categorical
    - Confusion (error) matrix
    - Types of Accuracy
        - Overall Acuraccy - number of correct observations divided by total number of observations
        - User's Accuracy - probability that a class on the map actually represents that category on the ground
        - Producer's Accuracy - probability that a ground reference observation was assigned to the correct class in the map
  
In soil science, we typically use the term model validation to refer to a statistical analysis that assesses how well a model will predict at an unknown location. A complete model should have a formal statistical evaluation that can be reported and stored as model and output meta-data. That is the portion of validation we will focus. For this discussion, validation can be thought of as an assessment of prediction error and variance.

**Three types of validation used in the course**

- Apparent - Performance on sample used to develop model
- Internal - Performance on population underlying the sample
- External - Performance on related (similar/adjacent) but independent population


# Apparent Validation
  
In this exploratory and explanatory phase you are looking for relationships that can be used later for predictive purposes.

- Use Goodness of fit tests on all the data in your sample
    - Correlation (R2, rho, etc.)
    - P-values (test questions about individual or combinations of variables)
- Analyze Residuals (distribution of model errors) to diagnose modeling problems. One or more of these issues indicate that one or more important variables was omitted from the model or that an incorrect functional form was used (linear when the function should be non-linear)
    - Heteroscedasticity
    - Normality
    - Spatial distribution (autocorrelation)

# Inherent Validation 

## Split-sample - A single partition of the data into a learning and a calibration set.

- Achieve an independent validation by partitioning the samples into calibration or training and validation datasets (70% of the samples available are recommended for calibration)
    - Build model on calibration (training) dataset
    - Test model on validation (test) dataset
    - Report accuracy on the validation dataset
- This method is relatively simple (conceptually and computationally). Results depend on having an adequate sample size to both develop and test the model.

## Cross-validation - Alternate development and validation

**Leave-One-Out Cross-Validation (LOOCV)**

- One observation is used for testing and all others are used to develop model
- Repeat n (total number of observations) times
- Average error over n
- The mean of the accuracy is the expected accuracy of the model (this assumes that new data is from the same population) (Efron, 1983)

**k-fold Cross-Validation (k-fold) CV**

- Randomly divide observations into calibration and validation sets.
- Repeat k times, each time one k group is used for error estimates
- Average error of k
- Less computationally intensive than LOOCV, but it is more robust and can be done with smaller sample sizes than a simple split.
- Several R packages have tools to cross-validate predictions, including: `DAAG` and `boot` for `lm()` and `glm()` objects, `caret`, `rms`,


```r
library(caret)
library(randomForest)

### Linear model example

# Load and modify data
load(file = "C:/workspace/ch7_data.Rdata")
train <- data
train <- subset(train, frags > 0 & frags < 100)

# Custom logit and inverse logit function
logit <- function(x) log(x / (1 - x)) # logit transform
ilogit <- function(x) exp(x) / (1 + exp(x)) # inverse logit 

# Transform
train$fragst <- logit(train$frags / 100)

# Subset
train <- subset(train, select = c(pc_2, pc_1, temp, twi, precipsum, fragst))

# Create linear model
fragst_lm <- lm(fragst ~ pc_2 + pc_1 + temp + twi + precipsum, data = train)

# Create folds
folds <- createFolds(train$fragst, k = 10)

# Cross validate
cv_results <- lapply(folds, function(x) {
  train <- train[-x,]
  test <- train[x,]
  model <- lm(fragst ~ ., data = train)
  R2 <- summary(model)$r.squared
  adjR2 <- summary(model)$adj.r.squared
  return(c(R2 = R2, adjR2 = adjR2))
  }
  )

# Convert to a data.frame
cv_results <- do.call(rbind, cv_results)

# Summarize results
summary(cv_results)
```

```
##        R2             adjR2       
##  Min.   :0.3551   Min.   :0.3512  
##  1st Qu.:0.3634   1st Qu.:0.3596  
##  Median :0.3657   Median :0.3618  
##  Mean   :0.3679   Mean   :0.3640  
##  3rd Qu.:0.3738   3rd Qu.:0.3700  
##  Max.   :0.3817   Max.   :0.3779
```

```r
### Random forest example

# Modify data
train2 <- data
train2 <- subset(train2, frags > 0 & frags < 100)
train2 <- subset(train2, select = - c(pedon_id, taxonname, x_std, y_std, describer2, landform.string, argillic.horizon, landform, tax_subgroup, cluster2, cluster_mast, aspect, mast, gsi, ndvi, sw))

# Create folds
folds2 <- createFolds(train2$frags, k = 10)

# Cross validate
cv_results2 <- lapply(folds2, function(x) {
  train <- train2[-x,]
  test <- train2[x,]
  model <- randomForest(frags ~ ., data = train, na.action = na.exclude)
  R2 <- model$rsq[500]
  return(R2)
  }
  )

# Summarize results
summary(unlist(cv_results2))
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.5421  0.5529  0.5584  0.5609  0.5699  0.5805
```

  
## Subsample (Resampling or sample simulation)

In this method, the 'leave-out' method can be random (Bootstrap) or observation selection can use a more sophisticated method to select observations to represent the population including Monte Carlo (Molarino, 2005) and .632+bootstrap of Efron & Tibshirani (1997). The details of those aren't important, except to know that they can give you a better idea of the robustness of your model.

- As with resampling for uncertainty estimation, observations are repeatedly sampled
    - Select a number of samples (Randomly or from known distribution).
    - Develop the model
    - Estimate model accuracy on unselected samples
    - Repeat the process (with independent sample) a large number of times, 500 - 5,000. 
    - The expected model accuracy is then the mean of the estimates. 

	
**NOTE:** The BEST model should not be assumed to be the one that makes the 'truest' predictions. Beware of overfitting. When a model is overfit, it predicts due to very specific "quirks" in the calibration data set and not due to explanatory relationships that will apply to validation and independent datasets. One strategy to avoid this situation is to build models with as few variables as possible. Parsimonious models (those that use the least amount of information possible to obtain the same result or convey the same meaning) often have higher predicative validity. The use of metrics such as Akaike's Information Criterion (AIC) can be helpful for balancing error and parameter minimization.


## External Validation  

In this case, an independent dataset is used as the test case.  

- Independent observations predicted with model
- Errors (ME, RMSE) calculated on predicted vs. actual
- Some exploratory analysis can be helpful to diagnose and explain model performance.
  
The use of validation will be demonstrated as part of each modeling section. The size of the dataset used, understanding of the variables involved and the nature of the statistical models and algorithms used all influence which validation techniques are most convienent and appropriate.


# References

Efron, B., Tibshirani, R.J., 1993. An introduction to the bootstrap. Monographs on Statistics and Applied Probability, vol. 57. Chapman & Hall, London, UK.

Good, P.I., 2001. Resampling methods. Birkhäuser.

James, G., D. Witten, T. Hastie, and R. Tibshirani, 2014. An Introduction to Statistical Learning: with Applications in R. Springer, New York. [http://www-bcf.usc.edu/~gareth/ISL/](http://www-bcf.usc.edu/~gareth/ISL/)

Molinaro, A. M. (2005). Prediction error estimation: a comparison of resampling methods. Bioinformatics, 21(15), 3301-3307. doi:10.1093/bioinformatics/bti499

Shmueli, G.. (2010). To Explain or to Predict?. Statistical Science, 25(3), 289:310. Retrieved from http://www.jstor.org/stable/41058949

Zar, J.H., 1999. Biostatistical analysis. Pearson Education India.


# Additional reading

James, G., D. Witten, T. Hastie, and R. Tibshirani, 2014. An Introduction to Statistical Learning: with Applications in R. Springer, New York. [http://www-bcf.usc.edu/~gareth/ISL/](http://www-bcf.usc.edu/~gareth/ISL/)

Hastie, T., R. Tibshirani, and J. Friedman 2009. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. Springer, New York. [http://statweb.stanford.edu/~tibs/ElemStatLearn/](http://statweb.stanford.edu/~tibs/ElemStatLearn/)



  
