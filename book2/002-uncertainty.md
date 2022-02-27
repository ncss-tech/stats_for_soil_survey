---
output: html_document
editor_options: 
  chunk_output_type: console
---

# Uncertainty {#uncertainty}




![](static-figures/logo.jpg)

## Introduction
 
Validating and assessing the uncertainty of a model is just as, if not more important, than generating the model itself. Validation quantifies the model's ability to explain variance in the data while uncertainty quantifies the confidence of model prediction. Uncertainty and validation assessments enable the end user to better understand model error, the nature and distribution of the input data, and the overall accuracy and spatial applicability of the model. Within a soil model, there are several sources of error:  

- Measurement errors
- Interpretation errors
- Digitization errors
- Classification errors
- Generalization errors
- Interpolation errors
- Semantic errors

Errors are simply the difference between reality and our representation of reality. Assessing the data structure with simple statistical measures such as mean, median and mode can be useful for understanding the central tendency of the data, but more complicated calculations are needed to get at dispersion or the variation of a property within a population to further assess error and uncertainty (Zar, 1999).

**Measure of Dispersion**

- Range: The difference between the highest and lowest values measured or observed. Not always reliable because it can include outliers, error, or misclassified data. 
- Quantiles: These refer to 25% increments in the rank of observations. Typically, the 25th and 75th quantiles are used to represent the spread of the most typical values around the central tendency.

**Measure of Variation**
  * Variance: The deviation of from the mean is calculated as sum of squares (SS) to use absolute deviation (eliminate any distinction between negative and positive correlation). $variance (sample) = \frac{SS}{n-1}$
  
- $SS = \sum{(X - x)^2}$
- Standard deviation: Used to return variance to the original units $sd = \sqrt{\frac{SS}{n-1}}$
- Coefficient of variation: Scale standard deviation with mean so that multiple properties can be compared $CV = \frac{SD}{x}$

**Measures of Certainty**

- Standard Error: represents the variance of the mean that would be found with repeated sampling. Estimated by dividing standard deviation by the square root of n. The concept of standard error is important for hypothesis testing.
- Confidence interval: Interval in which you are confident that a given percentage (known as the confidence limit 95, 80, 75%) of the population lie. If a normal distribution is assumed, for a 95% confidence interval, it can be estimated that the value is 95% likely to fall between as SD * 1.96 +/- mean.   


### Examples - Dispersion


```r
# load the GSP Salt Affected Soil dataset
url <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/gsp_sas.csv"
sas <- read.csv(url)

n   <- nrow(sas)


# take a random sample of 100
set.seed(42)
idx <- sample(1:n, 100)
sas_n100 <- cbind(n = "100", sas[idx, ])


# take a random sample of 1000
set.seed(42)
idx <- sample(1:n, 1000)
sas_n1000 <- cbind(n = "1000", sas[idx, ])


# combine d1 and d2
sas_sub <- rbind(sas_n100, sas_n1000)

aggregate(pH_0.30_obs ~ n, data = sas_sub, quantile)
```

```
##      n pH_0.30_obs.0% pH_0.30_obs.25% pH_0.30_obs.50% pH_0.30_obs.75%
## 1  100       4.021620        5.214799        5.742846        6.500468
## 2 1000       2.370782        5.252397        5.957984        6.836619
##   pH_0.30_obs.100%
## 1         8.854437
## 2         9.865657
```

```r
# examine box plots
library(ggplot2)

ggplot(sas_sub, aes(x = pH_0.30_obs, y = n)) + geom_boxplot()
```

<img src="002-uncertainty_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Dispersion or variance is a characteristic of the population being evaluated. While more or better sample collection might give you better precision of those estimates, we would not expect them to change the dispersion calculated. Conversely, measures of certainty of the central tendency (how sure are you of the typical value reported) depends both on the characteristic dispersion/variance and the number of samples collected.

  
### Examples - Variation and Certainty

Create an example data-set and and evaluate variation and certainty.


```r
# calculate the mean
mu <- mean(sas$pH_0.30_obs, na.rm = TRUE)

# subtract mean from each value and square (i.e. residuals)
sas$S <- (sas$pH_0.30_obs - mu)^2

# calculate overall sum of squares
SS <- sum(sas$S, na.rm = TRUE)

# calculate sample variance (length gives us the total number of sample/observations)
SS / (sum(!is.na(sas$pH_0.30_obs)) - 1)
```

```
## [1] 1.143514
```

Note the differences in range and variance calculated for pH in both examples (100 samples vs. 1000)


```r
aggregate(pH_0.30_obs ~ n, data = sas_sub, var)
```

```
##      n pH_0.30_obs
## 1  100    1.047552
## 2 1000    1.138315
```

Now Compare Standard Error (standard deviation / square root of n)


```r
SE <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

aggregate(pH_0.30_obs ~ n, data = sas_sub, SE)
```

```
##      n pH_0.30_obs
## 1  100  0.11101414
## 2 1000  0.03827549
```

Why are the standard errors different?


## Theory of Uncertainty
  
At it's most basic level, uncertainty is simply a lack of certainty. In soil survey, uncertainty encompasses both of these aspects:

1. you've gathered multiple observations and you need to describe them in relation to one another, and 
2. you must predict a property or characteristic at unobserved locations. 

It is difficult to quantify the knowledge we have about data and information uncertainty. While we may have good data of the accuracy of our GPS, how likely are we to include that in our estimates of model error? How important is it? In other disciplines, they spend a lot of time quantifying and tracking measurement error. In soil science, we tend to treat measurement as having an exact known location and value. Given the unknowns in mapping and predicting soil properties, this is a reasonable treatment of relatively small levels of error.

When using secondary information as data (or data that is actually a prior prediction or result of a model, including soil components), considering incorporated error can be crucial. One way to deal with this is through re-sampling an alternate way is to through error propagation theory. The most common way to deal with this in soil survey and digital soil mapping is to assess error through model validation.
	
**Explanatory vs. Predictive Modelling**

While explanatory and predictive modeling can use the same types of models, data and even questions, the errors and uncertainty are important for different reasons

**Explanatory or Descriptive** - data are collected and analyzed in order to test causal hypothesis and observe correlations and relationships between data element. Often used at the beginning phases of soil-landscape exploration. How does the soil relate to each of the soil forming factors?

**Predictive** - applying a model or algorithm to data for the purpose of making a prediction (in new or unknown locations) (Shueli, 2010).
  
  
## Resampling to Estimate Uncertainty
  
When calculating many basic statistical parameters, the assumption is that the data is parametric. That implies that the data is continuous (ratio or interval) and normally distributed. This is rarely the case with soil data. Soil properties are often not normally distributed (you cannot have less that 0% organic matter, for instance) and often we are trying to predict soil taxa or other nominal classes. 

Re-sampling is a general term that defines any procedure to repeatedly draw samples form a given data-set. You are essentially pretending to collect a series of separate samples from your sample set then calculating a statistic on that sample. Re-sampling techniques can be used on known and unknown data distributions for uncertainty estimation and validation [@good2013].  



```r
# this bootstrap is estimating the uncertainty associated with the variance of sas$pH_0.30_obs
# an example of getting a confidence interval through bootstrapping (no assumption of a normal distribution)

# abbreviate our data to simply the commands
ph <- na.exclude(sas$pH_0.30_obs)
n <- 100

# set number of iterations
k <- 50

# create a data frame to store the results
boot_stats <- data.frame(
  vars = numeric(k),
  means = numeric(k)
  )

# for each instance (i) in the set from 1 to N (50 in this case)
for (i in 1:k) {
  # create a new variable dB from each bootstrap sample of d
  boot.sample = sample(ph, n, replace = TRUE) 
  boot_stats$means[i] = mean(boot.sample)
  boot_stats$vars[i]  = var(boot.sample)
  }

quantile(boot_stats$vars)
```

```
##        0%       25%       50%       75%      100% 
## 0.9002976 1.0326038 1.1305090 1.2524690 1.4563505
```

```r
stripchart(boot_stats$vars)
```

<img src="002-uncertainty_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
# Traditional Approach
ci <- c(
  # lower 5th
  l = mean(ph) - 1.96 * sd(ph) / sqrt(n),
  # upper 95th
  u = mean(ph) + 1.96 * sd(ph) / sqrt(n)
  )

# Compare Bootstrap to Confidence Interval
quantile(boot_stats$means, c(0.025, 0.975))
```

```
##     2.5%    97.5% 
## 5.888672 6.197441
```

```r
ci
```

```
##        l        u 
## 5.804082 6.223267
```


### Exercise 1

1. Calculate a bootstrapped median, 10th percentile, and 90th percentile for `EC_0.30_obs`.
2. Calculate a traditional confidence interval for `EC_0.30_obs`.
3. What is wrong with the traditional confidence interval?


## Performance Metrics

### Numerical

Accuracy:

- Coefficient of variation ($R^2$): % of variance explained (`caret::R2(formula = "traditional)`); beware alternative definitions refer to this as correlation coefficient squared (`caret::R2()`), however this version does not assess the accuracy; see [http://mng.bz/ndYf](http://mng.bz/ndYf) for when the two terms overlap
- Mean Error (ME): average error
- Mean Absolute Error (MAE): average absolute error
- Root Mean Square Error (RMSE): average residual

Precision:

- Standard error (SE)
- Prediction intervals (percentiles)
- Distance



```r
library(caret)

# Numeric error metrics----

# R2 ----
R2(pred = sas$pH_0.30_pred, obs = sas$pH_0.30_obs, formula = "traditional", na.rm = TRUE)
```

```
## [1] 0.8526782
```

```r
# RMSE ----
sqrt(mean((sas$pH_0.30_pred - sas$pH_0.30_obs)^2, na.rm = TRUE))
```

```
## [1] 0.4690274
```

```r
# or

RMSE(pred = sas$pH_0.30_pred, obs = sas$pH_0.30_obs, na.rm = TRUE)
```

```
## [1] 0.4690274
```

```r
# plot errors

idx <- sample(1:nrow(sas), 100)

ggplot(sas[idx, ], aes(x = pH_0.30_obs, y = pH_0.30_pred)) +
  geom_point() +
  # draw a 1 to 1 line
  geom_abline() +
  # draw a linear fit; method = "lm"
  geom_smooth(method = "lm")
```

<img src="002-uncertainty_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```r
ggplot(sas, aes(x = pH_0.30_obs, y = pH_0.30_pred)) +
  # use if too many points overlap
  geom_hex() +
  geom_abline() +
  geom_smooth()
```

<img src="002-uncertainty_files/figure-html/unnamed-chunk-7-2.png" width="672" />


### Exercise 2

1. Compare the traditional $R^2$ to the alternative $R^2$ for `EC_0.30_obs` vs `EC_0.30_pred`?
2. Calculate the `RMSE()` and `MAE()`for `EC_0.30_obs` vs `EC_0.30_pred`?
3. Plot a hex bin scatterplot of `EC_0.30_obs` vs `EC_0.30_pred` with a linear smoother.



### Categorical 

**Probability-based metrics** (threshold-independent)

Beware the $D^2$ and Tjur's D only apply to binary classes.

Accuracy:

- Brier score: equivalent to the mean square error (`aqp::brierScore()`)
- Deviance squared ($D^2$): % of devariance explained $R^2$ (`modEvA::Dsquared()`)
- Coefficient of discrimination (or Tjur's D)(`modEvA::RsqGLM()`)

Precision:

- Shannon entropy (`aqp::shannonEntropy()`)
- Confusion index (`aqp::confusionIndex()`)


**Class-based metrics** are derivatives of the confusion matrix [kuhn2013; @zumel2014; @congalton2019; @james2021](`caret::confusionMatrix()`)


Confusion Matrix   | Observed           |                     |Metric                     |
-------------------|--------------------|---------------------|--------------|
**Predicted**      | No                 | Yes                 |            UA|
No                 | True Negative (TN) | False Negative (FN) |            UA|
Yes                | False Positive (FP)| True Positive (TP)  |Precision / UA|
-------------------|--------------------|---------------------|--------------|
Metric             |Specificity / PA    |Sensitivity / PA     |Overall       |
 
                   

- Overall Accuracy: % of observations that were correctly classified, for all classes
- Specificity:
    - TN / (TN + FP)
    - errors of commission (Type I)
- Sensitivity (also known as Recall or True Positive Rate):
    - TP / (TP + FN)
    - errors of omission (Type II))
    - % of predictions that were correctly classified, for an individual class
- Precision:
    - TP / (TP + FP)
    - % of observations that were correctly classified, for an individual class
- User's Accuracy (UA):
    - diagonal values / predicted values
- Producer's Accuracy (PA):
    - diagonal values (TN & TP) / observed values
- Tau index (`aqp::tauW()`)



```r
url <- "https://raw.githubusercontent.com/ncss-tech/stats_for_soil_survey/master/data/gsp_bs.csv"
bs <- read.csv(url)
bs <- subset(bs, complete.cases(BS1_obs, BS2_obs))


# Probability metrics ----

## Brier score ----
vars <- c("BS2_pred", "BS2_obs")
aqp::brierScore(bs[vars], "BS2_pred", actual = "BS2_obs")
```

```
## [1] 0.04384271
```

```r
## D2 & Tjur D2----
modEvA::RsqGLM(obs = bs$BS2_obs, pred = bs$BS2_pred, plot = FALSE)
```

```
## $CoxSnell
## [1] 0.3957293
## 
## $Nagelkerke
## [1] 0.7857143
## 
## $McFadden
## [1] 0.7191203
## 
## $Tjur
## [1] 0.4823267
## 
## $sqPearson
## [1] 0.5992087
```

```r
# Shannon entropy ----
# fake example
test <- seq(0, 0.5, 0.1)
test <- data.frame(obs = test, pred = 1 - test)
cbind(
  test,
  entropy = apply(test, 1, aqp::shannonEntropy)
)
```

```
##   obs pred   entropy
## 1 0.0  1.0 0.0000000
## 2 0.1  0.9 0.4689956
## 3 0.2  0.8 0.7219281
## 4 0.3  0.7 0.8812909
## 5 0.4  0.6 0.9709506
## 6 0.5  0.5 1.0000000
```

```r
# bs example
summary(
  apply(data.frame(bs$BS2_pred, 1 - bs$BS2_pred), 1, aqp::shannonEntropy)
)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.2111  0.4327  0.4722  0.7418  1.0000
```

```r
# Class-based metrics -----

## Confusion matrix ----
# beware conf_mat prefers factors with equal numbers of levels
cm1 <- table(pred = as.factor(bs$BS2_pred > 0.5), obs = as.factor(bs$BS2_obs))
cm1
```

```
##        obs
## pred    FALSE  TRUE
##   FALSE 28788   979
##   TRUE    411  2696
```

```r
# or

cm2 <- confusionMatrix(cm1, positive = "TRUE")
cm2
```

```
## Confusion Matrix and Statistics
## 
##        obs
## pred    FALSE  TRUE
##   FALSE 28788   979
##   TRUE    411  2696
##                                           
##                Accuracy : 0.9577          
##                  95% CI : (0.9555, 0.9599)
##     No Information Rate : 0.8882          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.7717          
##                                           
##  Mcnemar's Test P-Value : < 2.2e-16       
##                                           
##             Sensitivity : 0.73361         
##             Specificity : 0.98592         
##          Pos Pred Value : 0.86772         
##          Neg Pred Value : 0.96711         
##              Prevalence : 0.11179         
##          Detection Rate : 0.08201         
##    Detection Prevalence : 0.09451         
##       Balanced Accuracy : 0.85976         
##                                           
##        'Positive' Class : TRUE            
## 
```

```r
## Examine thresholds ----
ggplot(bs, aes(x = BS2_pred, fill = BS2_obs)) +
  geom_density(alpha = 0.5) +
  xlab("BS2 Probability")
```

<img src="002-uncertainty_files/figure-html/unnamed-chunk-8-1.png" width="672" />

```r
## Trade Precision for Sensitivity by Varying the Threshold 
table(predicted = bs$BS2_pred > 0.5, observed = bs$BS2_obs)
```

```
##          observed
## predicted FALSE  TRUE
##     FALSE 28788   979
##     TRUE    411  2696
```


### Exercise 3

Using the examples discussed thus far as a guide, demonstrate your mastery of the material by performing the following tasks.

1. Calculate the Brier score, $D^2$ and Shannon Entropy for the `BS1` class from the `bs` dataset.
2. What probably threshold creates the best split for the `BS1` class.
3. Calculate a confusion matrix for `sas30_obs` vs `sas30_pred` from the `sas` dataset. Be sure to manually set the factor levels as shown below.


```r
lev <- unique(c(sas$sas030_obs, sas$sas030_pred))
lev <- lev[c(1, 4, 8, 9, 12, 5, 3, 6, 7, 11)]
sas$sas030_obs  <- factor(sas$sas030_obs,  levels = lev)
sas$sas030_pred <- factor(sas$sas030_pred, levels = lev)
```

4. Why can't you calculate a Brier score and Shannon entropy for the `SAS` classes from the `sas` dataset?


## Validation
  
Validation refers to the process and the result of a process where the validity of a model is tested. That is, how well does the model represent reality? There are varying degrees of formality and thoroughness that can be used in validation. While multiple stages of the modeling process can be validated, usually it's the output of the model that is investigated and reported. You can group initial validation into three broad groups: Expert evaluation, Theoretical Analysis and Prediction Accuracy.  

**Expert Evaluation:**  In this case, the model output is inspected by an expert user. The first evaluator will be you (the developer), but ideally an outside expert will be utilized. This is often a step in an iterative process. Evaluate the model output, does it make sense, do you see things that need to be improved? Then make changes to the model to improve the output.  

**Theoretical Analysis:** compare the results of the model to what is theoretically possible. In systems modeling, this might include diagnostics statistics including residual analysis, cross-correlation of variables and outputs, sensitivity analysis and model analysis such as Akaike Information Criterion (AIC). This can also include simple comparison of output to known possible values. This is especially important for linear regression where the slope of the model is assumed to be steady no matter the values of the dependent variables.  

**Prediction Accuracy**: The correctness of the parameter being predicted by the model (soil taxa, property etc.). Ideally this is done with an independent set of data.

In soil science, we typically use the term model validation to refer to a statistical analysis that assesses how well a model will predict at an unknown location. A complete model should have a formal statistical evaluation that can be reported and stored as model and output meta-data. That is the portion of validation we will focus. For this discussion, validation can be thought of as an assessment of prediction error and variance.

**Three types of validation used in the course**

- Internal - Performance on population underlying the sample
- External - Performance on related (similar/adjacent) but independent population


<!-- ## Apparent Validation -->

<!-- In this exploratory and explanatory phase you are looking for relationships that can be used later for predictive purposes. -->

<!-- - Use Goodness of fit tests on all the data in your sample -->
<!--     - Correlation (R2, rho, etc.) -->
<!--     - P-values (test questions about individual or combinations of variables) -->
<!-- - Analyze Residuals (distribution of model errors) to diagnose modeling problems. One or more of these issues indicate that one or more important variables was omitted from the model or that an incorrect functional form was used (linear when the function should be non-linear) -->
<!--     - Heteroscedasticity -->
<!--     - Normality -->
<!--     - Spatial distribution (auto-correlation) -->

### Internal Validation 

#### Split-sample - A single partition of the data into a learning and a calibration set.

- Achieve an independent validation by partitioning the samples into calibration or training and validation data-sets (70% of the samples available are recommended for calibration)
    - Build model on calibration (training) data-set
    - Test model on validation (test) data-set
    - Report accuracy on the validation data-set
- This method is relatively simple (conceptually and computationally). Results depend on having an adequate sample size to both develop and test the model.

#### Cross-validation - Alternate development and validation

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
### Linear model example
# Create folds
folds <- createFolds(1:nrow(sas), k = 10)

# Cross validate
lm_cv <- lapply(folds, function(x) {
  train = sas[-x,]
  test  = sas[x,]
  obs   = test$pH_0.30_obs
  # predict = predict(model, test)
  pred  = test$pH_0.30_pred
  RMSE  = RMSE(pred, obs, na.rm = TRUE)
  R2    = R2(pred, obs, formula = "traditional", na.rm = TRUE)
  return(c(RMSE = RMSE, R2 = R2))
})

# Convert to a data.frame
lm_cv <- do.call(rbind, lm_cv)

# Summarize results
summary(lm_cv)
```

```
##       RMSE              R2        
##  Min.   :0.4508   Min.   :0.8394  
##  1st Qu.:0.4613   1st Qu.:0.8482  
##  Median :0.4690   Median :0.8542  
##  Mean   :0.4689   Mean   :0.8526  
##  3rd Qu.:0.4740   3rd Qu.:0.8567  
##  Max.   :0.4934   Max.   :0.8651
```

  
#### Subsample (Resampling or sample simulation)

In this method, the 'leave-out' method can be random (Bootstrap) or observation selection can use a more sophisticated method to select observations to represent the population including Monte Carlo (Molarino, 2005) and .632+bootstrap of Efron & Tibshirani (1997). The details of those aren't important, except to know that they can give you a better idea of the robustness of your model.

- As with re-sampling for uncertainty estimation, observations are repeatedly sampled
    - Select a number of samples (Randomly or from known distribution).
    - Develop the model
    - Estimate model accuracy on unselected samples
    - Repeat the process (with independent sample) a large number of times, 500 - 5,000. 
    - The expected model accuracy is then the mean of the estimates. 

	
**NOTE:** The BEST model should not be assumed to be the one that makes the 'truest' predictions. Beware of over-fitting. When a model is over-fit, it predicts due to very specific "quirks" in the calibration data set and not due to explanatory relationships that will apply to validation and independent data-sets. One strategy to avoid this situation is to build models with as few variables as possible. Parsimonious models (those that use the least amount of information possible to obtain the same result or convey the same meaning) often have higher predicative validity. The use of metrics such as Akaike's Information Criterion (AIC) can be helpful for balancing error and parameter minimization.


### External Validation  

In this case, an independent data-set is used as the test case.  

- Independent observations predicted with model
- Errors (ME, RMSE) calculated on predicted vs. actual
- Some exploratory analysis can be helpful to diagnose and explain model performance.
  
The use of validation will be demonstrated as part of each modeling section. The size of the data-set used, understanding of the variables involved and the nature of the statistical models and algorithms used all influence which validation techniques are most convenient and appropriate.


<!-- ## Additional reading -->

<!-- Efron, B., Tibshirani, R.J., 1993. An introduction to the bootstrap. Monographs on Statistics and Applied Probability, vol. 57. Chapman & Hall, London, UK. -->

<!-- Hastie, T., R. Tibshirani, and J. Friedman 2009. The Elements of Statistical Learning: Data Mining, Inference, and Prediction. Springer, New York. [http://statweb.stanford.edu/~tibs/ElemStatLearn/](http://statweb.stanford.edu/~tibs/ElemStatLearn/) -->

<!-- Molinaro, A. M. (2005). Prediction error estimation: a comparison of resampling methods. Bioinformatics, 21(15), 3301-3307. doi:10.1093/bioinformatics/bti499 -->

<!-- Shmueli, G.. (2010). To Explain or to Predict?. Statistical Science, 25(3), 289:310. Retrieved from http://www.jstor.org/stable/41058949 -->

<!-- Zar, J.H., 1999. Biostatistical analysis. Pearson Education India. -->

<!-- references automatically added here  -->




  
