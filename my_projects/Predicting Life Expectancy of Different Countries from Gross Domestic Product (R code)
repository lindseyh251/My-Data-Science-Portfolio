---
title: 'Predicting Life Expectancy of Different Countries from Gross Domestic Product'
author: "Lindsey Hornberger"
output:
word_document: default
html_document: default
---

```{r setup, include=FALSE}
# This chunk of code sets up the R session to perform the analysis
# Load packages, load data, load any other source scripts that may contain
# code or objects you will want to run to produce the report

# Load packages
library(tidyverse)
library(caret)
library(asbio)
library(olsrr)
library(xtable)
library(shiny)
library(knitr)
library(DT)
require(scatterplot3d)
require(Hmisc)
require(rgl)
require(faraway)
library(car)
data(chredlin)
attach(chredlin)


# declare global chunk options
knitr::opts_chunk$set(echo = FALSE) # Turn off print for all code chunks simultaneously

# determine output format dynamically
out_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")

# define custom function for data label outputs
# The DT::datatable function is great for producing tables for HTML docs
# Otherwise, use the knitr::kable function to produce tables
# You should use the R help to learn about these two functions as they
# will need to be used to produce visually appealing tables for your
# report

display_output <- function(dataset, out_type, filter_opt = 'none') {
  
  if (out_type == "html") {
    out_table <- DT::datatable(dataset, filter = filter_opt)
  } else {
    out_table <- knitr::kable(dataset)
  } 
  
  out_table
}

# Function to calculate predicted sum of squares (PRESS)
PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

data <- read_csv("Life-Expectancy-Data-Updated.csv", 
    col_types = cols(Infant_deaths = col_skip(), 
        Under_five_deaths = col_skip(), Adult_mortality = col_skip(), 
        Hepatitis_B = col_skip(), Measles = col_skip(), 
        Polio = col_skip(), Diphtheria = col_skip(), 
        Population_mln = col_skip(), Thinness_ten_nineteen_years = col_skip(), 
        Thinness_five_nine_years = col_skip(), 
        Schooling = col_double(), Economy_status_Developed = col_skip(), 
        Life_expectancy = col_double()))
data <- rename(data, GDP = GDP_per_capita, HIV = Incidents_HIV, economy_status = Economy_status_Developing)
```

### Abstract

To investigate the impact of outside factors on life expectancy in various countries, a dataset from is considered. The data within the dataset regarding population, GDP, and life expectancy was updated according to World Bank Data. Information about vaccinations for measles, Hepatitis B, polio, and diphtheria, alcohol consumption, BMI, HIV incidents, mortality rates, and thinness were collected from World Health Organization public datasets. Information about schooling was collected from 'Our World in Data' which is a project of the University of Oxford. The data was originally collected from research experiments implemented by each of the specific organizations and the data was complied into one dataset available on Kaggle. There are many different factors to consider when discussing life expectancy and a few of those variables are considered in this study.The variables are:  **GDP**, the gross domestic product (GDP) per capita in current USD; **schooling**, average years that people aged 25+ spent in formal education; **alcohol consumption**, alcohol consumption that is recorded in liters of pure alcohol per capita with 15+ years old; **measles**, % of coverage of Measles immunization among 1-year-old; **BMI**, average BMI measurement recorded; ; **HIV**, incidents of HIV per 1000 population and **infant deaths**, infant deaths per 1000 population. 

## I. Introduction
The worldwide average life expectancy is 72.27 years. However, when that is split between men and women, the average life expectancy for a man is 68.9 years and for a woman is 73.9 years. The average life expectancy is measured through surveying countries and government reporting. Life expectancy has increased dramatically since 1900 when the life expectancy for a person was 47 years. Life expectancy has been increasing steadily worldwide for some time and the driving force behind this is due to advances in technology and healthcare. In 1950, the life expectancy for men was 66.5 and 71.8 years for women. There are many different factors that can increase or decrease a person's life expectancy. A major factor in life expectancy is the country one lives in due to the access they will have to resources and healthcare. Life expectancy tends to increase when residents of a country have proper access to healthcare and nutrition. However, there are many other factors than can impact life expectancy. 


```{r describe}
# the display_output function was defined above, it's producing a table
# for each of the calls below
#display_output(data, out_type)
head(data)
```

### A. Aims. 
The purpose of the study is to **investigate the relationship** between GDP and life expectancy across difference countries between 2000 and 2015 while controlling for other potential sources of variation.

## II. Methods

### A. Preliminary Model.
A multiple linear regression model is considered. Let

  \(Y_i = \) the life expectancy for residents for the \(i^{th}\) country,

  \(X_{i1} =\) gross domestic product (GDP) per capita in current USD for the \(i^{th}\) country, 
  
  \(X_{i2} =\) average years that people aged 25+ spent in formal education for the \(i^{th}\) country, 

  \(X_{i3} = \) alcohol consumption that is recorded in liters of pure alcohol per capita with 15+ years old for the \(i^{th}\) country,  
  
  \(X_{i4} = \) average BMI measurement recorded for the \(i^{th}\) country,
  
  \(X_{i5} = \) region of the \(i^{th}\) country,
  
  \(X_{i6} =\) incidents of HIV per 1000 population aged 15-49 for the \(i^{th}\) country. 
  
  
Based on automatic variable selection methods in combination with criterion-based statistics, income was dropped from the model. Partial residual plots, residual-versus-fitted plots, and measures of influence were investigated and no issues with high influence points, linearity, constant variance, independence, or normality were identified. Details are included in the Appendix.

### B. Final Model
The **final model** is given by

\[Y_i = \beta_0 + \beta_1X_{i1} + \beta_2X_{i2} + \beta_3X_{i3} + \beta_4X_{i4} + \beta_5X_{i5} + \varepsilon_i\]

where \(\varepsilon_i \sim iidN(0,\sigma^2)\), \(i = 1, 2, . . . , 2864\), and \(\beta_0, \beta_1, . . . , \beta_,\) and \(\sigma^2\) are the unknown model parameters.

```{r}
# I didn't even get this entered until I did the Appendix steps!
# Fit the final model in order to describe it
m2 <- lm(Life_expectancy ~ log(GDP) + Schooling + Alcohol_consumption + BMI + log(HIV) + economy_status, data)
```

## III. Results.

*Description: Should follow the goals listed in Section I.  For each goal, write out the hypotheses being tested (if applicable) and the specific approach taken (e.g., \(F\) test or \(t\) test, 95% confidence interval, Bonferroni adjustment, value of \(\alpha\), etc.).*

**Statistical Analysis**
The data was available in .xlsx (excel) format. The data analysis is done using the statistical software R version 4.3.1 (2023-06-16). This project focuses mainly on multiple linear regression. All of the predictor variables are explored individually. In this dataset the sample size is 97 and there are no missing values. The smaller sample size could assume less predictability and a larger sampling variability. 

```{r}
summary(m2)
anova(m2)
confint(m2)
```

## IV. Discussion

*Description: Restate major findings and provide proper interpretation within the context of the study design. What are the implications of the findings?*

There appears to be a positive relationship between FAIR plan policies issued and percentage minority of the population in zip codes. The limitations of this analysis include that it is done at the zip code level rather than at the family or person level.  Notice that the data is at the zip code level--an analysis of this data is unable to directly investigate whether minorities are denied insurance. This type of **ecological analysis** requires we assume that the chances a minority homeowner obtains a FAIR plan after adjusting for the effect of the other covariates is constant across all zip codes.  This assumption is not verifiable and may be violated, resulting in incorrect conclusions (called an **ecological fallacy**).

## V. Appendix

*Description: Include details of the model building process including: transformations, outliers, variable selection, assumption checking.*

```{r}
# Fit the initial model in order to refine it
m1 <- lm(Life_expectancy ~ log(GDP) + Schooling + Alcohol_consumption + BMI + log(HIV) + economy_status, data)
```

### A. Diagnostics for Predictors.
The purpose of this section is to examine the distribution of predictors, identify any unusually large or small values, and examine bivariate associations to identify multicollinearity.  Unusual values should be flagged as they may **influence** the fit of the model.  Bivariate associations between predictors could cause issues if the purpose of the model is estimation.

A scatterplot matrix indicates positive linear associations between all variables. 
```{r}
pairs(Life_expectancy ~ log(GDP) + Schooling + Alcohol_consumption + BMI + log(HIV) + economy_status, data)
```

The Pearson correlation coefficients for all pairwise association are shown in Table 1.  Schooling appears to be highly associated with the dependent variable (life expectancy) with a correlation coefficient of 0.7324845.

```{r}
cor(data[,c(10,7,8,4,5,6, 9)])
```

Strip plots for all predictors and the dependent variable (jittered) are shown next to boxplots of the same data. First, it should be acknowledged that a log transformation of both **GDP** and **HIV** were taken.  GDP, as expected, is primarily positively skewed with the majority of observations clustered together and a few observations at much higher GDP levels.  Similarly, HIV was positively skewed with the majority of the observations clustered together around zero and fewer HIV values at might higher levels. Due the skewed nature of both of there variables, a natural-log transformation is appropriate for both.  

Other features of note:  there is a wide range of values for **BMI**. There is also skewness visible in the distributions of **alcohol consumption**, **BMI** and **log(HIV)**. For the variable **alcohol consumption** with observations clustered close to zero and a few data points with large values, we may need to apply transformations if the model diagnostics and assumption checks indicate it. Even after a log transformation, there is still some positively skewed values present in the distribution of **log(HIV)**. 

```{r}
boxplot(log(data$GDP), main = "log(GDP)")
stripchart(log(data$GDP), vertical = T, method = "jitter", main = "log(GDP)")

boxplot(data$Schooling, main = "Schooling")
stripchart(data$Schooling, vertical = T, method = "jitter", main = "Schooling")

boxplot(data$Alcohol_consumption, main = "Alcohol_consumption")
stripchart(data$Alcohol_consumption, vertical = T, method = "jitter", main = "Alcohol_consumption")

boxplot(data$BMI, main = "BMI")
stripchart(data$BMI, vertical = T, method = "jitter", main = "BMI")

boxplot(log(data$HIV), main = "HIV")
stripchart(log(data$HIV), vertical = T, method = "jitter", main = "log(HIV)")

boxplot(data$economy_status, main = "data$economy_status")
stripchart(data$economy_status, vertical = T, method = "jitter", main = "data$economy_status")
```

### C. Screening of Predictors

1. **Added variable plots** for each of the covariates are shown. Added variable plots (also known as partial residual plots or adjusted variable plots) provide evidence of the importance of a covariate given the other covariates already in the model. They also display the nature of the relationship between the covariate and the outcome (i.e., linear, curvilinear, transformation necessary, etc.) and any problematic data points with respect to the predictor. The plots all indicate no need for transformations because linear relationships are apparent.  They also indicate each variable provides some added value to a model that already includes all other covariates because the slopes of the linear relationships are all appear to be non-zero.  

```{r}
prplot(m1,1)
prplot(m1,2)
prplot(m1,3)
prplot(m1,4)
prplot(m1,5)
prplot(m1,6)
```

2. Since the purpose of the project is to examine the relationship between **GDP** and **Life_expectancy**, the goal is estimating \(\beta_1\). **Multicollinearity** can create instability in estimation and so it should be avoided. We have already seen that Schooling is highly associated with BMI (0.6354752) and a few other covariates. **Variance inflation factors (VIF)**  measure how much the variances of the estimated regression coefficients are inflated as compared to when the predictor variables are not linearly related.  A maximum VIF in excess of 10 is a good rule of thumb for multicollinearity problems.  Based on the maximum VIF, `r round(max(vif(m1)),2)`, there do not appear to be any issues that need remediation. However, \(VIF_2\) is much larger than the others, which indicates Schooling may be redundant.  

```{r}
vif(m1)
vif(m2)
```

3. **Automatic variable selection methods** can be a useful starting point in eliminating redundant variables. They should only be used as a guide to the screening and removal (or addition) of predictors. Here, **GDP** is forced to stay in the model and all other covariates are allowed to add or drop: 

```{r}
library(leaps)
ma <- regsubsets(Life_expectancy~log(GDP) + Schooling + Alcohol_consumption + BMI + log(HIV) + economy_status, data, force.in = 1, method = "seqrep")
sma <- summary(ma)
sma
```

The summary output includes a matrix indicating which predictors are included in each of the 4 candidate models.  In the first model (first row of the matrix, indicated by a '2' for the number of predictors) with two predictors, only log(GDP) and log(HIV) are included.  In the second model (row 2) with three (indicated by a '3') predictors, log(GDP), BMI, and log(HIV) are included. In the third model (row 3) with four (indicated by a '4') predictors, log(GDP), Schooling, Alcohol_consumption, and BMI are included. In the fourth model (row 4) with five (indicated by a '5') predictors,log(GDP), Schooling, Alcohol_consumption, BMI, and log(HIV) are included. 


Several criteria for selecting the best model are produced, including \(R^2_{adj}\) (large values are better), Bayes Information Criterion \(BIC\) (smaller values are better), and Mallow's \(C_p\) statistic (values of \(C_p\) close to \(p\) (number of beta coefficients).  Other criteria not produced by the `regsubsets` function are \(AIC\) and \(PRESS\).  We will calculate these statistics for the two potential final models based on the results of automatic variable selection.  Here, all statistics indicate that the best model is the full model: \(R^2_{adj} = \) `r round(sma$adj[4],3)`, \(BIC = \) `r round(sma$bic[4],3)`, \(C_p = \) `r round(sma$cp[4],3)`, \(AIC = \) `r round(extractAIC(m2)[3],3)`, and \(PRESS = \) `r round(PRESS(m2),3)`. The second best is a reduced model including only log(GDP), BMI, and log(HIV). 

```{r}
sma$adj # Adjusted R2 big
plot(3:7,sma$adj, xlab = "Number of Parameters", ylab = expression(R^2[adj]))
sma$bic # BIC small
plot(3:7, sma$bic, xlab = "Number of Parameters", ylab = expression(BIC))
sma$cp # Cp = p
plot(3:7, sma$cp, xlab = "Number of Parameters", ylab = expression(C[p]))



sma$bic

# Extract PRESS
PRESS(m2)

```



### C. Model Validation

Model validation can help us select the model that has the best predictive performance in a hold-out sample.  There are several approaches to model validation, two of which are **Leave-one-out cross validation** specifically for smaller datasets and **K-fold cross validation** which is for larger datasets. We will use the **K-fold cross validation** meant for larger datasets. 

**K-fold cross validation** is useful for larger datasets where training and testing data are available/feasible. This method involves:

1. Randomly split the data into \(k\) subsets. Reserve one of the subsets for testing.
2. Build (train) the model on the remaining \(k-1\) subsets.
3. Test the model on the reserved subset and record the mean squared prediction error.
4. Repeat the process, changing the testing subset each time, until all \(k\) subsets have served as the testing set.
5. Calculate the average of the \(k\) mean squared prediction errors.
6. If comparing models, the model with the lowest MSPE should be chosen.

```{r}
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
fullmodel <- train(Life_expectancy~log(GDP) + Schooling + Alcohol_consumption + BMI + log(HIV) + economy_status, data, method = "lm",
               trControl = train.control)

reducedmodel <- train(Life_expectancy~log(GDP) + Schooling + BMI + log(HIV) + economy_status, data, method = "lm",
               trControl = train.control)

data$interaction <- log(data$GDP)*data$BMI
interactionmodel <- train(Life_expectancy~log(GDP) + Schooling + Alcohol_consumption + BMI + log(HIV) + economy_status+ interaction, data, method = "lm",trControl = train.control)

# Summarize the results
print(fullmodel)
print(reducedmodel)
print(interactionmodel)
```
Using the **K-fold cross validation** method to check assumptions and outliers, we found there there were no apparent issues. K = 10 and the sample size for each fold was between 2577-2579. In this output, we are given three different values: RMSE, R-squared, and MAE. The model with the smallest RMSE and MAE and the larger R-squared is the best model. From this analysis, we can see that the full model is the final model.

### D. Residual Diagnostics

#### 1. **Model Completeness**

It's a good idea to also check for possible interactions (though we wouldn't hypothesize any for this analysis). The fitted-versus-residual plot looks like noise. This plot supports normality and constant variance of the residuals.  

```{r}
plot(residuals(m2)~fitted(m2)) # Model looks appropriate

gdp.i <- data$GDP > mean(data$GDP)
school.i <- data$Schooling > mean(data$Schooling)
alc.i <- data$Alcohol_consumption > mean(data$Alcohol_consumption)
bmi.i <- data$BMI > mean(data$BMI)
hiv.i <- data$HIV > mean(data$HIV) 
econ.i <- data$economy_status > mean(data$economy_status)
interaction.plot(gdp.i, school.i, data$Life_expectancy)
interaction.plot(gdp.i,alc.i,data$Life_expectancy)
interaction.plot(gdp.i,bmi.i,data$Life_expectancy)
interaction.plot(gdp.i,hiv.i,data$Life_expectancy)
interaction.plot(gdp.i,econ.i,data$Life_expectancy)

# Test for significant interaction using general linear f-test
#m3 <- lm(involact~race+fire+theft+age+race*theft)
#anova(m3) # Doesn't look like it is important but should probably consider for model validation, just in case

```

The interaction plots above test for significant interaction using general linear f-test. None of the interaction plots suggest the consideration of the addition of an interaction term in the model. 

#### 2. **Outliers**

Look for outliers in \(X\) and in \(Y\), and also investigate whether there are any influential points. 

To detect outliers in this study, the studentized residual plot is used. Based on this plot, 90% of the data points should be within the range of ±3 and this is found to be true. 

```{r}
plot(residuals(m2)~fitted(m2))
plot(rstudent(m2)~fitted(m2)) #Studentized residual
identify(rstudent(m2)~fitted(m2))
plot(rstandard(m2)~fitted(m2)) #Deleted studentized residual
```

These datapoints have an abs(rstandard(m2)) > 3: 
```{r}
which(abs(rstandard(m2)) > 3) 
```

These datapoints have hatvalues(m2)>2*p/n
```{r}
which(hatvalues(m2)>2*7/2864) # High leverage?
```



Difference in fits, difference in betas, and Cooks distance plots: 
```{r}
plot(dffits(m2)) # Compare to 2sqrt(p/n) (0.09) for large datasets and 1 for small
which(dffits(m2)>0.08)
which(dfbetas(m2)>0.04) # Compare to 2/sqrt(n) for large datasets and 1 for small
plot(cooks.distance(m2)) # Compare percentile F(p,n-p) to 10th or 20th
q <- pf(cooks.distance(m2),7,2864-7)
which(q>.1) 
which(q>.2)
```
The Cooks distance plot is shown above. None of the data points compare at the 10th or 20th percentile. Also, the diffits(m2) plot shows at least 90% of the data points within the range of 0.09 above and below 1. Due to this and the cooks.distance(m2) plot, we can conclude that there are no outliers that need to be excluded from the dataset. 

OLS Cook's D plot, dfbetas, and dffits plots are shown below. 
```{r}
ols_plot_cooksd_bar(m2) # One way to visualize Cook's distance
ols_plot_dfbetas(m2) # Visualize influence on estimation of betas
ols_plot_dffits(m2) # Visualize influence on estimation of Y

# Another approach to getting influence statistics
#m2i <- influence(m2) # Save influence stats
#halfnorm(cooks.distance(m2)) # Another approach to visualize Cook's distance
```

There do appear to be many outliers with values above the 0.001 threshold. However, all of these values are real and therefore will not be dropped from the dataset. 

#### 3. Constant Variance

There are no apparent issues with non-constant variance. The absolute value of the residuals appear to be normally distributed. 

```{r}
plot(abs(residuals(m2))~predict(m2), xlab = expression(hat(Y)), ylab = "Abs Residuals")
```

#### 4. Normality

A Q-Q plot supports approximate normality. 

```{r}
# with outliers
qqnorm(residuals(m2))
qqline(residuals(m2))
```

Both tails do appear to depart from the fitted line  slightly, but overall the distribution does appear to be normal and symmetric and shows a reasonably linear pattern. 
