# Test file to make sure I could commit. 

## SW, SF and RJ are left-tailed test
# the critical values are the 100(α)th percentiles of the empirical distributions of these test statistics.

#The critical values for AD, KS, LL, CVM, DP and JB tests are the 100(1 − α)th percentiles of the
#empirical distribution of the respective test statistics. 

#The SK and KU are two-tailed tests so the critical values are the 100(α/2)th and 100(1 − α/2)th percentiles of the empirical
#distribution of the test statistics.

#The alternative distributions:

## 6 symmetric short-tailed distributions:
#1. (U(0,1), #2. GLD(0,1,0.25,0.25), #3. GLD(0,1,0.5,0.5), #4. GLD(0,1,0.75,0.75),
#5.GLD(0,1,1.25,1.25) ,#6.Trunc(−2,2).

## 8 symmetric long-tailed distributions:
#1.Laplace, #2.logistic, #3. GLD(0,1,−0.10,−0.10), #4.GLD(0,1,−0.15,−0.15)
#5.t(10), #6.t(15), #7.ScConN(0.2,9) #8.ScConN(0.05,9)

# 10 asymmetric distributions:
#1.gamma(4,5), #2.Beta(2,1), #3.Beta(3,2), #4.CSQ(4), #5.CSQ(10), 
#6.CSQ(20), #7.Weibull(3,1), #8.Lognormal, #9.LoConN(0.2,3), #10.LoConN(0.05,3).


set.seed(1234) # Seed to discuss the tests and distributions with. 
n <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000) # 15 Sample sizes used in paper. 
# Distributions

# Figure 1
  # GLD(0.0,1.0,0.75,0.75) at 5% significance level (skewness = 0, kurtosis = 1.89)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # GLD(0.0,1.0,0.5,0.5) at 5% significance level (skewness = 0, kurtosis = 2.08)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # GLD(0.0,1.0,0.25,0.25) at 5% significance level (skewness = 0, kurtosis = 2.54)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
    #gld package

u <- runif(n)
lam1 = 0 
lam2 = 1
lam3 = 0.75
lam4 = 0.75
GLDdist = lam1 + u^lam3-(1-u)^lam4/lam2

rGLD <- function(n, lam1, lam2, lam3, lam4){
  u <- runif(n)
  GLDdist <- lam1 + u^lam1 + u^lam3-(1-u)^lam4/lam2
}

x <- rGLD(30, 0, 1, 0.75,0.75)

# Table 2
  # U(0.1) at 5% significance level (skewness = 0, kurtosis = 1.8)
    # Can use runiform(n)
  # Tukey(0, 1, 1.25, 1.25) at 5% significance level (skewness=0, kurtosis=1.76)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # TRUNC(-2, 2) at 5% significance level (skewness=0, kurtosis=2.36)
    # Can use rtruncnorm(n, a, b, mean, sd) from https://cran.r-project.org/web/packages/truncnorm/truncnorm.pdf

# This is specifically looking at power comparisons of six short tailed tests.
# Tukey is used interchangeably with GLD in the text. 
# The truncated normal distribution is denoted as TruncN(a, b)

# Figure 2
  # GLD(0.0,1.0,−0.10,−0.10) at 5% significance level (skewness = 0,kurtosis = 6.78)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # ScConN(0.05,3) at 5% significance level (skewness = 0, kurtosis = 7.65)
    # Can use p*rnorm(n, 0, b)+(1-p)*rnorm(n, 0, 1)
  # GLD(0.0,1.0,−0.15,−0.15) at 5% significance level (skewness = 0, kurtosis = 10.36)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12

# Table 3
  # t(15) at 5% significance level (skewness = 0,kurtosis = 3.55)
    # Can use rt()
  # Logistic at 5% significance level (skewness = 0,kurtosis = 4.2)
    # Can use rlogis()
  # Laplace at 5% significance level (skewness = 0,kurtosis = 6.0)
    # Can use rlaplace() from https://search.r-project.org/CRAN/refmans/VGAM/html/laplaceUC.html

# These are 6 of the 8 long tailed tests in the paper. 

# ScConN stands for Scale Contaminated Normal Distribution. Need to find a reference for this. 
    # The scale-contaminated normal distribution, denoted by ScConN(p, b) is a mixture of two normal distribution 
    # with probability p from a normal distribution N(0, b^2) and probability 1 − p from N(0, 1).


# Figure 3
  # CHI(4df) at 5% significance level (skewness = 1.41, kurtosis = 6.00)
    # Can use rchisq()
  # BETA(2,1) at 5% significance level. (skewness = −0.57, kurtosis = 2.40).
    # Can use rbeta()

# Table 4
  # Weibull(3, 1) at 5% significance level. (skewness = 0.17, kurtosis = 2.73). 
    # Can use rweibull()
  # Lognormal at 5% significance level. (skewness = 1.07, kurtosis = 5.10).
    # Can use rlnorm()
  # LoConN(0.2,3) at 5% significance level. (skewness = 0.68, kurtosis = 3.09).
    # I believe we can use p*rnorm(n, a, 1)+(1-p)rnorm*(n, 0, 1)

# These are for 5 of the 10 asymmetric distributions discussed in the text. 

# LoConN is the location contaminated normal distribution.
  # LoConN(p, a) denotes the distribution of a random variable that is sampled with probability p from a normal   
  # distribution with mean a and variance 1 and with probability 1 − p from a standard normal distribution

rLoConN <- function(n,p,a){
  k <- runif(n)
  k <- ifelse(k <= p, 1, 0)
  LoConNdist <- k*rnorm(n, mean = a, sd=1) + (1-k)*rnorm(n)
  LoConNdist
}


# We need to determine the best way to generate random samples for GLD, ScConN, LoConN, Trunc, and Laplace distributions. 
# I believe the rest are in some r packages. We could look for other packages that can provide random samples of the above. 
# I'm not sure what to do with the skewness and kurtosis values, should we measure these for each dist. and show that they match?

# Normality Tests

data <- rnorm(1000, 0, 1) # Generated Normal data to apply the tests on.
    ## Why 1000 (shouldn't it be 10,000 (page 7)) and why standard normal

# Note About Theory:
# Check the theory on each of these to double check that they match. The documentation for 
# the Jarque Bera and D'Agostino Pearson test lack any theory, and there is no overlapping sources
# with the papers. 

############################################

## Test based on Regression and Correlation
# Shapiro Wilk Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html
# R documentation referencing the same source material as our journal. 
# Sample size for this test must be between 3 and 5000. 

SW <- shapiro.test(data) 

# SW$Statistic: value of Test
# SW$p.value: p value of test
# SW$method: character string "Shapiro-Wilk normality test"
# SW$data.name: character string giving the name(s) of the data.


############################################

## Empirical Distribution Test
# Kolmogorov Smirnov Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ks.test.html
# I cannot find whether they used the two-sided, right sided, or left sided version of the test. 
# Theory appears to be the same, someone can check and confirm if they want. They cite different resources. 

KS <- ks.test(data, pnorm(0,1), alternative="two.sided", exact=TRUE)

# KS$statistic: value of the test statistic
# KS$p.value: p-value of the test
# KS$alternative: character string describing the alternative hypothesis.
# KS$method: character string indicating what type of test was performed.
# KS$data.name: character string giving the name(s) of the data.

############################################

## Empirical Distribution Test
# Lilliefors Test - Library: nortest
# Link: https://www.rdocumentation.org/packages/nortest/versions/1.0-4/topics/lillie.test
# Theory appears to match. 

library(nortest)
LF <- lillie.test(data)

# LF$statistic: value of the test statistic
# LF$p.value: p-value of the test
# LF$method: character string indicating what type of test was performed.
# LF$data.name: character string giving the name(s) of the data.

############################################

## Empirical Distribution Test
# Cramer Von Mises Test - Library: nortest
# Link: https://www.rdocumentation.org/packages/nortest/versions/1.0-4/topics/cvm.test

CVM <- cvm.test(data)

# CVM$statistic: value of the test statistic
# CVM$p.value: p-value of the test
# CVM$method: character string indicating what type of test was performed.
# CVM$data.name: character string giving the name(s) of the data.

############################################

## Empirical Distribution Test
# Anderson Darling Test - Library: nortest
# Link: https://www.rdocumentation.org/packages/nortest/versions/1.0-4/topics/ad.test

AD <- ad.test(data)

# AD$statistic: value of the test statistic
# AD$p.value: p-value of the test
# AD$method: character string indicating what type of test was performed.
# AD$data.name: character string giving the name(s) of the data.

############################################

# Pearson's Chi-squared Test - Library: nortest
# Link to stats lib: https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test
# Link to nortest lib: https://www.rdocumentation.org/packages/nortest/versions/1.0-4/topics/pearson.test
# I chose the nortest test because while I think the documentation theory both matches our paper, 
# The test statistic described in the nortest document exactly matches the test statistic.
# Make your own judgements on which is better and let us know if you have a strong opinion or if
# I am completely wrong about my assertion (that they are equivalent tests) above. 

CSQ <- pearson.test(data)

# CSQ$statistic: value of the test statistic
# CSQ$p.value: p-value of the test
# CSQ$method: character string indicating what type of test was performed.
# CSQ$data.name: character string giving the name(s) of the data.
# CSQ$n.classes: number of classes use for the test
# CSQ$df degree of freedom to computer p-value

############################################

## Moments Test
# Jarque-Bera Test

# https://search.r-project.org/CRAN/refmans/DescTools/html/JarqueBeraTest.html
# https://rdrr.io/cran/lawstat/man/rjb.test.html

# We can use either one of them

library(DescTools) # it is a combination of tseries and lawtest
jbtest <- JarqueBeraTest(data)

#function (x, robust = TRUE, method = c("chisq", "mc"), N = 0, na.rm = FALSE) 
# method should be "chisq" and robust= TRUE

# jbtest$statistic: value of the test statistic
# jbtest$parameter: degree of freedom
# jbtest$p.value: p-value of the test
# jbtest$method: type of test was performed.
# jbtest$data.name: a character string giving the name of the data.
# jbtest$class: htest


library(lawstat)
jbtest <- rjb.test(data)
# Use RJB option. Bydefault option 
# function (x, option = c("RJB", "JB"), crit.values = c("chisq.approximation", "empirical"), N = 0) 

# jbtest$statistic: value of the test statistic
# jbtest$parameter: degree of freedom
# jbtest$p.value: p-value of the test
# jbtest$method: type of test was performed.
# jbtest$data.name: a character string giving the name of the data.
# jbtest$class: htest


############################################

## Moments Test
# D'Agostino Pearson Test

# https://www.rdocumentation.org/packages/fBasics/versions/3011.87/topics/NormalityTests
# https://rdrr.io/cran/fBasics/src/R/test-normalityTest.R

library(fBasics)
DPtest <- dagoTest(data)


# Internally it uses below functions: .omnibus.test,.skewness.test, .kurtosis.test
# Function of Our interest: 
# ITs statistic: (statistic = omni,method = "D'Agostino Omnibus Normality Test", p.value = pomni, data.name = DNAME)

# c(test$p.value, skew$p.value, kurt$p.value) 
# Consider 1st p-value as it is for DP Omnibus test
# attr(,"test")$p.value[1]

# c(test$statistic, skew$statistic, kurt$statistic) #receive o/p as a vector 
# Consider 1st statistic as it is for DP Omnibus test
# attr(DPtest,"test")$statistic[1]

