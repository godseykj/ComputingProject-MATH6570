# This file is a compilation of libraries, notes from the paper, and code to use when writing other files.
  # Written by Jeremiah with help from Kara & Mansi


# The SW, SF and RJ are left-tailed test so the critical values are the 
# 100(α)th percentiles of the empirical distributions of these test statistics.

# The critical values for AD, KS, LL, CVM, DP and JB tests are the 100(1 − α)th percentiles of the
# empirical distribution of the respective test statistics. 

# The SK and KU are two-tailed tests so the critical values are the 100(α/2)th and 100(1 − α/2)th percentiles of the empirical
# distribution of the test statistics.

# The distributions:

## 6 symmetric short-tailed distributions:
# 1. (U(0,1)            2. GLD(0,1,0.25,0.25) 3. GLD(0,1,0.5,0.5), 
# 4. GLD(0,1,0.75,0.75) 5.GLD(0,1,1.25,1.25)  6.Trunc(−2,2).

## 8 symmetric long-tailed distributions:
# 1.Laplace 2.logistic  3. GLD(0,1,−0.10,−0.10) 4.GLD(0,1,−0.15,−0.15)
# 5.t(10),  6.t(15)     7.ScConN(0.2,9)         8.ScConN(0.05,9)

## 10 asymmetric distributions:
# 1.Gamma(4,5)  2.Beta(2,1)     3.Beta(3,2) 4.CSQ(4)          5.CSQ(10), 
# 6.CSQ(20)     7.Weibull(3,1)  8.Lognormal 9.LoConN(0.2,3)   10.LoConN(0.05,3).


set.seed(1234) # Seed to discuss the tests and distributions with. 
n <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000) # 15 Sample sizes used in paper. 

# Distributions

# These are the 6 short tailed distributions in the paper. 

# Figure 1
  # GLD(0.0,1.0,0.75,0.75) at 5% significance level (skewness = 0, kurtosis = 1.89)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # GLD(0.0,1.0,0.5,0.5) at 5% significance level (skewness = 0, kurtosis = 2.08)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # GLD(0.0,1.0,0.25,0.25) at 5% significance level (skewness = 0, kurtosis = 2.54)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12


  # Kara found GLD package to run these distributions, using gld.moments() and rgl() functions. 
    # From https://cran.r-project.org/web/packages/gld/gld.pdf


# Table 2
  # U(0,1) at 5% significance level (skewness = 0, kurtosis = 1.8)
    # Can use runiform(n)
  # Tukey(0, 1, 1.25, 1.25) at 5% significance level (skewness=0, kurtosis=1.76)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # TRUNC(-2, 2) at 5% significance level (skewness=0, kurtosis=2.36)
    # Can use rtruncnorm(n, a, b, mean, sd)


  # Tukey is used interchangeably with GLD in the text. 
  # Jeremiah found truncnorm package to generate random variates.
    # From https://cran.r-project.org/web/packages/truncnorm/truncnorm.pdf


# These are 6 of the 8 long tailed tests in the paper. Other 2 not reported. 

# Figure 2
  # GLD(0.0,1.0,−0.10,−0.10) at 5% significance level (skewness = 0,kurtosis = 6.78)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12
  # ScConN(0.05,3) at 5% significance level (skewness = 0, kurtosis = 7.65)
    # Can use p*rnorm(n, 0, b)+(1-p)*rnorm(n, 0, 1)
  # GLD(0.0,1.0,−0.15,−0.15) at 5% significance level (skewness = 0, kurtosis = 10.36)
    # Generate runiform(n), set each lambda, generate x from equation on top of page 12


  # Mian derived mean/variance of scale contaminated normal. 
  # Kara wrote the function to generate random variates
    # rScConN <- function(n,p,b){
      # k <- runif(n)
      # k <- ifelse(k <= p, 1, 0)
      # ScConNdist <- k*rnorm(n, mean = 0, sd=b) + (1-k)*rnorm(n)
      # ScConNdist
    # }


# Table 3
  # t(15) at 5% significance level (skewness = 0,kurtosis = 3.55)
    # Can use rt()
  # Logistic at 5% significance level (skewness = 0,kurtosis = 4.2)
    # Can use rlogis()
  # Laplace at 5% significance level (skewness = 0,kurtosis = 6.0)
    # Can use rlaplace() 


  # Jeremiah found package to generate Laplace random variates. 
    # From https://search.r-project.org/CRAN/refmans/VGAM/html/laplaceUC.html


# These are for 5 of the 10 asymmetric distributions discussed in the text. Other 5 not reported. 


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


    # We tried to use an rpackage for lognormal, but got bad results. Generated random normal variates and 
    # found e^X for those random variates to generate lognormal, and got better results
    # Kara wrote function for location contaminated normal
      # rLoConN <- function(n,p,a){
        # k <- runif(n)
        # k <- ifelse(k <= p, 1, 0)
        # LoConNdist <- k*rnorm(n, mean = a, sd=1) + (1-k)*rnorm(n)
        # LoConNdist
      # }

# Normality Tests

data <- rnorm(1000, 0, 1) # Generated Normal data to apply the tests on.

# Note for all tests: The documentation references and theory were matched to see if it
# is the same as the paper, for some of the tests the references matched but for all of
# the tests the theory in the paper matched the documentation. 

############################################

## Test based on Regression and Correlation
# Shapiro Wilk Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html

SW <- shapiro.test(data) 

# SW$Statistic: value of Test
# SW$p.value: p value of test
# SW$method: character string "Shapiro-Wilk normality test"
# SW$data.name: character string giving the name(s) of the data.


############################################

## Empirical Distribution Test
# Kolmogorov Smirnov Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ks.test.html

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

## Empirical Distribution Test
# Pearson's Chi-squared Test - Library: nortest
# Link to nortest lib: https://www.rdocumentation.org/packages/nortest/versions/1.0-4/topics/pearson.test

CSQ <- pearson.test(data)

# CSQ$statistic: value of the test statistic
# CSQ$p.value: p-value of the test
# CSQ$method: character string indicating what type of test was performed.
# CSQ$data.name: character string giving the name(s) of the data.
# CSQ$n.classes: number of classes use for the test
# CSQ$df degree of freedom to computer p-value

############################################

## Moments Test
# Jarque-Bera Test - Library: lawstat
# Link: https://rdrr.io/cran/lawstat/man/rjb.test.html

library(lawstat)
jbtest <- rjb.test(data, option="JB")

# jbtest$statistic: value of the test statistic
# jbtest$parameter: degree of freedom
# jbtest$p.value: p-value of the test
# jbtest$method: type of test was performed.
# jbtest$data.name: a character string giving the name of the data.
# jbtest$class: htest

# Note: Experimentally, the test they used in the paper was the jarque bera test, 
# not the robust jarque bera test. 

############################################

## Moments Test
# D'Agostino Pearson Test - Library: fBasics
# https://rdrr.io/cran/fBasics/src/R/test-normalityTest.R

omnibus.test <- function(x)
  {
    # Internal Function for D'Agostino Normality Test:
    
    # FUNCTION:

    if (exists("complete.cases")) {
      test = complete.cases(x)
    } else {
      test = !is.na(x)
    }
    x = x[test]
    n = length(x)
    meanX = mean(x)
    s =  sqrt(mean((x-meanX)**2))
    a3 = mean((x-meanX)**3)/s**3
    a4 = mean((x-meanX)**4)/s**4
    SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
    SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
    U3 = a3/SD3
    U4 = (a4-3+6/(n+1))/SD4
    b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
    W2 = sqrt(2*(b-1))-1
    delta = 1/sqrt(log(sqrt(W2)))
    a = sqrt(2/(W2-1))
    Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
    B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
    A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
    jm = sqrt(2/(9*A))
    pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
    Z4 = (1-2/(9*A)-pos)/jm
    omni = Z3**2+Z4**2
    
    # Result:
    omni
}
omnibus.test(data)

# This code was taken from the dagoTest(x) function. This calculated 4-5 other values
# that we did not care for, and so we grabbed what we needed to properly apply the test. 
