# Test file to make sure I could commit. 

set.seed(1234) # Seed to discuss the tests and distributions with. 

# Distributions

# Figure 1
  # GLD(0.0,1.0,0.75,0.75) at 5% significance level (skewness = 0, kurtosis = 1.89)
  # GLD(0.0,1.0,0.5,0.5) at 5% significance level (skewness = 0, kurtosis = 2.08)
  # GLD(0.0,1.0,0.25,0.25) at 5% significance level (skewness = 0, kurtosis = 2.54)

# Table 2
  # U(0.1) at 5% significance level (skewness = 0, kurtosis = 1.8)
  # Tukey(0, 1, 1.25, 1.25) at 5% significance level (skewness=0, kurtosis=1.76)
  # TRUNC(-2, 2) at 5% significance level (skewness=0, kurtosis=2.36)

# This is specifically looking at power comparisons of six short tailed tests.
# Tukey is used interchangeably with GLD in the text. 
# The truncated normal distribution is denoted as TruncN(a, b)

# Figure 2
  # GLD(0.0,1.0,−0.10,−0.10) at 5% significance level (skewness = 0,kurtosis = 6.78)
  # ScConN(0.05,3) at 5% significance level (skewness = 0, kurtosis = 7.65)
  # GLD(0.0,1.0,−0.15,−0.15) at 5% significance level (skewness = 0, kurtosis = 10.36)

# Table 3
  # t(15) at 5% significance level (skewness = 0,kurtosis = 3.55)
  # Logistic at 5% significance level (skewness = 0,kurtosis = 4.2)
  # Laplace at 5% significance level (skewness = 0,kurtosis = 6.0)

# These are 6 of the 8 long tailed tests in the paper. 
# ScConN stands for Scale Contaminated Normal Distribution. Need to find a reference for this. 
    # The scale-contaminated normal distribution, denoted by ScConN(p, b) is a mixture of two normal distribution 
    # with probability p from a normal distribution N(0, b2) and probability 1 − p from N(0, 1).


# Figure 3
  # CHI(4df) at 5% significance level (skewness = 1.41, kurtosis = 6.00)
  # BETA(2,1) at 5% significance level. (skewness = −0.57, kurtosis = 2.40).

# Table 4
  # Weibull(3, 1) at 5% significance level. (skewness = 0.17, kurtosis = 2.73).
  # Lognormal at 5% significance level. (skewness = 1.07, kurtosis = 5.10).
  # LoConN(0.2,3) at 5% significance level. (skewness = 0.68, kurtosis = 3.09).

# These are for 5 of the 10 asymmetric distributions discussed in the text. 
# LoConN is the location contaminated normal distribution.
  # LoConN(p, a) denotes the distribution of a random variable that is sampled with probability p from a normal   
  # distribution with mean a and variance 1 and with probability 1 − p from a standard normal distribution


# We need to determine the best way to generate random samples for GLD, ScConN, LoConN, Trunc, and Laplace distributions. 
# I believe the rest are in some r packages. We could look for other packages that can provide random samples of the above. 
# I'm not sure what to do with the skewness and kurtosis values, should we measure these for each dist. and show that they match?

# Normality Tests

data <- rnorm(1000, 0, 1) # Generated Normal data to apply the tests on.

# Note About Theory:
# Check the theory on each of these to double check that they match. The documentation for 
# the Jarque Bera and D'Agostino Pearson test lack any theory, and there is no overlapping sources
# with the papers. 

############################################

# Shapiro Wilks Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html
# R documentation referencing the same source material as our journal. 
# Sample size for this test must be between 3 and 5000. 

SW <- shapiro.test(data) 

# SW$Statistic: value of Test
# SW$p.value: p value of test
# SW$method: character string "Shapiro-Wilk normality test"
# SW$data.name: character string giving the name(s) of the data.


############################################

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

# Cramer Von Mises Test - Library: nortest
# Link: https://www.rdocumentation.org/packages/nortest/versions/1.0-4/topics/cvm.test

CVM <- cvm.test(data)

# CVM$statistic: value of the test statistic
# CVM$p.value: p-value of the test
# CVM$method: character string indicating what type of test was performed.
# CVM$data.name: character string giving the name(s) of the data.

############################################

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

# Jarque-Bera Test

# We have a couple options, I'm not sure which is best in this case. 

# https://search.r-project.org/CRAN/refmans/DescTools/html/JarqueBeraTest.html
# https://www.rdocumentation.org/packages/tsoutliers/versions/0.3/topics/jarque.bera.test
# https://search.r-project.org/CRAN/refmans/moments/html/jarque.test.html
# https://rdrr.io/cran/tseries/man/jarque.bera.test.html
# https://www.rdocumentation.org/packages/fBasics/versions/3011.87/topics/NormalityTests

############################################

# D'Agostino Pearson Test

# We have a couple options, I'm not sure which is best in this case. 

# https://www.rdocumentation.org/packages/fBasics/versions/3011.87/topics/NormalityTests
# https://rdrr.io/bioc/globalSeq/man/omnibus.html
# https://rdrr.io/cran/PoweR/man/stat0006.DAgostinoPearson.html


