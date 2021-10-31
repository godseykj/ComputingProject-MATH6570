# Test file to make sure I could commit. 

set.seed(1234) # Seed to discuss the tests and distributions with. 

# Distributions


# Normality Tests

data <- rnorm(1000, 0, 1) # Generated Normal data to apply the tests on. 

############################################

# Shapiro Wilks Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/shapiro.test.html
# The paper and the R documentation both credit Patrick Royston's papers, so the theory should be correct.
# Patrick Royston (1982). An extension of Shapiro and Wilk's W test for normality to large samples. Applied Statistics, 31, 115–124. doi: 10.2307/2347973.
# Patrick Royston (1982). Algorithm AS 181: The W test for Normality. Applied Statistics, 31, 176–180. doi: 10.2307/2347986.
# Sample size for this test must be between 3 and 5000. 

SW <- shapiro.test(data) 

# SW$Statistic: value of Test
# SW$p.value: p value of test - p > 0.05 implies normal data
# SW$method: character string "Shapiro-Wilk normality test"
# SW$data.name: character string giving the name(s) of the data.


############################################

# Kolmogorov Smirnov Test - Library: stats
# Link: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/ks.test.html
# I cannot find whether they used the two-sided, right sided, or left sided version of the test. 
# Theory appears to be the same, someone should double check and confirm. They cite different resources. 

KS <- ks.test(data, pnorm(0,1), alternative="two.sided", exact=TRUE)
# statistic: value of the test statistic
# p.value: p-value of the test - p > 0.05 implies normal data
# alternative: character string describing the alternative hypothesis.
# method: character string indicating what type of test was performed.
# data.name: character string giving the name(s) of the data.

############################################




