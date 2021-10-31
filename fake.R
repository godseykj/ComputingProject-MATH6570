# Test file to make sure I could commit. 

set.seed(1234) # Seed to discuss the tests and distributions with. 

# Distributions


# Normality Tests

data <- rnorm(1000, 0, 1) # Generated Normal data to apply the tests on.

# Note About Theory:
# Check the theory on each of these to double check that they match. The documentation for 
# the Jarque Bera and D'Agostino Pearson tests are awful, and there is no overlapping sources
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


