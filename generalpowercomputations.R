library(nortest)
library(lawstat)
library(fBasics)

# Edited source code from dagoTest in fBasics.
.skewness.test <- function(x)
{
  # Internal Function for D'Agostino Normality Test:
  
  # FUNCTION:
  
  DNAME = deparse(substitute(x))
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
  SD3 = sqrt(6*(n-2)/((n+1)*(n+3)))
  U3 = a3/SD3
  b  = (3*(n**2+27*n-70)*(n+1)*(n+3))/((n-2)*(n+5)*(n+7)*(n+9))
  W2 = sqrt(2*(b-1))-1
  delta = 1/sqrt(log(sqrt(W2)))
  a = sqrt(2/(W2-1))
  Z3 = delta*log((U3/a)+sqrt((U3/a)**2+1))
  pZ3 = 2*(1-pnorm(abs(Z3),0,1))
  names(Z3) = "Z3"
  
  # Result:
  RVAL = list(
    statistic = Z3,
    p.value = pZ3,
    method = "D'Agostino Skewness Normality Test",
    data.name = DNAME)
  
  # Return Value:
  class(RVAL) = "htest"
  RVAL
}

.kurtosis.test <- function(x)
  {
    # Internal Function for D'Agostino Normality Test:
    
    # FUNCTION:
    
    DNAME = deparse(substitute(x))
    
    if (exists("complete.cases")) {
      test = complete.cases(x)
    } else {
      test = !is.na(x)
    }
    x = x[test]
    n = length(x)
    meanX = mean(x)
    s =  sqrt(mean((x-meanX)**2))
    a4 = mean((x-meanX)**4)/s**4
    SD4 = sqrt(24*(n-2)*(n-3)*n/((n+1)**2*(n+3)*(n+5)))
    U4 = (a4-3+6/(n+1))/SD4
    B = (6*(n*n-5*n+2)/((n+7)*(n+9)))*sqrt((6*(n+3)*(n+5))/(n*(n-2)*(n-3)))
    A = 6+(8/B)*((2/B)+sqrt(1+4/(B**2)))
    jm = sqrt(2/(9*A))
    pos = ((1-2/A)/(1+U4*sqrt(2/(A-4))))**(1/3)
    Z4 = (1-2/(9*A)-pos)/jm
    pZ4 = 2*(1-pnorm(abs(Z4),0,1))
    names(Z4) = "Z4"
    
    # Result:
    RVAL = list(
      statistic = Z4,
      p.value = pZ4,
      method = "D'Agostino Kurtosis Normality Test",
      data.name = DNAME)
    
    # Return Value:
    class(RVAL) = "htest"
    RVAL
  }

.omnibus.test <-
  function(x)
  {
    # Internal Function for D'Agostino Normality Test:
    
    # FUNCTION:
    
    DNAME = deparse(substitute(x))
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
    pomni = 1-pchisq(omni,2)
    names(omni) = "Chi2"
    
    # Result:
    RVAL = list(
      statistic = omni,
      method = "D'Agostino Omnibus Normality Test",
      p.value = pomni,
      data.name = DNAME)
    
    # Return Value:
    class(RVAL) = "htest"
    RVAL
  }

dagotest =
  function(x, title = NULL, description = NULL)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Performs the D'Agostino normality test
    
    # Source:
    #   This function was inspired by ...
    #   http://adela.karlin.mff.cuni.cz/~klaster/vyuka/
    
    # FUNCTION:
    
    # Data Set Name:
    DNAME = deparse(substitute(x))
    
    # Convert Type:
    if (class(x) == "fREG") x = residuals(x)
    x = as.vector(x)
    
    # Call:
    call = match.call()
    
    # Test:
    ans = NA
    test = .omnibus.test(x)
    skew = .skewness.test(x)
    kurt = .kurtosis.test(x)
    test$data.name = DNAME
    PVAL = c(test$p.value, skew$p.value, kurt$p.value)
    names(PVAL) = c(
      "Omnibus  Test",
      "Skewness Test",
      "Kurtosis Test")
    test$p.value = PVAL
    STATISTIC = c(test$statistic, skew$statistic, kurt$statistic)
    names(STATISTIC) = c(
      "Chi2 | Omnibus",
      "Z3  | Skewness",
      "Z4  | Kurtosis")
    test$statistic = STATISTIC
    class(test) = "list"
    
    # Add:
    if (is.null(title)) title = "D'Agostino Normality Test"
    if (is.null(description)) description = description()
    
    # Return Value:
    new("fHTEST",
        call = call,
        data = list(x = x),
        test = test,
        title = as.character(title),
        description = as.character(description) )
  }

alpha <- 0.05
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)

SW <- numeric()
KS <- numeric()
LL <- numeric()
AD <- numeric()
DP <- numeric()
JB <- numeric()
CVM <- numeric()
CSQ <- numeric()
SWcrit <- numeric()
KScrit  <- numeric()
LLcrit  <- numeric()
ADcrit <- numeric()
DPcrit <- numeric()
JBcrit <- numeric()
CVMcrit <- numeric()
CSQcrit <- numeric()
for (a in 1:15){
  
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    SW[i] <- shapiro.test(x)$statistic
    KS[i] <- ks.test(x, "pnorm")$statistic
    LL[i] <- lillie.test(x)$statistic 
    AD[i] <- ad.test(x)$statistic
    DP[i]<- dagotest(x)@test$statistic[1]
    JB[i] <- rjb.test(x,option="JB")$statistic
    CVM[i] <- cvm.test(x)$statistic
    
    c <- nclass.scott(x)
    CSQ[i] <- pearson.test(x, n.classes=c)$statistic
  }
  
  SW <- sort(SW)
  KS <- sort(KS)
  LL <- sort(LL)
  AD <- sort(AD)
  DP <- sort(DP)
  JB <- sort(JB)
  CVM <- sort(CVM)
  CSQ <- sort(CSQ)
  
  SWcrit[a] <- quantile(SW, alpha)
  KScrit[a]  <- quantile(KS, 1-alpha)
  LLcrit[a]  <- quantile(LL, 1-alpha)
  ADcrit[a] <- quantile(AD, 1-alpha)
  DPcrit[a] <- quantile(DP, 1-alpha)
  JBcrit[a] <- quantile(JB, 1-alpha)
  CVMcrit[a] <- quantile(CVM, 1-alpha)
  CSQcrit[a] <- quantile(CSQ, 1-alpha)
}
Critvaluematrix <- cbind(ssizes, SWcrit, KScrit, LLcrit, ADcrit, DPcrit, JBcrit, CVMcrit, CSQcrit)
write.table(Critvaluematrix, "critical_values.csv")

CSQcrit <- Critvaluematrix[, 9]

powerSW <- c()
powerKS <- c()
powerLL <- c()
powerAD <- c()
powerDP <- c()
powerJB <- c()
powerCVM <- c()
powerCSQ <- c()

for (a in 1:15){
  testSW <- numeric()
  testKS <- numeric()
  testLL <- numeric()
  testAD <- numeric()
  testDP <- numeric()
  testJB <- numeric()
  testCVM <- numeric()
  testCSQ <- numeric()
  
  for(i in 1:10000) { 
    n <- ssizes[a]
    dist <- runif(n,0,1)
    
    SWd <- shapiro.test(dist)$statistic 
    
    distsd <- sqrt(1/12)
    distavg <- 0.5
    distKS <- (dist-distavg)/distsd
    KSd <- ks.test(distKS, "pnorm")$statistic
    
    LLd <- lillie.test(dist)$statistic 
    ADd <- ad.test(dist)$statistic
    DPd <- dagotest(dist)@test$statistic[1]
    JBd <- rjb.test(dist,option="JB")$statistic
    CVMd <- cvm.test(dist)$statistic
    
    c <- nclass.scott(dist)
    CSQd <- pearson.test(dist, n.classes=c)$statistic
    
    ifelse(SWd <= SWcrit[a], testSW[i] <- 1, testSW[i] <- 0)
    ifelse(KSd >= KScrit[a] | KSd <= -KScrit[a], testKS[i] <- 1, testKS[i] <- 0)
    ifelse(LLd >= LLcrit[a], testLL[i] <- 1, testLL[i] <- 0)
    ifelse(ADd >= ADcrit[a], testAD[i] <- 1, testAD[i] <- 0)
    ifelse(DPd >= DPcrit[a], testDP[i] <- 1, testDP[i] <- 0)
    ifelse(JBd >= JBcrit[a], testJB[i] <- 1, testJB[i] <- 0)
    ifelse(CVMd >= CVMcrit[a], testCVM[i] <- 1, testCVM[i] <- 0)
    ifelse(CSQd >= CSQcrit[a], testCSQ[i] <- 1, testCSQ[i] <- 0)
    
  }
  powerSW <- c(powerSW, sum(testSW)/10000)
  powerKS <- c(powerKS, sum(testKS)/10000)
  powerLL <- c(powerLL, sum(testLL)/10000)
  powerAD <- c(powerAD, sum(testAD)/10000)
  powerDP <- c(powerDP, sum(testDP)/10000)
  powerJB <- c(powerJB, sum(testJB)/10000)
  powerCVM <- c(powerCVM, sum(testCVM)/10000)
  powerCSQ <- c(powerCSQ, sum(testCSQ)/10000)
  }
Unifpowermatrix <- cbind(ssizes, powerSW,  powerKS, powerLL, powerAD, powerDP, powerJB, powerCVM, powerCSQ)
# We can use knitr to output tables as needed

n <- 10
CSQ <- numeric()
CSQcrit <- numeric()
for (i in 1:50000){
    x <- rnorm(n, 0, 1)
    c <- nclass.scott(x)
    CSQ[i] <- pearson.test(x, n.classes=c)$statistic
}
CSQcrit <- quantile(CSQ, 1-0.05)
CSQstat <- numeric()
testCSQ <- numeric()
powerCSQ <- c()
for(i in 1:10000) { 
  dist <- runif(n, 0, 1)
  c <- nclass.scott(dist)
  stat1 <- pearson.test(dist, n.classes=c)$statistic
  CSQstat[i] <- stat1
  ifelse(CSQstat[i] >= CSQcrit, testCSQ[i] <- 1, testCSQ[i] <- 0)
}
powerCSQ <- sum(testCSQ)/10000
powerCSQ

