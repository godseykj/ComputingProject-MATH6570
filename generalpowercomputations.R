library(nortest)
library(lawstat)
library(fBasics)

alpha <- 0.05
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)

SW <- numeric()
KS <- numeric()
LL <- numeric()
AD <- numeric()
#DP <- numeric()
JB <- numeric()
CVM <- numeric()
#CSQ <- numeric()
SWcrit <- numeric()
KScrit  <- numeric()
LLcrit  <- numeric()
ADcrit <- numeric()
#DPcrit <- numeric()
JBcrit <- numeric()
CVMcrit <- numeric()
#CSQcrit <- numeric()
for (a in 1:15){
  
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    SW[i] <- shapiro.test(x)$statistic 
    KS[i] <- ks.test(x, "pnorm")$statistic
    LL[i] <- lillie.test(x)$statistic 
    AD[i] <- ad.test(x)$statistic
    #DPtest <- dagoTest(x)
    #DP[i] <- DPtest@test$statistic[1]
    JB[i] <- rjb.test(x, "JB")$statistic
    CVM[i] <- cvm.test(x)$statistic
    #CSQ[i] <- pearson.test(x)$statistic
  }
  
  SW <- sort(SW)
  KS <- sort(KS)
  LL <- sort(LL)
  AD <- sort(AD)
  #DP <- sort(DP)
  JB <- sort(JB)
  CVM <- sort(CVM)
  #CSQ <- sort(CSQ)
  
  SWcrit[a] <- quantile(SW, alpha)
  KScrit[a]  <- quantile(KS, 1-alpha)
  LLcrit[a]  <- quantile(LL, 1-alpha)
  ADcrit[a] <- quantile(AD, 1-alpha)
  #DPcrit[a] <- quantile(DP, 1-alpha)
  JBcrit[a] <- quantile(JB, 1-alpha)
  CVMcrit[a] <- quantile(CVM, 1-alpha)
  #CSQcrit[a] <- quantile(CSQ, 1-alpha)
}
Critvaluematrix <- cbind(ssizes, SWcrit, LLcrit, KScrit, ADcrit, JBcrit, CVMcrit)

powerSW <- c()
powerKS <- c()
powerLL <- c()
powerAD <- c()
#powerDP <- c()
powerJB <- c()
powerCVM <- c()
#powerCSQ <- c()

for (a in 1:15){
  testSW <- numeric()
  testKS <- numeric()
  testLL <- numeric()
  testAD <- numeric()
  #testDP <- numeric()
  testJB <- numeric()
  testCVM <- numeric()
  #testCSQ <- numeric()
  
  for(i in 1:10000) { 
    n <- ssizes[a]
    dist <- runif(n,0,1)
    distsd <- sqrt(1/12)
    distavg <- 0.5
    distKS <- (dist-distavg)/distsd
    SWd <- shapiro.test(dist)$statistic 
    KSd <- ks.test(distKS, "pnorm")$statistic
    LLd <- lillie.test(dist)$statistic 
    ADd <- ad.test(dist)$statistic
    #DPtest <- dagoTest(x)
    #DPd <- DPtest@test$statistic[1]
    JBd <- rjb.test(dist, "JB")$statistic
    CVMd <- cvm.test(dist)$statistic
    #CSQd <- pearson.test(dist)$statistic
    
    ifelse(SWd <= SWcrit[a], testSW[i] <- 1, testSW[i] <- 0)
    ifelse(KSd >= KScrit[a] | KSd <= -KScrit[a], testKS[i] <- 1, testKS[i] <- 0)
    ifelse(LLd >= LLcrit[a], testLL[i] <- 1, testLL[i] <- 0)
    ifelse(ADd >= ADcrit[a], testAD[i] <- 1, testAD[i] <- 0)
    #ifelse(DPd >= DPcrit[a], testDP[i] <- 1, testDP[i] <- 0)
    ifelse(JBd >= JBcrit[a], testJB[i] <- 1, testJB[i] <- 0)
    ifelse(CVMd >= CVMcrit[a], testCVM[i] <- 1, testCVM[i] <- 0)
    #ifelse(CSQd >= CSQcrit[a], testCSQ[i] <- 1, testCSQ[i] <- 0)
    
  }
  powerSW <- c(powerSW, sum(testSW)/10000)
  powerKS <- c(powerKS, sum(testKS)/10000)
  powerLL <- c(powerLL, sum(testLL)/10000)
  powerAD <- c(powerAD, sum(testAD)/10000)
  #powerDP <- c(powerDP, sum(testDP)/10000)
  powerJB <- c(powerJB, sum(testJB)/10000)
  powerCVM <- c(powerCVM, sum(testCVM)/10000)
  #powerCSQ <- c(powerCSQ, sum(testCSQ)/10000)
}
Unifpowermatrix <- cbind(ssizes, powerSW, powerLL, powerKS, powerAD, powerJB, powerCVM)

# We can use knitr to output tables as needed 
