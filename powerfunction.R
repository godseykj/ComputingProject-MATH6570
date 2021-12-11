library(nortest)
library(lawstat)
library(fBasics)
library(gld)
library(VGAM)
library(truncnorm)
library(ggamma)

#inputs are theoretical mean, theoretical variance, r function to generate distribution (in quotations, i.e. "rnorm"), and optional...
  #parameters which are any parameters that the distribution function requires (beyond sample size)
cvalues <- read.table("critical_values.csv",header=TRUE)
ssizes <- cvalues[,"ssizes"]; SWcrit <- cvalues[,"SWcrit"]; LLcrit <- cvalues[,"LLcrit"]; KScrit <- cvalues[,"KScrit"]; ADcrit <- cvalues[,"ADcrit"]; JBcrit <- cvalues[,"JBcrit"]; CVMcrit <- cvalues[,"CVMcrit"]
getpower <- function(distmean, distvar, dist_function, p1=NULL, p2=NULL, p3=NULL, p4=NULL){
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
      if(is.null(p1)==FALSE){
        if(is.null(p2)==FALSE){
          if(is.null(p3)==FALSE){
            if(is.null(p4)==FALSE){
              dist <- as.vector(sapply(n, noquote(dist_function), p1, p2, p3, p4))
            } else{dist <- as.vector(sapply(n, noquote(dist_function), p1, p2, p3))}
          } else{dist <- as.vector(sapply(n, noquote(dist_function), p1, p2))}
        } else{dist <- as.vector(sapply(n, noquote(dist_function), p1))}
      } else{dist <- as.vector(sapply(n, noquote(dist_function)))}
      distsd <- sqrt(distvar)
      distKS <- (dist-distmean)/distsd
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
  powermatrix <<- cbind(ssizes, powerSW, powerKS, powerLL, powerAD, powerJB, powerCVM)
}

## homemade functions for distributions

#scale contaminated function
rScConN <- function(n,p,b){
  k <- runif(n)
  k <- ifelse(k <= p, 1, 0)
  ScConNdist <- k*rnorm(n, mean = 0, sd=b) + (1-k)*rnorm(n)
  ScConNdist
}

#location contaminated function
rLoConN <- function(n,p,a){
  k <- runif(n)
  k <- ifelse(k <= p, 1, 0)
  LoConNdist <- k*rnorm(n, mean = a, sd=1) + (1-k)*rnorm(n)
  LoConNdist
}

#lognormal
rlog <- function(n){
  x <- rnorm(n,0,1)
  dist <- exp(x)
}

## Using the function

# Table 2 Short Tailed Distributions

# Uniform 
Uniformpower <- getpower(0, 1, "runif")

# Tukey(0, 1, 1.25, 1.25)

lam1 <- 0; lam2 <- 1; lam3 <- 1.25; lam4 <- 1.25
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]
Tukeypower <- getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4)

# Trunc(-2, 2)
Truncpower <- getpower(0, 1, "rtruncnorm", -2, 2) # Weird error


# Table 3 Long Tailed Distributions

#t distribution - t(15)
tmean <- 0
df <- 15
tvar <- df / (df-2)
Tpower <- getpower(tmean, tvar, "rt", df)

#logistic  (Standard)
logmean <- 0
logvar <- (pi^2)/3
logsd <- sqrt(logvar)
Logisticpower <- getpower(logmean, logvar, "rlogis")

#Laplace (Standard)
laplacemean <- 0
laplacevar <- 2
laplacesd <- sqrt(laplacevar)
Laplacepower <- getpower(laplacemean, laplacevar, "rlaplace")

# Asymptotic Distributions Table 4

#weibull(3,1)
alpha <- 3; beta <- 1
wmean <- beta*gamma(1 + (1/alpha))
wvar <- (beta^2)*(gamma(1+(2/alpha))-(gamma(1 + (1/alpha)))^2)
weibullpower <- getpower(wmean, wvar, "rweibull",alpha,beta)
#this one is off (KS, LL, JB, CVM), lots of KS warnings

#ggamma method
a <- 3
p <- 1
wgpower <- getpower(a,(a^2), "rggamma",a,p,p)
#this one is off too

#lognormal (standard)

logpower <- getpower(exp(1/2),exp(2)-exp(1),"rlog")
#lots of cvm warnings: p-value is smaller than 7.37e-10, cannot be computed more accurately
#very off

#LoConN(0.2,3)
p <- 0.2
a <- 3
meanN1 <- a; meanN2 <- 0; varN1 <- 1; varN2 <- 1
meanlcn <- p*meanN1 + (1-p)*meanN2
varlcn <- (p^2)*varN1 + ((1-p)^2)*varN2
LCnom <- getpower(meanlcn, varlcn, "rLoConN", p, a)
#ks is wrong

# Figure 1

#a - GLD(0,1,0.75,0.75)
lam1 <- 0; lam2 <- 1; lam3 <- 0.75; lam4 <- 0.75
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]
GLD1a <- getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4)

#b - GLD(0,1,0.5,0.5)
lam1 <- 0; lam2 <- 1; lam3 <- 0.5; lam4 <- 0.5
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

GLD1b <- getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4)

#c - GLD(0,1,0.25,0.25)
lam1 <- 0; lam2 <- 1; lam3 <- 0.25; lam4 <- 0.25
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

GLD1c <- getpower(gldmean, gldvar, "rgl", lam1, lam2, lam3, lam4)

# Figure 2 

#a - GLD(0,1,-0.1,-0.1)
lam1 <- 0; lam2 <- 1; lam3 <- -0.10; lam4 <- -0.10
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

GLD2a <- getpower(gldmean, gldvar, "rgl", lam1, lam2, lam3, lam4)

# Scale Contanimated Normal (0.05, 3)
p <- 0.05
b <- 3
meanN1 <- 0; meanN2 <- 0; varN1 <- b; varN2 <- 1
meanscn <- p*meanN1 + (1-p)*meanN2
varscn <- (p^2)*varN1 + ((1-p)^2)*varN2
SCnom <- getpower(meanscn, varscn, "rScConN", p, b)
#ks and cvm are wrong

#c - GLD(0,1,-0.15,-0.15)
lam1 <- 0; lam2 <- 1; lam3 <- -0.15; lam4 <- -0.15
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

GLD2c <- getpower(gldmean, gldvar, "rgl", lam1, lam2, lam3, lam4)

# Figure 3

# Chi(4df)
df <- 4
chimean <- df
chivar <- 2*df
chipower <- getpower(chimean, chivar, "rchisq",df)

# Beta(2,1)
alpha <- 2
beta <- 1
betamean <- alpha / (alpha + beta)
betavar <- (alpha*beta) / (((alpha+beta)^2)*(alpha+beta+1))
betapower <- getpower(betamean, betavar, "rbeta",alpha, beta)