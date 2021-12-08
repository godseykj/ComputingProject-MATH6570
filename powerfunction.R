library(nortest)
library(lawstat)
library(fBasics)
#inputs are theoretical mean, theoretical variance, r function to generate distribution (in quotations, i.e. "rnorm"), and optional...
  #parameters which are any parameters that the distribution function requires (beyond sample size)
getpower <- function(distmean, distvar, dist_function, p1=NULL, p2=NULL, p3=NULL, p4=NULL){
  cvalues <- read.table("critical_values.csv",header=TRUE)
  ssizes <- cvalues[,1]; SWcrit <- cvalues[,2]; LLcrit <- cvalues[,3]; KScrit <- cvalues[,4]; ADcrit <- cvalues[,5]; JBcrit <- cvalues[,6]; CVMcrit <- cvalues[,7]
  
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
      #distmean <- 0.5
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
  Unifpowermatrix <<- cbind(ssizes, powerSW, powerKS, powerLL, powerAD, powerJB, powerCVM)
}

## Using the function

#t distribution - t(15)
tmean <- 0
df <- 15
tvar <- df / (df-2)
getpower(tmean, tvar, "rt", df)

#logistic  (assuming standard)
logmean <- 0
logvar <- (pi^2)/3
logsd <- sqrt(logvar)
getpower(logmean, logvar, "rlogis")

#laplace
laplacemean <- 0
laplacevar <- 2
laplacesd <- sqrt(laplacevar)
library(VGAM)
getpower(laplacemean, laplacevar, "rlaplace")

#weibull(3,1)
gam <- 3; alpha <- 1
wmean <- alpha*gamma(1 + (1/gam))
wvar <- (alpha^2)*gamma(1+(2/gam)) - (alpha*gamma(1 + 1/gam))^2
getpower(wmean, wvar, "rweibull",gam,alpha)
#this one is off (KS, LL, JB, CVM), lots of KS warnings

#lognormal (standard)
getpower(0,1,"rlnorm")
#lots of cvm warnings: p-value is smaller than 7.37e-10, cannot be computed more accurately
#very off

#LoConN(0.2,3)
getpower() #can't use this without the theoretical mean and variance...

#generalized lambda (table 2)
library(gld)
lam1 <- 0; lam2 <- 1; lam3 <- 1.25; lam4 <- 1.25
#A <- ((1/(1+lam3)) - (1/(1+lam4)))
#B <- ((1/(1+2*lam3)) - (1/(1+2*lam4))) - 2*beta(1 + lam3, 1 + lam4)
#gldmean <- la m1 + (A/lam2)
#gldvar <- lam1 + ((B - A^2)/(lam2^2))
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4)

#generalized lambda (figure 1)
#a - GLD(0,1,0.75,0.75)
lam1 <- 0; lam2 <- 1; lam3 <- 0.75; lam4 <- 0.75
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4)

#b - GLD(0,1,0.5,0.5)
lam1 <- 0; lam2 <- 1; lam3 <- 0.5; lam4 <- 0.5
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4)

#c - GLD(0,1,0.25,0.25)
lam1 <- 0; lam2 <- 1; lam3 <- 0.25; lam4 <- 0.25
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

getpower(gldmean, gldvar, "rgl", lam1, lam2, lam3, lam4)

#generalized lambda (figure 2)
#a - GLD(0,1,-0.1,-0.1)
lam1 <- 0; lam2 <- 1; lam3 <- -0.10; lam4 <- -0.10
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

getpower(gldmean, gldvar, "rgl", lam1, lam2, lam3, lam4)

#c - GLD(0,1,-0.15,-0.15)
lam1 <- 0; lam2 <- 1; lam3 <- -0.15; lam4 <- -0.15
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]

getpower(gldmean, gldvar, "rgl", lam1, lam2, lam3, lam4)
