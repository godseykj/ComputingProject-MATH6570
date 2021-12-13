library(nortest)
library(lawstat)
library(gld)
library(VGAM)
library(truncnorm)
library(ggplot2)

omnibus.test <- function(x){
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

alpha <- 0.05
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)

# Obtaining Critical Values - Found in generalpowercomputations.r

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
    DP[i]<- omnibus.test(x)$statistic
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

# Power Function - Found in powerfunction.r

cvalues <- read.table("critical_values.csv",header=TRUE)
ssizes <- cvalues[,"ssizes"]; SWcrit <- cvalues[,"SWcrit"]; LLcrit <- cvalues[,"LLcrit"]; 
KScrit <- cvalues[,"KScrit"]; ADcrit <- cvalues[,"ADcrit"]; JBcrit <- cvalues[,"JBcrit"]; 
CVMcrit <- cvalues[,"CVMcrit"]; DPcrit <- cvalues[,"DPcrit"]; CSQcrit <- cvalues[,"CSQcrit"]
getpower <- function(distmean, distvar, dist_function, p1=NULL, p2=NULL, p3=NULL, p4=NULL){
  powerSW <- c()
  powerKS <- c()
  powerLL <- c()
  powerAD <- c()
  powerDP <- c()
  powerJB <- c()
  powerCVM <- c()
  powerCSQ <- c()
  distsd <- sqrt(distvar)
  
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
      if(is.null(p1)==FALSE){
        if(is.null(p2)==FALSE){
          if(is.null(p3)==FALSE){
            if(is.null(p4)==FALSE){
              dist <- as.vector(sapply(n, noquote(dist_function), p1, p2, p3, p4))
            } else{dist <- as.vector(sapply(n, noquote(dist_function), p1, p2, p3))}
          } else{dist <- as.vector(sapply(n, noquote(dist_function), p1, p2))}
        } else{dist <- as.vector(sapply(n, noquote(dist_function), p1))}
      } else{dist <- as.vector(sapply(n, noquote(dist_function)))}
      distKS <- (dist-distmean)/distsd
      SWd <- shapiro.test(dist)$statistic 
      KSd <- ks.test(distKS, "pnorm")$statistic
      LLd <- lillie.test(dist)$statistic 
      ADd <- ad.test(dist)$statistic
      DPd <- omnibus.test(dist)
      JBd <- rjb.test(dist, "JB")$statistic
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
  powermatrix <<- cbind(ssizes, powerSW, powerKS, powerLL, powerAD, powerDP, powerJB, powerCVM, powerCSQ)
}

# Random Variate Generation for distributions without a function

rScConN <- function(n,p,b){
  k <- runif(n)
  k <- ifelse(k <= p, 1, 0)
  ScConNdist <- k*rnorm(n, mean = 0, sd=b) + (1-k)*rnorm(n)
  ScConNdist
}

rLoConN <- function(n,p,a){
  k <- runif(n)
  k <- ifelse(k <= p, 1, 0)
  LoConNdist <- k*rnorm(n, mean = a, sd=1) + (1-k)*rnorm(n)
  LoConNdist
}

rlog <- function(n){
  x <- rnorm(n,0,1)
  dist <- exp(x)
}

# Power calculation for every distribution

# Table 2 Short Tailed Distributions

# Uniform 
Uniformpower <- getpower(0, 1, "runif")

# Tukey(0, 1, 1.25, 1.25)
lam1 <- 0; lam2 <- 1; lam3 <- 1.25; lam4 <- 1.25
gldmean <- gld.moments(c(lam1,lam2,lam3,lam4))[1]
gldvar <- gld.moments(c(lam1,lam2,lam3,lam4))[2]
system.time(Tukeypower <- getpower(gldmean, gldvar, "rgl",lam1, lam2, lam3, lam4))

# Trunc(-2, 2)
Truncpower <- getpower(0, 1, "rtruncnorm", -2, 2)

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

#lognormal (standard)
logpower <- getpower(exp(1/2),exp(2)-exp(1),"rlog")

#LoConN(0.2,3)
p <- 0.2
a <- 3
meanN1 <- a; meanN2 <- 0; varN1 <- 1; varN2 <- 1
meanlcn <- p*meanN1 + (1-p)*meanN2
varlcn <- (p^2)*varN1 + ((1-p)^2)*varN2
LCnom <- getpower(meanlcn, varlcn, "rLoConN", p, a)

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

# Graphing the power curves for the necessary distributions - Found in powertable.r

graph <- function(filename, distribution){
  table <- read.table(filename)
  data <- data.frame(table)
  ss <- data[,1]
  
  colors <- c("SW"="steelblue","KS"="red3","LL"="green3","AD"="blueviolet",
              "JB"="orange","CVM"="mediumpurple1", "DP"="cyan3", "CSQ"="rosybrown2")
  
  ggplot(data, aes(x=seq(1,15), y=seq(0,1,10))) + 
    geom_line(aes(y=powerSW, color="SW"), size=2) +
    geom_point(aes(y=powerSW, color="SW"), size=3.5) +
    geom_line(aes(y=powerKS, color="KS"),size=2) + 
    geom_point(aes(y=powerKS, color="KS"), size=3.5) +
    geom_line(aes(y=powerLL, color="LL"),size=2) +
    geom_point(aes(y=powerLL, color="LL"), size=3.5) +
    geom_line(aes(y=powerAD, color="AD"),size=2) +
    geom_point(aes(y=powerAD, color="AD"), size=3.5) +
    geom_line(aes(y=powerJB, color="JB"),size=2) +
    geom_point(aes(y=powerJB, color="JB"), size=3.5) +
    geom_line(aes(y=powerCVM, color="CVM"),size=2) +
    geom_point(aes(y=powerCVM, color="CVM"), size=3.5) +
    geom_line(aes(y=powerDP, color="DP"),size=2) +
    geom_point(aes(y=powerDP, color="DP"), size=3.5) +
    geom_line(aes(y=powerCSQ, color="CSQ"),size=2) +
    geom_point(aes(y=powerCSQ, color="CSQ"), size=3.5) +
    labs(x="sample size", y="power",title = distribution, color="Legend") +
    scale_x_continuous(breaks=seq(1,15),labels=c("10","15","20","25","30","40","50","100","200","300","400","500","1000","1500","2000")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values=colors)
}

graph("Figure1/gld-a.csv", "GLD(0,1,0.75,0.75)")
graph("Figure1/gld-b.csv", "GLD(0,1,0.5,0.5)")
graph("Figure1/gld-c.csv", "GLD(0,1,0.25,0.25)")
graph("Figure2/gld-a-figure2.csv", "GLD(0,1,-0.1,-0.1)")
graph("Figure2/scnom-b-figure2.csv", "ScConN(0.05,3)")
graph("Figure2/gld-c-figure2.csv", "GLD(0,1,−0.15,−0.15)")
graph("Figure3/chi.csv", "CHI(4df)")
graph("Figure3/beta.csv", "BETA(2,1)")


# Tables were made based on outputs from power function using knitr::kable. - Found in Project_Report.rmd

# Error Analysis of Tabled Data.  - Example of 1 Distribution. - Found in ErrorAnalysis.r

# This data was from our paper, we just switched values around for each distribution.
Theory <- matrix(
  c(10, 0.1381, 0.0714, 0.1188, 0.1365, 0.1111, 0.1157, 0.1329, 0.0000,
    20, 0.2558, 0.0965, 0.217, 0.2634, 0.1448, 0.1615, 0.2559, 0.0279,
    30, 0.3874, 0.1275, 0.3183, 0.4082, 0.181, 0.2093, 0.3886, 0.0832,
    50, 0.6149, 0.189, 0.5134, 0.6502, 0.2957, 0.3651, 0.6248, 0.1966,
    100, 0.9285, 0.3382, 0.8463, 0.9425, 0.681, 0.7476, 0.9294, 0.4663,
    300, 0.9999, 0.8235, 0.9995, 1.0000, 0.9994, 0.9997, 1.0000, 0.9129,
    500, 1.0000, 0.9789, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.9885,
    1000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000,
    2000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000),
  nrow=9,
  ncol=9,
  byrow=TRUE)
ss <- Theory[, 1]

# Upper/Lower bound for the intervals based on theoretical means from the paper. 
upperbound <- matrix(NA, 9, 9)
upperbound[,1] <- ss
for (i in 1:9){
  for (j in 2:9){
    upperbound[i, j] <- Theory[i, j]+3*sqrt(Theory[i, j]*(1-Theory[i, j])/10000)
  }
}
lowerbound <- matrix(NA, 9, 9)
lowerbound[,1] <- ss
for (i in 1:9){
  for (j in 2:9){
    lowerbound[i, j] <- Theory[i, j]-3*sqrt(Theory[i, j]*(1-Theory[i, j])/10000)
  }
}

# Grabbing our experimental results
Experimental <- read.table("Table4/loconnorm.csv")
Experimental <- Experimental[Experimental$ssizes %in% ss,]

# Comparison of experimental and theoretical values.
Approval <- matrix(NA, 9, 9)
Approval[, 1] <- ss
for (i in 1:9){
  for (j in 2:9){
    b <- upperbound[i, j]
    c <- lowerbound[i, j]
    d <- Experimental[i, j]
    ifelse(d<=b & d>=c, Approval[i, j] <- 1, Approval[i, j] <- 0)
  }
}

colnames(Approval) <- c("Sample", "SW", "KS", "LL", "AD", "DP", "JB", "CVM", "CSQ")
write.table(Approval, "Approval/LocConNorm")

