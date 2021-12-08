#### Table 2

#1 sample size at a time
  #1000 replicates at a time
    #generate sample size n from given distribution
    #calculate all sample statistics
    #write row to csv

######critical values######

alpha <- 0.05
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)

### shapiro wilk ###

SWcrit <- numeric()
for (a in 1:15){
  SW <- numeric()
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    SW[i] <- shapiro.test(x)$statistic
  }
  SW <- sort(SW)
  SWcrit[a] <- quantile(SW, alpha)
}

### kolmogorov smirnov ###

KScrit <- numeric()
for (a in 1:15){
  KS <- numeric()
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    KS[i] <- ks.test(x, pnorm(0,1), "greater")$statistic
  }
  KS <- sort(KS)
  KScrit[a] <- quantile(KS, 1-alpha)
}

### lilliefors ###

library(nortest)
LLcrit <- numeric()
for (a in 1:15){
  LL <- numeric()
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    LL[i] <- lillie.test(x)$statistic
  }
  LL <- sort(LL)
  LLcrit[a] <- quantile(LL, 1-alpha)
}

### cvm ###
library(nortest)
CVMcrit <- numeric()
for (a in 1:15){
  CVM <- numeric()
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    CVM[i] <- cvm.test(x)$statistic
  }
  CVM <- sort(CVM)
  CVMcrit[a] <- quantile(CVM, 1-alpha)
}

### anderson darling ###
library(nortest)
ADcrit <- numeric()
for (a in 1:15){
  AD <- numeric()
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    AD[i] <- ad.test(x)$statistic
  }
  AD <- sort(AD)
  ADcrit[a] <- quantile(AD, 1-alpha)
}

### pearson chi squared ###
#very different strategy described in paper


#function to count how many reject null
#runtime: 56.67 sec
uniformcounts <- function(filename, alpha){
  sim <- paste("UNIF(0,1)")
  ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
  k1 <- numeric()
  k2 <- numeric()
  r <- numeric()
  for (a in 1:15){
    for (j in 1:10){
      j <- j*1000
      for (i in (j-999):j){
        x <- runif(ssizes[a], 0, 1)
        stat <- shapiro.test(x)$statistic
        stat2 <- ks.test(x, pnorm(0.5,1/12), alternative="greater", exact=TRUE)$statistic
        ifelse(stat <= SWcrit[a], k1[i] <- 1, k1[i]<-0)
        ifelse(stat2 >= KScrit[a], k2[i] <- 1, k2[i]<-0) 
      }
      dat <- data.frame(sim=sim, n=ssizes[a], r=j - (j-999) + 1, SW=sum(k1[(j-999):j]),KS=sum(k2[(j-999):j]))
      if(file.exists(filename)){
        write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
      } else{
        write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
      }
    }
  }
}

#function to get power
uniformpower <- function(file_in, file_out){
  counts <- read.table(file_in, header=TRUE)
  library(dplyr)
  power <- counts %>% 
  group_by(sim, n) %>% 
  summarize(sw = sum(SW)/10000) %>%  #ks = sum(KS)/10000) %>% 
  filter(n %in% c(10, 20, 30, 50, 100, 300, 500, 1000, 2000))
  if(file.exists(file_out)){
    write.table(power, file=file_out, append=TRUE, row.names = FALSE, col.names=FALSE)
  } else{
    write.table(power, file=file_out, append=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

uniformpower("table2-uniform-sw.csv","t2-uniform-sw-power.csv")


start_time <- Sys.time()
uniformcounts("t2-unif-sw-ks.csv",0.05)
end_time <- Sys.time()
runtime <- end_time - start_time
runtime

uniformpower("t2-unif-sw-ks.csv")

test <- numeric()
power <- numeric()
stat <- numeric()
for(i in 1:10000) { 
  n <- 300
  dist <- runif(n,0,1)
  stat[i] <- shapiro.test(dist)$statistic
  ifelse(stat[i] <= SWcrit, test[i] <- 1, test[i]<-0)
} 
power <- sum(test)/10000
power

# do not re run
library(dplyr)
write.table(SWcrit, row.names = FALSE,col.names=c("SW"),file = "critical_values.csv")
cvals <- read.table("critical_values.csv", header=TRUE)
cvals <- cvals %>% 
  mutate(LL = LLcrit)
write.table(cvals, row.names = FALSE, col.names = TRUE, file = "critical_values.csv")
cvals <- read.table("critical_values.csv", header=TRUE)
cvals <- cvals %>% 
  mutate(CVM = CVMcrit)
write.table(cvals, row.names = FALSE, col.names = TRUE, file = "critical_values.csv")
cvals <- read.table("critical_values.csv", header=TRUE)
cvals <- cvals %>% 
  mutate(AD = ADcrit)
write.table(cvals, row.names = FALSE, col.names = TRUE, file = "critical_values.csv")
cvals <- cvals %>% 
  mutate(CSQ = CSQcrit)
write.table(cvals, row.names = FALSE, col.names = TRUE, file = "critical_values.csv")

#experimenting with KS
KS <- numeric()
for(i in 1:50000){
  x <- rnorm(300,0,1)
  KS[i] <- ks.test(x,pnorm(0,1),"greater")$statistic
}
KS <- sort(KS); KScrit <- quantile(KS, 1-0.05)
k2 <- numeric()
for (j in 1:10){
  j <- j*1000
  for (i in (j-999):j){
    x <- runif(300, 0, 1)
    #x <- (x-0.5)/sqrt(1/12)
    stat2 <- ks.test(x, pnorm(0.5,sqrt(1/12)), alternative="two.sided", exact=TRUE)$statistic
    ifelse(stat2 >= KScrit | stat2 <= -KScrit, k2[i] <- 1, k2[i]<-0) 
  }
}
power <- sum(k2)/10000
power

#lillie.test
uniformcounts <- function(filename, alpha){
  sim <- paste("UNIF(0,1)")
  ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
  k <- numeric()
  for (a in 1:15){
    for (j in 1:10){
      j <- j*1000
      for (i in (j-999):j){
        x <- runif(ssizes[a], 0, 1)
        stat <- lillie.test(x)$statistic
        ifelse(stat >= LLcrit[a], k[i] <- 1, k[i]<-0)
      }
      dat <- data.frame(sim=sim, n=ssizes[a], r=j - (j-999) + 1, c=sum(k[(j-999):j]))
      if(file.exists(filename)){
        write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
      } else{
        write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
      }
    }
  }
}

uniformpower <- function(file_in, file_out){
  counts <- read.table(file_in, header=TRUE)
  library(dplyr)
  power <- counts %>% 
    group_by(sim, n) %>% 
    summarize(LL = sum(c)/10000) %>%  #ks = sum(KS)/10000) %>% 
    filter(n %in% c(10, 20, 30, 50, 100, 300, 500, 1000, 2000))
  if(file.exists(file_out)){
    write.table(power, file=file_out, append=TRUE, row.names = FALSE, col.names=FALSE)
  } else{
    write.table(power, file=file_out, append=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

uniformcounts("table2-uniform-ll.csv",0.05)
uniformpower("table2-uniform-ll.csv","t2-uniform-ll-power.csv")


#cramer von mises
uniformcounts <- function(filename, alpha){
  sim <- paste("UNIF(0,1)")
  ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
  k <- numeric()
  for (a in 1:15){
    for (j in 1:10){
      j <- j*1000
      for (i in (j-999):j){
        x <- runif(ssizes[a], 0, 1)
        stat <- cvm.test(x)$statistic
        ifelse(stat >= CVMcrit[a], k[i] <- 1, k[i]<-0)
      }
      dat <- data.frame(sim=sim, n=ssizes[a], r=j - (j-999) + 1, c=sum(k[(j-999):j]))
      if(file.exists(filename)){
        write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
      } else{
        write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
      }
    }
  }
}

uniformpower <- function(file_in, file_out){
  counts <- read.table(file_in, header=TRUE)
  library(dplyr)
  power <- counts %>% 
    group_by(sim, n) %>% 
    summarize(CVM = sum(c)/10000) %>%  #ks = sum(KS)/10000) %>% 
    filter(n %in% c(10, 20, 30, 50, 100, 300, 500, 1000, 2000))
  if(file.exists(file_out)){
    write.table(power, file=file_out, append=TRUE, row.names = FALSE, col.names=FALSE)
  } else{
    write.table(power, file=file_out, append=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

uniformcounts("table2-uniform-cvm.csv",0.05)
uniformpower("table2-uniform-cvm.csv","t2-uniform-cvm-power.csv")


#anderson darling
uniformcounts <- function(filename, alpha){
  sim <- paste("UNIF(0,1)")
  ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
  k <- numeric()
  for (a in 1:15){
    for (j in 1:10){
      j <- j*1000
      for (i in (j-999):j){
        x <- runif(ssizes[a], 0, 1)
        stat <- ad.test(x)$statistic
        ifelse(stat >= ADcrit[a], k[i] <- 1, k[i]<-0)
      }
      dat <- data.frame(sim=sim, n=ssizes[a], r=j - (j-999) + 1, c=sum(k[(j-999):j]))
      if(file.exists(filename)){
        write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
      } else{
        write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
      }
    }
  }
}

uniformpower <- function(file_in, file_out){
  counts <- read.table(file_in, header=TRUE)
  library(dplyr)
  power <- counts %>% 
    group_by(sim, n) %>% 
    summarize(AD = sum(c)/10000) %>%  #ks = sum(KS)/10000) %>% 
    filter(n %in% c(10, 20, 30, 50, 100, 300, 500, 1000, 2000))
  if(file.exists(file_out)){
    write.table(power, file=file_out, append=TRUE, row.names = FALSE, col.names=FALSE)
  } else{
    write.table(power, file=file_out, append=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

uniformcounts("table2-uniform-ad.csv",0.05)
uniformpower("table2-uniform-ad.csv","t2-uniform-ad-power.csv")

#combine the tests into one table
swpower <- read.table("Table2/t2-uniform-sw-power.csv", header = TRUE)
n <- swpower[,2]
swpower <- swpower[,3]
llpower <- read.table("Table2/t2-uniform-ll-power.csv", header = TRUE)
llpower <- llpower[,3]
adpower <- read.table("Table2/t2-uniform-ad-power.csv", header = TRUE)
adpower <- adpower[,3]
cvmpower <- read.table("Table2/t2-uniform-cvm-power.csv", header = TRUE)
cvmpower <- cvmpower[,3]
unifdata <- data.frame(sim=paste("UNIF(0,1)"), SW = swpower, LL = llpower, AD = adpower, CVM = cvmpower)
write.table(unifdata, file = "Table2/t2-uniform.csv", row.names=FALSE, col.names=TRUE)

#jeremiah's power computation as a function
library(nortest)
library(lawstat)
library(fBasics)
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

#generalized lambda
cvalues <- read.table("critical_values.csv",header=TRUE)
ssizes <- cvalues[,1]; SWcrit <- cvalues[,2]; LLcrit <- cvalues[,3]; KScrit <- cvalues[,4]; ADcrit <- cvalues[,5]; JBcrit <- cvalues[,6]; CVMcrit <- cvalues[,7]
lam1 <- 0; lam2 <- 1; lam3 <- 1.25; lam4 <- 1.25
A <- ((1/(1+lam3)) - (1/(1+lam4)))
B <- ((1/(1+2*lam3)) - (1/(1+2*lam4))) - 2*beta(1 + lam3, 1 + lam4)
gldmean <- la m1 + (A/lam2)
gldvar <- lam1 + ((B - A^2)/(lam2^2))

getpower(gldmean, gldvar, "rGLD",lam1, lam2, lam3, lam4)

#experimenting with parameters for nested sapply
distributions <- c("runif(n, 0, 1)", "rexp(n)")
distributions
run <- function(whichdist,p1=NULL,p2=NULL,p3=NULL,p4=NULL){
  n <- 20
  if(is.null(p1)==FALSE){
    if(is.null(p2)==FALSE){
      if(is.null(p3)==FALSE){
        if(is.null(p4)==FALSE){
          x <- as.vector(sapply(n, noquote(whichdist), p1, p2, p3, p4))
        } else{x <- as.vector(sapply(n, noquote(whichdist), p1, p2, p3))}
      } else{x <- as.vector(sapply(n, noquote(whichdist), p1, p2))}
    } else{x <- as.vector(sapply(n, noquote(whichdist), p1))}
  } else{x <- as.vector(sapply(n, noquote(whichdist)))}
  #x <- as.vector(sapply(n, noquote(whichdist)))
  x
}
run("rGLD",0,1,0.75,0.75)
run("runif",0,0.5)

#truncated normal
cvalues <- read.table("critical_values.csv",header=TRUE)
ssizes <- cvalues[,1]; SWcrit <- cvalues[,2]; LLcrit <- cvalues[,3]; KScrit <- cvalues[,4]; ADcrit <- cvalues[,5]; JBcrit <- cvalues[,6]; CVMcrit <- cvalues[,7]
library(truncnorm)
rtruncnorm(10,-2,2,0,sqrt(2.36))
getpower(0,2.36, "rtruncnorm",-2,2,0,sqrt(2.36))
#not quite correct

#t distribution - t(15)
cvalues <- read.table("critical_values.csv",header=TRUE)
ssizes <- cvalues[,1]; SWcrit <- cvalues[,2]; LLcrit <- cvalues[,3]; KScrit <- cvalues[,4]; ADcrit <- cvalues[,5]; JBcrit <- cvalues[,6]; CVMcrit <- cvalues[,7]
tmean <- 0
df <- 15
tvar <- df / (df-2)
getpower(tmean, tvar, "rt", df)

#logistic  (assuming standard)
cvalues <- read.table("critical_values.csv",header=TRUE)
ssizes <- cvalues[,1]; SWcrit <- cvalues[,2]; LLcrit <- cvalues[,3]; KScrit <- cvalues[,4]; ADcrit <- cvalues[,5]; JBcrit <- cvalues[,6]; CVMcrit <- cvalues[,7]
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
