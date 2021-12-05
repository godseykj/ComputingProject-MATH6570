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
  x <- rnorm(50,0,1)
  KS[i] <- ks.test(x,"pnorm")$statistic
}
KS <- sort(KS); KScrit <- quantile(KS, 1-0.05)
k2 <- numeric()
for (j in 1:10){
  j <- j*1000
  for (i in (j-999):j){
    x <- runif(50, 0, 1)
    x <- (x-0.5)/sqrt(1/12)
    stat2 <- ks.test(x,"pnorm")$statistic
    #stat2 <- ks.test(x, y)$statistic
    ifelse(stat2 >= KScrit, k2[i] <- 1, k2[i]<-0) 
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
