# Table 2
#Uniform
filename <- "table2-uniform-sw-ks.csv"
sim <- paste("UNIF(0,1)")
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
uniformcounts <- function(filename, alpha){
  k1 <- numeric()
  k2 <- numeric()
  r <- numeric()
  for (m in 1:length(ssizes)){
    for (j in 1:10){
      j <- j*1000
      for (i in (j-999):j){
        x <- runif(ssizes[m], 0, 1)
        SW <- shapiro.test(x)
        KS <- ks.test(x, pnorm(0,1), alternative="two.sided", exact=TRUE)
        ifelse(SW$p.value <= alpha, k1[i] <- 1, k1[i]<-0)
        ifelse(KS$p.value <= alpha, k2[i] <- 1, k2[i]<-0)
      }
      dat <- data.frame(sim=sim, n=n, r=j - (j-999) + 1, k1=sum(k1[(j-999):j]), k2=sum(k2[(j-999):j]))
      if(file.exists(filename)){
        write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
      } else{
        write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
      }
    }
  }
}
uniformcounts(uc,0.05)
uniformpower <- function(x){
  counts <- read.table(x, header=TRUE)
  library(dplyr)
  head(counts)
  power <- counts %>% 
    group_by(sim, n) %>% 
    summarize(sw = sum(SW)/10000, ks = sum(KS)/10000) %>% 
    filter(n %in% c(10, 20, 30, 50, 100, 300, 500, 1000, 2000))
  head(power)
  filename <- "t2-unif-sw-ks-power.csv"
  if(file.exists(filename)){
    write.table(power, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
  } else{
    write.table(power, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

uniformpower("table2-uniform-sw-ks")

#1 sample size at a time
  #1000 replicates at a time
    #generate sample size n from given distribution
    #calculate all sample statistics
    #write row to csv

#runtime: 1.14 min
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
        SW <- shapiro.test(x)
        KS <- ks.test(x, pnorm(0,1), alternative="greater", exact=TRUE) #not sure about two-sided here
        ifelse(SW$p.value <= alpha, k1[i] <- 1, k1[i]<-0)
        ifelse(KS$p.value <= 1-alpha, k2[i] <- 1, k2[i]<-0) #using 1-alpha because of the blurb about critical values in simulation methodology (??)
        #CVM <- cvm.test(x)
        #ifelse(CVM$p.value >= 1 - alpha, k2[i] <- 1, k2[i] <- 0)
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

start_time <- Sys.time()
uniformcounts("table2-uniform-sw-ks.csv",0.05)
end_time <- Sys.time()
runtime <- end_time - start_time
runtime

#trying to find the right ks test
a<- 7
test <- numeric()
power <- numeric()
for(i in 1:10000) { 
  n <- ssizes[a]
  dist <- runif(n,0,1)
  KS <- ks.test(dist, pnorm(0,1), alternative="greater", exact=FALSE)$statistic
  ifelse(KS >= KScrit[a], test[i] <- 1, test[i] <- 0)
} 
power <- sum(test)/10000
power

library(nortest)
test <- numeric()
power <- numeric()
for(i in 1:10000) { 
  n <- 300
  dist <- runif(n,0,1)
  CVM <- cvm.test(dist)
  ifelse(CVM$p.value <= 0.05, test[i] <- 1, test[i] <- 0)
} 
power <- sum(test)/10000
power

#critical values
alpha <- 0.05
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
#SW <- numeric(); KS <- numeric()
#SWcrit <- numeric(); KScrit <- numeric()
for (a in 1:15){
  #SW <- numeric()
  KS <- numeric()
  for (i in 1:50000){
    x <- rnorm(ssizes[a], 0, 1)
    #SW[i] <- shapiro.test(x)$statistic
    KS[i] <- ks.test(x, pnorm(0,1), "greater")$statistic
  }
  KS <- sort(KS)#SW <- sort(SW); KS <- sort(KS)
  KScrit[a] <- quantile(KS, 1-alpha)#SWcrit[a] <- quantile(SW, alpha); KScrit[a] <- quantile(KS, 1-alpha)
}

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

write.table(SWcrit, row.names = FALSE,col.names=c("SW"),file = "critical_values.csv")

#experimenting with KS
KS <- numeric()
for(i in 1:50000){
  x <- rnorm(50,0,1)
  KS[i] <- ks.test(x,pnorm(0,1),"two.sided")$statistic
}
KS <- sort(KS); KScrit <- quantile(KS, 1-0.05)
k2 <- numeric()
for (j in 1:10){
  j <- j*1000
  for (i in (j-999):j){
    x <- runif(50, 0, 1)
    stat2 <- ks.test(x, pnorm(0.5,1/12), alternative="two.sided", exact=TRUE)$statistic
    ifelse(stat2 >= KScrit, k2[i] <- 1, k2[i]<-0) 
  }
}
power <- sum(k2)/10000
power
