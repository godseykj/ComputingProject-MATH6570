# Table 2
#Uniform
filename <- "table2-uniform-sw.csv"
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
  counts <- read.table(filename, header=TRUE)
  library(dplyr)
  head(counts)
  power <- counts %>% 
    group_by(sim, n) %>% 
    summarize(p = sum(k)/10000)
  head(power)
  filename <- "uniform_power.csv"
  if(file.exists(filename)){
    write.table(power, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
  } else{
    write.table(power, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
  }
}

#This part works perfect but can I combine this into one function?
uniformcounts <- function(n, alpha){
  k1 <- numeric()
  r <- numeric()
  for (j in 1:10){
    j <- j*1000
    for (i in (j-999):j){
      x <- runif(n, 0, 1)
      SW <- shapiro.test(x)
      ifelse(SW$p.value <= alpha, k1[i] <- 1, k1[i]<-0)
    }
    dat <- data.frame(sim=sim, n=n, r=j - (j-999) + 1, k1=sum(k1[(j-999):j]))
    if(file.exists(filename)){
      write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
    } else{
      write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
    }
  }
}
for (i in 1:15){
  uniformcounts(ssizes[i],0.05)
}
