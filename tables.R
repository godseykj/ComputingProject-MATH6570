# Table 2
#Uniform
filename <- "table2-uniform.csv"
sim <- paste("UNIF(0,1)")
<<<<<<< HEAD
uniformsw <- function(n, alpha){
  k <- numeric()
  r <- numeric()
  for (j in 1:10){
    j <- j*1000
    for (i in (j-999):j){
      x <- runif(n, 0, 1)
      SW <- shapiro.test(x)
      ifelse(SW$p.value <= alpha, k[i] <- 1, k[i]<-0)
    }
    dat <- data.frame(sim=sim, n=n, r=j - (j-999) + 1, k=sum(k[(j-999):j]))
    if(file.exists(filename)){
      write.table(dat, file=filename, append=TRUE, row.names = FALSE, col.names=FALSE)
    } else{
      write.table(dat, file=filename, append=FALSE, row.names=FALSE, col.names=TRUE)
    }
  }
}
uniformsw(15, 0.05)
ssizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
for (i in 3:15){
  uniformsw(ssizes[i],0.05)
}
read.table(filename, header=TRUE)
=======
alpha <- 0.05
k <- numeric()
for (i in 1:R){
  x <- runif(n, 0, 1)
  SW <- shapiro.test(data)
  ifelse(SW$p.value <= alpha, k[i] <- 1, k[i]=0)
}
>>>>>>> 2397263a21c9b6ad8eb73e1a41794e8b6af29d4e
