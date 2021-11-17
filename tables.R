# Table 2
#Uniform
filename <- "table2-uniform.csv"
sim <- paste("UNIF(0,1)")
alpha <- 0.05
k <- numeric()
for (i in 1:R){
  x <- runif(n, 0, 1)
  SW <- shapiro.test(data)
  ifelse(SW$p.value <= alpha, k[i] <- 1, k[i]=0)
}