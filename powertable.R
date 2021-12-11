library(ggplot2)

a <- read.table("unifKSstandardized.csv")

a <- data.frame(a)
a_ssizes <- a[,1]
a_power <- seq(0, 1, 10)
names <- as.character(a_ssizes)
a[,1] <- names

#working
ggplot(a, aes(x=a_ssizes, y=a_power)) + 
  geom_line(aes(y=powerSW), color="purple") +
  geom_point(aes(y=powerSW), color="purple") +
  geom_line(aes(y=powerKS), color="red") + 
  geom_point(aes(y=powerKS), color="red") 

#tweaking
ggplot(a, aes(x=names, y=a_power)) + 
  geom_line(aes(y=powerSW), color="purple") +
  geom_point(aes(y=powerSW), color="purple") +
  geom_line(aes(y=powerKS), color="red") + 
  geom_point(aes(y=powerKS), color="red") 
