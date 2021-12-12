library(ggplot2)

#funtion to produce graph when power file is inputted and title (distribution) is specified (both arguments in quotations)
graph <- function(filename, distribution){
  table <- read.table(filename)
  data <- data.frame(table)
  ss <- data[,1]
  power <- seq(0,1,10)
  
  ggplot(data, aes(x=ss, y=power)) + 
    geom_line(aes(y=powerSW), color="purple") +
    geom_point(aes(y=powerSW), color="purple") +
    geom_line(aes(y=powerKS), color="red") + 
    geom_point(aes(y=powerKS), color="red") +
    geom_line(aes(y=powerLL), color="blue") +
    geom_point(aes(y=powerLL), color="blue") +
    geom_line(aes(y=powerAD), color="green") +
    geom_point(aes(y=powerAD), color="green") +
    geom_line(aes(y=powerJB), color="orange") +
    geom_point(aes(y=powerJB), color="orange") +
    geom_line(aes(y=powerCVM), color="dark blue") +
    geom_point(aes(y=powerCVM), color="dark blue") +
    labs(x="sample size", y="power",title = distribution) +
    theme(plot.title = element_text(hjust = 0.5))
}

#using the function
graph("Figure1/gld-a.csv", "GLD(0,1,0.75,0.75)")
graph("Figure1/gld-b.csv", "GLD(0,1,0.5,0.5)")
graph("Figure1/gld-c.csv", "GLD(0,1,0.25,0.25)")
graph("Figure2/gld-a-figure2.csv", "GLD(0,1,-0.1,-0.1)")
graph("Figure2/scnom-b-figure2.csv", "ScConN(0.05,3)")
graph("Figure2/gld-c-figure2.csv", "GLD(0,1,−0.15,−0.15)")
graph("Figure3/chi.csv", "CHI(4df)")
graph("Figure3/beta.csv", "BETA(2,1)")

#figuring things out
a <- read.table("unifKSstandardized.csv")

a <- data.frame(a)
a_ssizes <- a[,1]
a_power <- seq(0, 1, 10)

#this works
ggplot(a, aes(x=a_ssizes, y=a_power)) + 
  geom_line(aes(y=powerSW), color="purple") +
  geom_point(aes(y=powerSW), color="purple") +
  geom_line(aes(y=powerKS), color="red") + 
  geom_point(aes(y=powerKS), color="red") +
  geom_line(aes(y=powerLL), color="blue") +
  geom_point(aes(y=powerLL), color="blue") +
  geom_line(aes(y=powerAD), color="green") +
  geom_point(aes(y=powerAD), color="green") +
  geom_line(aes(y=powerJB), color="orange") +
  geom_point(aes(y=powerJB), color="orange") +
  geom_line(aes(y=powerCVM), color="dark blue") +
  geom_point(aes(y=powerCVM), color="dark blue")

#tweaking
plot(x=a$ssizes, y=a$powerSW, xlab = "Sample Sizes", ylab = "Power")
lines(a$ssizes, y=a$powerSW)

a$ssizes <- factor(a$ssizes, levels=a$ssizes[order(a$ssizes)])

