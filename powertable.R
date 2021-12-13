library(ggplot2)

#funtion to produce graph when power file is inputted and title (distribution) is specified (both arguments in quotations)
graph <- function(filename, distribution){
  table <- read.table(filename)
  data <- data.frame(table)
  ss <- data[,1]
  
  colors <- c("SW"="purple","KS"="red","LL"="blue","AD"="green",
              "JB"="orange","CVM"="dark blue", "DP"="black", "CSQ"="pink")
  
  ggplot(data, aes(x=seq(1,15), y=seq(0,1,10))) + 
    geom_line(aes(y=powerSW), color="SW") +
    geom_point(aes(y=powerSW), color="SW") +
    geom_line(aes(y=powerKS), color="KS") + 
    geom_point(aes(y=powerKS), color="KS") +
    geom_line(aes(y=powerLL), color="LL") +
    geom_point(aes(y=powerLL), color="LL") +
    geom_line(aes(y=powerAD), color="AD") +
    geom_point(aes(y=powerAD), color="AD") +
    geom_line(aes(y=powerJB), color="JB") +
    geom_point(aes(y=powerJB), color="JB") +
    geom_line(aes(y=powerCVM), color="CVM") +
    geom_point(aes(y=powerCVM), color="CVM") +
    geom_line(aes(y=powerDP), color="DP") +
    geom_point(aes(y=powerDP), color="DP") +
    geom_line(aes(y=powerCSQ), color="CSQ") +
    geom_point(aes(y=powerCSQ), color="CSQ") +
    labs(x="sample size", y="power",title = distribution, color="Legend") +
    scale_x_continuous(breaks=seq(1,15),labels=c("10","15","20","25","30","40","50","100","200","300","400","500","1000","1500","2000")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values=colors)
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

table <- read.table("Figure2/scnom-b-figure2.csv")
data <- data.frame(table)
ss <- data[,1]
power <- seq(0,1,10)
sizes <- c(10, 15, 20, 25, 30, 40, 50, 100, 200, 300, 400, 500, 1000, 1500, 2000)
sizesnames <- as.character(sizes)

distribution <- c("GLD(0,1,0.75,0.75)")
filename <- c("Figure1/gld-a.csv")
table <- read.table(filename)
data <- data.frame(table)
ss <- data[,1]
power <- seq(0,1,10)

ggplot(data, aes(x=seq(1,15), y=power)) + 
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
  geom_line(aes(y=powerDP), color="black") +
  geom_point(aes(y=powerDP), color="black") +
  geom_line(aes(y=powerCSQ), color="pink") +
  geom_point(aes(y=powerCSQ), color="pink") +
  labs(x="sample size", y="power",title = distribution) +
  scale_x_continuous(breaks=seq(1,15),labels=c("10","15","20","25","30","40","50","100","200","300","400","500","1000","1500","2000")) +
  theme(plot.title = element_text(hjust = 0.5))
