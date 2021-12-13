library(ggplot2)

#funtion to produce graph when power file is inputted and title (distribution) is specified (both arguments in quotations)
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

#using the function
graph("Figure1/gld-a.csv", "GLD(0,1,0.75,0.75)")
graph("Figure1/gld-b.csv", "GLD(0,1,0.5,0.5)")
graph("Figure1/gld-c.csv", "GLD(0,1,0.25,0.25)")
graph("Figure2/gld-a-figure2.csv", "GLD(0,1,-0.1,-0.1)")
graph("Figure2/scnom-b-figure2.csv", "ScConN(0.05,3)")
graph("Figure2/gld-c-figure2.csv", "GLD(0,1,−0.15,−0.15)")
graph("Figure3/chi.csv", "CHI(4df)")
graph("Figure3/beta.csv", "BETA(2,1)")

