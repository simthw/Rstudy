## r in action, chapter1

## 1.1 A sample of R session
age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)
sd(weight)
cor(age,weight)
plot(age,weight)

## 1.7 working through an example
help.start()
install.packages("vcd")
library("vcd")
help(package = "vcd")
help("Arthritis")
Arthritis 
example(Arthritis)