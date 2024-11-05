
sine <- function(x){
  n = 10
  total = 0
  for (i in 0:n){
    total = total + (-1)^i * x^(2*i + 1)/factorial(2*i + 1)
  }
  return(total)
  }
sine(0.5)




