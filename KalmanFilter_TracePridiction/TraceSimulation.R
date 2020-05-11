library(ggplot2)

a = runif(1, -10, 10)
b = runif(1, -10, 10)
 
y1 = function(x){
  a*x + b
}

y2 = function(x){
  c*(x - h)^2 + k
}

y3 = function(x){
  
}

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
p + stat_function(fun = y2) + xlim(-5, 5)

y(10)

data = data.frame(a, b)
ggplot(data, aes(x = a, y = b)) + geom_line() + geom_point()

n = 1000
x = cumsum(sample(c(-1, 1), n, TRUE))
x
?sample
