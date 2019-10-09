# Simple Monte Carlo Integration

### page 3 ###
x <- runif(1000)
omega.hat <- mean(exp(-x)) ### sum(exp(-x))/1000
true.omega <- 1-exp(-1)

### page 5 ###
x <- runif(1000,2,4)
omega.hat <- (4-2)*mean(exp(-x))
true.omega <- exp(-2)-exp(-4)

### page 6 ###
z <- rnorm(10000)
mean(z<= -1.645)
pnorm(-1.645,lower.tail=TRUE) ## true value



#e.g, comparing the variance of omega hat for different sample size
n <- 100
z <- rnorm(n)
var(z<= -1.645)/n
n <- 1000
z <- rnorm(n)
var(z<= -1.645)/n
n <- 10000
z <- rnorm(n)
var(z<= -1.645)/n

### pdf of cauchy vs normal ###
x <- seq(-5,5,length=100)
y1 <- dnorm(x)
y2 <- dcauchy(x)
plot(x,y1,type='l',col='blue')
lines(x,y2,col='red')

### regular monte carlo
n <- 1000
z <- rnorm(n)
omega1 <- mean(z<= -2)
var1 <- var(z<= -2)


#¿¼
## Importance Sampling
x <- rcauchy(n)
imp.g <- function(x){
    (x<= -2)*dnorm(x)/dcauchy(x)
    }
omega2 <- mean(imp.g(x))

var2 <- var(imp.g(x))

