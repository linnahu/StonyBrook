
##  Importance Sampling ( continued)
### visualization of how well each proposal/importance function capture the shape of g(x)
x <- seq(0,1,length=100)
g <- exp(-x)/(1+x^2)
h0 <- 1
h1 <- exp(-x)
h2 <- 1/(pi*(1+x^2))
h3 <- exp(-x)/(1-exp(-1))
h4 <- 4/(pi*(1+x^2))

par(mfrow=c(2,3))
plot(x,g/h0,type='l',main='h0',ylim=c(0,3))
plot(x,g/h1,type='l',main='h1',ylim=c(0,3))
plot(x,g/h2,type='l',main='h2',ylim=c(0,3))
plot(x,g/h3,type='l',main='h3',ylim=c(0,3))
plot(x,g/h4,type='l',main='h4',ylim=c(0,3))

### importance sampling ###
g <- function(x){
    exp(-x)/(1+x^2)*(x>0)*(x<1)
}

n <- 10000
### h0 ###
x <- runif(n)
omega.hat0 <- mean(g(x))
se.omega.hat0 <- sd(g(x))

### h1 ###
x <- rexp(n)
omega.hat1 <- mean(g(x)/dexp(x))
se.omega.hat1 <- sd(g(x)/dexp(x))

### h2 ###
x <- rcauchy(x)
x[x>3] <- 3
x[x< -3] <- -3
omega.hat2 <- mean(g(x)/dcauchy(x))
se.omega.hat2 <- sd(g(x)/dcauchy(x))

### h3 ###
x <- rexp(2*n)
x[x>1] <- NA
x[x<0] <- NA
x <- na.omit(x)[1:n]
h3 <- function(x){
    exp(-x)/(1-exp(-1))
    }
omega.hat3 <- mean(g(x)/h3(x))
se.omega.hat3 <- sd(g(x)/h3(x))

### h4 ###
x <- rcauchy(5*n)
x[x>1] <- NA
x[x<0] <- NA
if(length(na.omit(x))>n){
    x <- na.omit(x)[1:n]
}else{cat('not enough n')}

h4 <- function(x){
    4/(pi*(1+x^2))
    }
omega.hat4 <- mean(g(x)/h4(x))
se.omega.hat4 <- sd(g(x)/h4(x))

c(omega.hat0,omega.hat1,omega.hat2,omega.hat3,omega.hat4)

c(se.omega.hat0,se.omega.hat1,se.omega.hat2,se.omega.hat3,se.omega.hat4)



# Monte Carlo methods in inference
#### page 4 ###
n <- 10000
x1 <- rnorm(n)
x2 <- rnorm(n)
omega.hat <- mean(abs(x1-x2))


## Hypothesis Test
#### determine the minimum required sample size n for a given power and effect size

dat <- read.table('http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/HeightData.txt',header=T)
sd.est <- sd(dat$height)

m <- 10000
mu1 <- 162
n <- c(1:10)*5+15

beta <- rep(NA,length(n))
for(k in 1:length(n)){
    T.rand.pwr = T.rand <- rep(NA,m)
    for(i in 1:m){
        x0 <- rnorm(n[k],160,sd.est)
        T.rand[i] <- (mean(x0)-160)*sqrt(n[k])/sd(x0)
        x1 <- rnorm(n[k],mu1,sd.est)
        T.rand.pwr[i] <- (mean(x1)-160)*sqrt(n[k])/sd(x1)
    }
    CL <- quantile(T.rand,0.05)
    CH <- quantile(T.rand,0.95)
    beta[k] <- length(which(T.rand.pwr>CL&T.rand.pwr<CH))/m
}

plot(n,1-beta,type='b',xlab='sample size (n)',ylab='power',cex.axis=2)
abline(h=0.8,col='red')

