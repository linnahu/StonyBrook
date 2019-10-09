### inverse tranform method for exp ###
lambda <- 2
n <- 10000
u <- runif(n)
x <- -log(u)/lambda

### compare with rexp()
x1 <- rexp(n,lambda)
qqplot(x,x1)
abline(0,1,col='red')

### example in Page 7
x <- sqrt(2*runif(1000))
hist(x,freq=F)

### example in page 10
phi.inv <- function(u){
    t2 <- -2*log(u)
    t <- sqrt(t2)
    x <- t-(2.30753+0.27061*t)/(1+0.99229*t+0.04481*t2)
    return(x)
}

u <- runif(10000)
x <- phi.inv(u)
hist(x)

### compare with rnorm
x1 <- rnorm(10000)
qqplot(x,x1)
abline(0,1,col='red')

## or simply
qqnorm(x)
abline(0,1,col='red')

### AR method in page 13
n <- 10000
k <- 0 #counter for number of accepted
j <- 0 #counter for number of iterations
x <- rep(NA,n)

while(k<n){
    y <- runif(1) ###g
    j <- j+1
    u <- runif(1)
    if(u<=y){
        k <- k+1
        x[k] <- y
        }
    }

### AR method in page 14 using c=6/4, best bound
n <- 10000
k <- 0 #counter for number of accepted
j <- 0 #counter for number of iterations
x <- rep(NA,n)

while(k<n){
    y <- runif(1) ###g
    j <- j+1
    u <- runif(1)
    if(u<=4*y*(1-y)){
        k <- k+1
        x[k] <- y
        }
}


## compare to rbeta
x1 <- rbeta(10000,2,2)

qqplot(x,x1)
abline(0,1,col='red')

### plotting beta density
y <- seq(0.01,0.99,length=100)
f <- 6*y*(1-y)
plot(y,f,type='l')


# Transformation Methods (Beta Distribution)
### page 17 ####
n <- 10000
lambda <- 1 ##  you can choose other lambda
a <- 2
b <- 2
x <- rgamma(n,shape=a,rate=lambda)
y <- rgamma(n,shape=b,rate=lambda)

z <- x/(x+y)

# (Box- Muller Method)
### Page 19
n <- 5000
u1 <- runif(n)
u2 <- runif(n)
x1 <- sqrt(-2*log(u1))*cos(2*pi*u2)
x2 <- sqrt(-2*log(u1))*sin(2*pi*u2)

x <- c(x1,x2) ### you get 10000 N(0,1)
qqnorm(x)
abline(0,1,col='red')
