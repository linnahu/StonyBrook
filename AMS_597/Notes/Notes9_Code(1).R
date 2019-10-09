### page 3 ###
### here i will cheat and use rnorm to generate standard normal. If you are using uniform, then you can get normal from Box-Muller transformation
n <- 1000
k <- 3
A <- matrix(rnorm(n*k),ncol=k)
y <- apply(A^2,1,sum)

###compare with rchisq
y1 <- rchisq(n,k)
qqplot(y,y1)
abline(0,1,col='blue')

### page 5 ###
Y <- sample(1:3,10000,prob=c(0.2,0.5,0.3),replace=TRUE)
W <- rep(NA,10000) ### this will store the random variable from the mixture distribution
W[Y==1] <- rnorm(length(which(Y==1)),0,1)
W[Y==2] <- rnorm(length(which(Y==2)),-1,1)
W[Y==3] <- rnorm(length(which(Y==3)),2,1)
plot(density(W))

### page 6 ###
n <- 10000
p <- 0.4
r <- 10
lambda1 <- rgamma(n,shape=r,rate=(1-p)/p)
Y <- rpois(n,lambda1)
### Y follows negative binomial ###
m <- p*r/(1-p) 
Y1 <- rnbinom(n,mu=m,size=r) ## check with the build in negative binomial in R, note that the parametrization in rnbinom is slightly different from the formula on page 6
qqplot(Y,Y1)
abline(0,1,col='red')
### alternatively, you can also use rnegbin in library(MASS)
#library(MASS)
#rnegbin()

### page 10 ###
Sigma <- matrix(c(2,1,1,1,2,1,1,1,2),nrow=3)
evD <- eigen(Sigma,symmetric=TRUE)
Lamda.mat <- diag(evD$values)
P <- evD$vectors
Q <- P%*%sqrt(Lamda.mat)%*%t(P)

mu <- c(0,0,0)
n <- 10000
d <- length(mu)
Z <- matrix(rnorm(n*d),nrow=n)
J <- matrix(1,nrow=n,ncol=1)
X <- Z%*%Q+J%*%mu

### checking the estimated mean ###
apply(X,2,mean)
cov(X)

### page 12 ###
Sigma <- matrix(c(2,1,1,1,2,1,1,1,2),nrow=3)
Q <- chol(Sigma)

mu <- c(0,0,0)
n <- 10000
d <- length(mu)
Z <- matrix(rnorm(n*d),nrow=n)
J <- matrix(1,nrow=n,ncol=1)
X <- Z%*%Q+J%*%mu

### checking the estimated mean ###
apply(X,2,mean)
cov(X)

### checking run time to show that cholesky decomposition is more efficient
system.time(chol(Sigma))
system.time(eigen(Sigma,symmetric=TRUE))

