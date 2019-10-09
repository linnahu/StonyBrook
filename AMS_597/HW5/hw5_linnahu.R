#### Question1.
# Box-Muller
mysdnorm <-function(n){
  l <- c()
  i <- 1
  while (length(l)<=n-2){
    u1 <- runif(1)
    u2 <- runif(1)
    v1 <- 2*u1-1
    v2 <- 2*u2-1
    s <- v1^2 + v2^2
    if (s <= 1){
      x <- sqrt((-2*log(s))/s)*v1
      y <- sqrt((-2*log(s))/s)*v2
      l[i] <- x
      l[i+1] <- y
      i = i + 2
    }
  }
  return(l)
}

n = 10000
l <- mysdnorm(n)
qqnorm(l)
abline(0,1,col='red')



# Mixture
#### Question2.
myt <- function(n,nfree){
  mynorm <- function(){
    u1 <- runif(1)
    u2 <- runif(1)
    x <- sqrt(-2*log(u1))*cos(2*pi*u2)
    return(x)
  }
  mychisq <- function(nfree){
    sum <- 0
    for (i in 1:nfree){
      x <- mynorm()
      sum <- sum + x^2
    }  
    return(sum)
  }
  l <- c()
  for (i in 1:n){
    norm <- mynorm()
    chisq <- mychisq(nfree)
    l[i] <- norm/sqrt(chisq/nfree)
  }
  return(l)
}

Y <- sample(1:3,100,prob=c(0.3,0.35,0.35),replace=TRUE)
W <- rep(NA,100) 
W[Y==1] <- myt(length(which(Y==1)),3)
W[Y==2] <- myt(length(which(Y==2)),5)
W[Y==3] <- myt(length(which(Y==3)),7)
plot(density(W))


# multivariate Distribution
#### Question3.
rmultivarNorm <- function(n,mu,Sigma){
  d <- length(mu)
  mynorms <- function(n){
    l <- c()
    for (i in 1:n){
      u1 <- runif(1)
      u2 <- runif(1)
      x <- sqrt(-2*log(u1))*cos(2*pi*u2)
      l[i] <- x
    }
    return(l)
  }
  Z <- matrix(mynorms(n*d),nrow=n)
  
  evD <- eigen(Sigma,symmetric=TRUE)
  Lamda.mat <- diag(evD$values)
  P <- evD$vectors
  Q <- P%*%sqrt(Lamda.mat)%*%t(P)
  
  J <- matrix(1,nrow=n,ncol=1)
  X <- Z%*%Q+J%*%mu
  
  return (X)
}
mu <- c(0,0,0)
Sigma <- matrix(c(2,1,1,1,2,1,1,1,2),nrow=3)
x <- rmultivarNorm(100, mu, Sigma) #each row of x is a multinomal variable with dimension 3 and the above mu and sigma;



#categorical #maybe not include final
####Question 4.
data(ChickWeight)

my.ls <- function(y,x1,x2){
   X <- model.matrix(y~ x1 + x2)
   beta.est <- solve(t(X)%*%X)%*%t(X)%*%y
   return(beta.est)
}

my.ls(ChickWeight$weight, ChickWeight$Time, ChickWeight$Diet)
fit <- lm(ChickWeight$weight~ChickWeight$Time+ChickWeight$Diet)
fit$coef



# EM Algorithm (categorical)
####Question 5.
#(p/4)*(x1 + x2 + x3)

Estep <- function(x1,x2,x3,p){
  taui.z2 <- (p/4)*(x1+x2+x3)
  logLik <- (x1 - taui.z2)*log(1/2) + taui.z2*log(p/4) +  x2*log((1-p)/2) + x3*log(1/4)
  return(list(taui.z2 = taui.z2, logLik=logLik))
}

Mstep <- function(taui.z2, x1, x2, x3 ){
  p <- (taui.z2 + x3)
  
  
  
  / (taui.z2 + x2 + x3)
  return(p=p)
}


### initialize values
x1 <- 6544
x2 <- 2008
x3 <- 1448
init.p <- 0.5

cur.Estep <- Estep(x1, x2, x3, init.p)
cur.Mstep <- Mstep(cur.Estep$taui.z2, x1, x2, x3)

loglik <- c(0,cur.Estep$logLik)
i <- 1
epsilon <- 1e-6
while(abs(loglik[i+1]-loglik[i])>epsilon){
  print(i)
  cur.Estep <- Estep(x1, x2, x3, cur.Mstep)
  cur.Mstep <- Mstep(cur.Estep$taui.z2, x1,x2,x3)
  loglik <- c(loglik,cur.Estep$logLik)
  i <- i+1
}
plot(loglik[-1])
