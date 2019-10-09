#HW5_solution
#1.
k <- 0
z <- rep(NA,10000)
while (k < 5000) {
  u1 <- runif(1)
  u2 <- runif(1)
  v1 <- 2 * u1 - 1
  v2 <- 2 * u2 - 1
  s <- v1 ^ 2 + v2 ^ 2
  if (s <= 1){
    k <- k + 1
    z[2 * k - 1] <- sqrt((-2 * log(s)) / s) * v1
    z[2 * k] <- sqrt((-2 * log(s)) / s) * v2
  }
}
##checking with rnorm(10000)
z1 <- rnorm(10000)
qqplot(z,z1)
abline(0,1,col = "red")

#2.
u1 <- runif(1500)
u2 <- runif(1500)
z1 <- sqrt(-2 * log(u1)) * cos(2 * pi * u2)
z2 <- sqrt(-2 * log(u1)) * sin(2 * pi * u2)
m1 <- matrix(z1[1:300],ncol = 3)
m2 <- matrix(z1[301:800],ncol = 5)
m3 <- matrix(z1[801:1500],ncol = 7)
w1 <- rowSums(m1 ^ 2)
w2 <- rowSums(m2 ^ 2)
w3 <- rowSums(m3 ^ 2)
t1 <- z2[1:100] / sqrt(w1 / 3)
t2 <- z2[101:200] / sqrt(w2 / 5)
t3 <- z2[201:300] / sqrt(w3 / 7)
s1 <- sample(c(1:3),100, prob = c(0.3,0.35,0.35), replace = TRUE)
X <- rep(NA,100)
X[s1 == 1] <- t1[1:sum(s1 == 1)]
X[s1 == 2] <- t2[1:sum(s1 == 2)]
X[s1 == 3] <- t3[1:sum(s1 == 3)]
##checking
s2 <- sample(1:3,100,prob = c(0.3,0.35,0.35),rep = T)
Y <- rep(NA,100)
Y[s2 == 1] <- rt(length(which(s2 == 1)), df = 3)
Y[s2 == 2] <- rt(length(which(s2 == 2)), df = 5)
Y[s2 == 3] <- rt(length(which(s2 == 3)), df = 7)
qqplot(X,Y)
abline(0,1,col = "red")

#3.
rmultivarNorm <- function(n,mu,Sigma){
  k <- 0
  d <- length(mu)
  z <- rep(NA, n * d)
  while(k < n * d / 2){
    u1 <- runif(1)
    u2 <- runif(1)
    v1 <- 2 * u1 - 1
    v2 <- 2 * u2 - 1
    s <- v1 ^ 2 + v2 ^ 2
    if (s <= 1){
      k <- k + 1
      z[2 * k - 1] <- sqrt((-2 * log(s)) / s) * v1
      z[2 * k] <- sqrt((-2 * log(s)) / s) * v2
    } 
  }
  Z <- matrix(z,ncol = d)
  evD <- eigen(Sigma,symmetric=TRUE)
  Lamda.mat <- diag(evD$values)
  P <- evD$vectors
  Q <- P%*%sqrt(Lamda.mat)%*%t(P)
  JMU <- matrix(rep(mu,n), ncol = n)
  result <- Z %*% Q + t(JMU)
  return(result)
}
##checking
Sigma <- matrix(c(2,1,1,1,2,1,1,1,2),nrow=3)
mu <- c(0,0,0)
n <- 10000
R <- rmultivarNorm(n,mu,Sigma)
apply(R,2,mean)
cov(R)

#4.
my.ls <- function(y,x1,x2){
  X <- cbind(x1,model.matrix(y~x2))
  beta.est <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta.est)
}
##checking
data(ChickWeight)
fit <- lm(ChickWeight$weight~ChickWeight$Time+ChickWeight$Diet)
fit$coef
my.ls(ChickWeight$weight,ChickWeight$Time,ChickWeight$Diet)

#5.
Estep <- function(p,x){
  taui <- x[1] * p / (2 + p)
  loglik <- (taui + x[3]) * log(p) + x[2] * log(1 - p)
  return(list(taui=taui,loglik=loglik))
}
Mstep <- function(taui,x){
  p <- (taui + x[3]) / (taui + x[2] + x[3])
  return(p)
}
##initialize with arbitrary p
cur.Estep<-Estep(init.p,x)
cur.Mstep<-Mstep(cur.Estep$taui,x)
loglik<-c(0,cur.Estep$loglik)

i <- 1
epsilon <- 1e-8

while(abs(loglik[i+1]-loglik[i]) > epsilon){
  cur.Estep <- Estep(cur.Mstep,x)
  cur.Mstep <- Mstep(cur.Estep$taui,x)
  loglik <- c(loglik,cur.Estep$loglik)
  i <- i+1
}

x<-c(6544,2008,1448)
init.p <- 0.2
plot(loglik[-1])
loglik
cur.Mstep
#0.5948475