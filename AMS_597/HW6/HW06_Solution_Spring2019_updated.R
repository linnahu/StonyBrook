#1.
set.seed(123)
n <- 10000
x <- runif(n,0,pi/3)
omega.hat <- pi/3 * mean(sin(x))
omega.hat
true.omega <- 1/2
#omega.hat = 0.49808, close to true.omega

#2.
##a.
set.seed(123)
n <- 10000
x <- runif(n,0,0.5)
omega.hat <- 0.5 * mean(exp(-x))
omega.hat
true.omega <- 1 - exp(-0.5)
#omega.hat = 0.3938947, true value is 0.3934693, close.
var.omega <- var(exp(-x) * 0.5) / n
#var = 3.173705e-07

##b.
##hb is exponential dist
set.seed(123)
n <- 10000
x <- rexp(n) 
g <- function(x){
  exp(-x) * (x < 0.5) * (x > 0)
}
gfdvh <- g(x) / exp(-x)
omega.hat <- mean(gfdvh)
omega.hat
#omega.hat = 0.3915, close to true value
var.omega <- var(gfdvh) / n
#var = 2.387667e-05

##c.
##var(a) < var(b), so method a is better

#3.
##a.
pv <- function(N,n,mu = 0,sd = 1){
  p_t <- rep(NA,N)
  p_wilcox <- rep(NA,N)
  
  for (i in 1:N) {
    x <- rnorm(n,mu,sd)
    p_t[i] <- t.test(x)$p.value
    p_wilcox[i] <- wilcox.test(x,exact = T)$p.value
  }
  
  prob_t <- length(which(p_t <= 0.05)) / length(p_t)
  #prob_t <- sum(p_t <= 0.05) / length(p_t)
  prob_wilcox <- length(which(p_wilcox <= 0.05)) / length(p_wilcox)
  #prob_wilcox <- sum(p_wilcox <= 0.05) / length(p_wilcox)
  return(list(p_t,p_wilcox,prob_t,prob_wilcox))
}

pv(1000,20)
#prob_t = 0.051  prob_wilcox = 0.053
##b.
#using a's function
pv(1000,20,0.5,1)
#power = 1 - beta, power_t = 0.573, power_wilcox = 0.538

#4
#the bootstrap estimate of the standard error
set.seed(123)
x<-rnorm(50)
median(x)

library(boot)
n<-50
B <- 10000
Rb <- rep(NA,B)
for(b in 1:B){
  id <- sample(1:n,n,replace=TRUE)
  bootlaw <- x[id]
  Rb[b] <- median(bootlaw)
}
sd(Rb)
#0.1994129
#or
library(boot)
r.boot.func <- function(x,id){
  median(x[id])
}

a<-boot(data=x,statistic=r.boot.func,R=10000)

#bootstrapCI 
theta.hat <- median(x)
B <- 1000
R <- 200
n <- 50
Rb = se.Rb = tb <- rep(NA,B)

for(b in 1:B){
  id1 <- sample(1:n,n,replace=TRUE)
  bootlaw1 <- x[id1]
  Rb[b] <- median(bootlaw1)
  Rb2 <- rep(NA,R)
  for(r in 1:R){
    id2 <- sample(1:n,n,replace=TRUE)
    bootlaw2 <- bootlaw1[id2]
    Rb2[r] <- median(bootlaw2)
  }
  se.Rb[b] <- sd(Rb2)
  tb[b] <- (Rb[b]-theta.hat)/se.Rb[b]
}

alpha <- 0.05
se.R <- sd(Rb)
se.R
#[1] 0.2052689
Qtb <- quantile(tb,c(alpha/2,1-alpha/2),type=1)
Qtb
names(Qtb) <- rev(names(Qtb))
Qtb
bootstrapCI <- rev(theta.hat-Qtb*se.R)
bootstrapCI
#     2.5%      97.5% 
#-0.5765544  0.3980420 

#5.
set.seed(123)
x <- rnorm(50)
y <- 0.2 * x + rnorm(50)
summary(cor.test(x,y,method = "spearman"))
esti <- cor.test(x,y,method = "spearman")$esti
B <- 19999
cperm <- rep(NA,B)

for(i in 1:B){
  cperm[i] <- cor.test(sample(x,length(x),replace=FALSE),y,method = "spearman")$esti
}
pv=(length(which(abs(cperm)>=abs(esti)))+1)/(B+1)
pv
#pv = 0.33365
cor.test(x,y,method = "spearman")$p.value
#true p.value = 0.337955, close
