#### Question 1

gx <- function(theta){
    u <- runif(1)
    v <- runif(1)
    Y <- (1 + log(v)/log(1 - (1-theta)^u))
    return(Y)
}
gx.1 <- gx(0.2)
gx.2 <- gx(0.3)
gx.3 <- gx(0.5)
  

Y <- sample(1:3,100,prob=c(0.2,0.3,0.5),replace=TRUE)
W <- rep(NA,100) 
W[Y==1] <- myt(length(which(Y==1)),3)
W[Y==2] <- myt(length(which(Y==2)),5)
W[Y==3] <- myt(length(which(Y==3)),7)
plot(density(W))



## Question 2
#(a)
set.seed(123)
n <- 1000
x.2a <- runif(n)
omega.hat <- (0.5-0) * mean(1 / (1 + x.2a^2))   #0.3935287
var((1/(1+x.2a^2)) * 0.5)/n   #  6.408145e-06

#(b)
set.seed(12)
x.2b <- rnorm(n,0,1)
omega.asterisk <- mean(x.2b <= 0.5)     #0.688
var(x.2b <= 0.5)/n   # 0.0002148709




## Question 3

# slice data
data(ChickWeight)
x <- ChickWeight$weight[ChickWeight$Diet==1]
y <- ChickWeight$weight[ChickWeight$Diet==4]
xv <- var(D1)
yv <- var(D4)

# permutation test
B <- 10000
v.perm <- rep(NA,B)
z <- c(x,y)
N <- rep(c(1,2),c(length(x),length(y)))
for(i in 1:B){
  shuffle <- sample(N,size=length(N),replace=FALSE)
  v.perm[i] <- var.test(z[shuffle==1], z[shuffle==2])$esti
}

emp.pv=(1+length(which(abs(v.perm)>=abs(v0))))/(B+1)
emp.pv    # 0.9926007

