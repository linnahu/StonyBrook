# Monte Carlo
#### Question 1
x = runif(10000, 0, pi/3)
omega.hat <- (pi/3-0) * mean(sin(x))    #0.49864
true.omega <- integrate(sin, 0, pi/3)   #0.5
# The estimate 0.49864 is very close to the true value 0.5

#### Question 2
###(a)
set.seed(123)
n <- 10000
x.2a <- runif(n, 0, 0.5)
omega.hat.2 <- (0.5-0) * mean(exp(-x.2a))   #0.3938947
var(exp(-x.2a) * 0.5)/n   #  3.207159e-07

###(b)
x.2b <- rexp(n)
omega.asterisk <- mean(x.2b <= 0.5)     #0.3899
var(x.2b <= 0.5)/n   #2.379018e-05

###(c)
# The variance of omega hat is smaller.



# Hypothesis test
#### Question 3
###(a)
ttest <- c()
wilcoxon <- c()

for (i in 1:1000){
  x.3 <- rnorm(20)
  ttest[i] <- t.test(x.3)$p.value
  wilcoxon[i] <-wilcox.test(x.3, exact = T)$p.value 
}
TI.1 <- length(which(ttest < 0.05))/length(ttest)             #0.049
TI.2 <- length(which(wilcoxon < 0.05))/length(wilcoxon)     #0.042

###(b)
ttest.b <-c()
wilcoxon.b <- c()

for (i in 1:1000){
  x.3b <- rnorm(20,0.5,1)
  ttest.b[i] <- t.test(x.3b)$p.value
  wilcoxon.b[i] <- wilcox.test(x.3b, exact = T)$p.value
}
TI.3 <- length(which(ttest.b < 0.05))/length(ttest.b)        #0.579
TI.4 <- length(which(wilcoxon.b < 0.05))/length(wilcoxon.b)  #0.551



# bootstrap
### Question 4
set.seed(123)
x <- rnorm(50)
mx <- median(x)

B <- 1000
R <- 200
n <- length(x)
Rb = se.Rb = tb <- rep(NA,B)

for(b in 1:B){
  id1 <- sample(1:n,n,replace=TRUE)
  Rb[b] <-  median(id1)
  Rb2 <- rep(NA,R)
  for(r in 1:R){
    id2 <- sample(1:n,n,replace=TRUE)
    Rb2[r] <- median(id2)
  }
  se.Rb[b] <- sd(Rb2)
  tb[b] <- (Rb[b]-mx)/se.Rb[b]
}

alpha <- 0.05
se.R <- sd(Rb)    #3.386486
Qtb <- quantile(tb,c(alpha/2,1-alpha/2),type=1)
names(Qtb) <- rev(names(Qtb))
bootstrapCI <- rev(mx-Qtb*se.R)  # [-32.9,-18.3]


#permutation test
#### Question 5
set.seed(123)
x <- rnorm(50)
y <- 0.2*x + rnorm(50)


c0 <- cor.test(x,y,method="spearman")$esti
B <- 10000
c.perm <- rep(NA,B)
z <- c(x,y)
N <- rep(c(1,2),c(length(x),length(y)))
for(i in 1:B){
  shuffle <- sample(N,size=length(N),replace=FALSE)
  c.perm[i] <- cor.test(z[shuffle==1], z[shuffle==2], method="spearman")$esti
}

emp.pv=(1+length(which(abs(c.perm)>=abs(c0))))/(B+1)
emp.pv#0.3278672