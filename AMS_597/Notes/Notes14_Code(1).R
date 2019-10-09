### Bootstrap
### standard error
### page 10 ###
library(bootstrap)
str(law)
theta.hat <- cor(law$LSAT,law$GPA)

B <- 1000
n <- dim(law)[1]
Rb <- rep(NA,B)

for(b in 1:B){
    id <- sample(1:n,n,replace=TRUE)
    bootlaw <- law[id,]
    Rb[b] <- cor(bootlaw$LSAT,bootlaw$GPA)
}

se.R <- sd(Rb) ### this is the bootstrap estimate of the standard error or pearson correlation coefficient
se.R
bias.R <- mean(Rb)-theta.hat


library(boot)

r.boot.func <- function(x,id){
    cor(x[id,1],x[id,2])
    }

boot(data=law,statistic=r.boot.func,R=50)



# T-interval
### page 20 ###
theta.hat <- cor(law$LSAT,law$GPA)
B <- 1000
R <- 200
n <- dim(law)[1]
Rb = se.Rb = tb <- rep(NA,B)

for(b in 1:B){
    id1 <- sample(1:n,n,replace=TRUE)
    bootlaw1 <- law[id1,]
    Rb[b] <- cor(bootlaw1$LSAT,bootlaw1$GPA)
    Rb2 <- rep(NA,R)
    for(r in 1:R){
        #id2 <- sample(id1,n,replace=TRUE) ## this will be the bootstrap of bootstrap
        #bootlaw2 <- law[id2,]
        id2 <- sample(1:n,n,replace=TRUE)
        bootlaw2 <- bootlaw1[id2,]
        Rb2[r] <- cor(bootlaw2$LSAT,bootlaw2$GPA)
    }
    se.Rb[b] <- sd(Rb2)
    tb[b] <- (Rb[b]-theta.hat)/se.Rb[b]
}

alpha <- 0.05
se.R <- sd(Rb) 
Qtb <- quantile(tb,c(alpha/2,1-alpha/2),type=1)
names(Qtb) <- rev(names(Qtb))
bootstrapCI <- rev(theta.hat-Qtb*se.R)

### using boot ###
library(boot)
r.boot.func1 <- function(x,id){
    r <- cor(x[id,1],x[id,2])
    }
r.boot.func2 <- function(x,id){
    r <- cor(x[id,1],x[id,2])
    v <- var(boot(data=x[id,],statistic=r.boot.func1,R=200)$t)
    c(r,v)
    }
B <- 1000
my.boot.out <- boot(data=law,statistic=r.boot.func2,R=B)
boot.ci(my.boot.out,type='stud')
