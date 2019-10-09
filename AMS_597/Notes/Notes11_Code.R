# Expectation-maximization (EM) method
# Unknown mixture of normal mixture(notes)

### simulate 1000 X from mixture normal
### 0.25*N(1,0.5^2)+0.75*N(3,0.3^2)
set.seed(123)
n <- 1000
z <- sample(c(0,1),size=n,prob=c(0.25,0.75),replace=TRUE)
x <- rep(NA,n)
x[z==0] <- rnorm(length(which(z==0)),1,0.5)
x[z==1] <- rnorm(length(which(z==1)),3,0.3)

### given x, now fit EM algorithm to estimate the unknown paramaters

Estep <- function(mu0,mu1,sigma0,sigma1,p,x){
    taui <- p*dnorm(x,mu1,sigma1)/((1-p)*dnorm(x,mu0,sigma0)+p*dnorm(x,mu1,sigma1))
    logLik <- sum(log((1-p)*dnorm(x,mu0,sigma0)+p*dnorm(x,mu1,sigma1)))
    return(list(taui=taui,logLik=logLik))
}

Mstep <- function(taui,x){
    mu0 <- sum((1-taui)*x)/sum(1-taui)
    mu1 <- sum(taui*x)/sum(taui)
    sigma02 <- sum((1-taui)*(x-mu0)^2)/sum(1-taui)
    sigma12 <- sum(taui*(x-mu1)^2)/sum(taui)
    p <- sum(taui)/length(x)
    sigma0 <- sqrt(sigma02)
    sigma1 <- sqrt(sigma12)
    return(list(mu0=mu0,mu1=mu1,sigma0=sigma0,sigma1=sigma1,p=p))
    }

### initialize with arbitrary mu0,sigma0,...values
init.mu0 <- 0
init.mu1 <- 1
init.sigma0 = init.sigma1 <- 1
init.p <- 0.5

cur.Estep <- Estep(init.mu0,init.mu1,init.sigma0,init.sigma1,init.p,x)

cur.Mstep <- Mstep(cur.Estep$taui,x)

loglik <- c(0,cur.Estep$logLik)
i <- 1
epsilon <- 1e-6
while(abs(loglik[i+1]-loglik[i])>epsilon){
    cur.Estep <- Estep(cur.Mstep$mu0,cur.Mstep$mu1,cur.Mstep$sigma0,cur.Mstep$sigma1,cur.Mstep$p,x)
    cur.Mstep <- Mstep(cur.Estep$taui,x)
    loglik <- c(loglik,cur.Estep$logLik)
    i <- i+1
}

plot(loglik[-1])
