# Permutation test
#### page 8
set.seed(123)
x <- rnorm(20)
y <- rnorm(15,mean=-0.5)
#t.test(x,y,alternative='greater',var.equal=TRUE)

### permutation test ###
B <- 10000
t0 <- function(x,y){
    (mean(x)-mean(y))/sqrt(var(x)/length(x)+var(y)/length(y))
    }
t.obs <- t0(x,y)
z <- c(x,y)
n <- length(x)
m <- length(y)
N <- n+m
t.perm <- rep(NA,B)

for(i in 1:B){
    z.shuffle <- sample(z,N,replace=FALSE)
    x.shuffle <- z.shuffle[1:n]
    y.shuffle <- z.shuffle[-c(1:n)]
    t.perm[i] <- t0(x.shuffle,y.shuffle)
 }
emp.pv <- (1+length(which(t.perm>=t.obs)))/(B+1)

#### page 12 ###
set.seed(123)
x <- rnorm(20)
y <- rnorm(15,mean=-0.5)
##ks.test(x,y,exact=FALSE)

ks0 <- function(x,y){
    z <- sort(c(x,y))
    F <- sapply(z,function(i){length(which(x<=i))/length(x)})
    G <- sapply(z,function(i){length(which(y<=i))/length(y)})
    ### SLOW LOOP METHOD ###
    #F = G <- rep(NA,length(z))
    #for(i in 1:length(z)){
    #    F[i] <- length(which(x<=z[i]))/length(x)
    #    G[i] <- length(which(y<=z[i]))/length(y)
    #}
    D <- max(abs(F-G))
}

ks.obs <- ks0(x,y)
z <- c(x,y)
n <- length(x)
m <- length(y)
N <- n+m
ks.perm <- rep(NA,B)

for(i in 1:B){
    z.shuffle <- sample(z,N,replace=FALSE)
    x.shuffle <- z.shuffle[1:n]
    y.shuffle <- z.shuffle[-c(1:n)]
    ks.perm[i] <- ks0(x.shuffle,y.shuffle)
 }
emp.pv <- (1+length(which(ks.perm>=ks.obs)))/(B+1)
