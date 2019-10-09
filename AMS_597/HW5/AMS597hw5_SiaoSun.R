###AMS597HW5 Siao Sun 111572643###
#Q1
set.seed(11)
x1<-runif(100000,0,pi/3)
w1<-mean(sin(x1))*pi/3
#w1=0.4994595, the exact answer is 0.5

#Q2
set.seed(12)
n<-100000
#(a)
xa<-runif(n, 0, 0.5)
wa<-0.5*mean(exp(-xa))
var(exp(-xa)*0.5)/n
#wa is 0.3932158, and var is 3.201314e-08
#(b)
xb<-rexp(n,1)
wb<-mean(xb<0.5)
var(xb<0.5)/sum(xb<0.5)
#wb is 0.39281, and var is 6.055761e-06.

#(c)
#The variance of (a) is smaller.

#Q3
set.seed(13)
#(a)
pa1<-rep(NA,1000)
for (i in 1:1000){
  x3<-rnorm(20)
  T3<-t.test(x3)
  pa1[i]<-T3$p.value}
TIE1<-sum(pa1<0.05)/1000     
#TIE1=0.043

pa2<-rep(NA,1000)
for (i in 1:1000){
  x3<-rnorm(20)
  T3<-wilcox.test(x3)
  pa2[i]<-T3$p.value}
TIE2<-sum(pa2<0.05)/1000    
#TIE2=0.042

#(b)
pb1<-rep(NA,1000)
for (i in 1:1000){
  x3<-rnorm(20,0.5,1)
  T3<-t.test(x3)
  pb1[i]<-T3$p.value}
pow1<-sum(pb1<0.05)/1000 
#pow1=0.575
pb2<-rep(NA,1000)
for (i in 1:1000){
  x3<-rnorm(20,0.5,1)
  T3<-wilcox.test(x3)
  pb2[i]<-T3$p.value}
pow2<-sum(pb2<0.05)/1000 
#pow2=0.556

#Q4

set.seed(123)
x <- rnorm(50)
y <- 0.2*x+rnorm(50)
cor.test(x,y,method="spearman")
c0 <- cor.test(x,y,method="spearman")$esti
B <- 19999
cperm <- rep(NA,B)
z <- c(x,y)
grp <- rep(c(1,2),c(length(x),length(y)))
for(i in 1:B){
  shuffle.id <- sample(grp,size=length(grp),replace=FALSE)
  cperm[i] <- cor.test(z[shuffle.id==1],z[shuffle.id==2],method="spearman")$esti
}

emp.pv=(length(which(abs(cperm)>=abs(c0))+1))/(B+1)
emp.pv#0.32335