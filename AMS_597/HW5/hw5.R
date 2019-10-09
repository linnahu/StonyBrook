#####Q.1.


n=10000


x=runif(n,0,pi/3)


omega.hat=(pi/3-0)*mean(sin(x))


omega.hat 

#[1] 0.4944274

integrate(sin,0,pi/3)$value

#[1] 0.5

#the estimate 0.4944274 is very close to the actual value of 0.5



#####Q.2.




#part a)

m <- 10000

u <- runif(m, 0, 0.5)

theta <- 0.5 * mean(exp(-u))

theta

#[1] 0.3936808

var(exp(-u)*0.5)/m 

#[1] 3.211422e-07




#part b)

m <- 10000

u <- rexp(m)

theta <- mean(u<=0.5)

theta

#[1] 0.3873

var(u<=0.5)/m 

#[1] 2.373224e-05



#part c)


# Hence the variance from uniform simulation is smaller compares to the variance from exponential simulation. 







#####Q.3.


#part a) 1) 2)


q3tres<-c()


q3wres<-c()


for(i in 1:1000){
  
  
  q3agen<-rnorm(20)
  
  
  q3tres[i]<-t.test(q3agen)$p.value
  
  
  q3wres[i]<-wilcox.test(q3agen,exact=T)$p.value
  
  
}


q3tt1er<-length(which(q3tres<=0.05))/length(q3tres)


q3wt1er<-length(which(q3wres<=0.05))/length(q3wres)


q3tt1er


#[1] 0.047


q3wt1er


#[1] 0.048





#part b) 1) 2)


q3tres<-c()


q3wres<-c()


for(i in 1:1000){
  
  
  q3bgen<-rnorm(20,0.5,1)
  
  
  q3tres[i]<-t.test(q3bgen)$p.value
  
  
  q3wres[i]<-wilcox.test(q3bgen,exact=T)$p.value
  
  
}


q3tp<-length(which(q3tres<=0.05))/length(q3tres)


q3wp<-length(which(q3wres<=0.05))/length(q3wres)


q3tp


#[1] 0.582


q3wp


#[1] 0.564




#####Q.4.

set.seed(123)

x=rnorm(50)

y=0.2*x+rnorm(50)

cor.test(x,y,method="spearman") 

c0=cor.test(x,y,method="spearman")$esti

B=9999

cperm=rep(NA,B)

z=c(x,y)

grp=rep(c(1,2),c(length(x),length(y)))

for(i in 1:B){
  
  shuffle.id=sample(grp,size=length(grp),replace=FALSE)
  
  cperm[i]=cor.test(z[shuffle.id==1],z[shuffle.id==2],method="spearman")$esti
  
}

emp.pv=(length(which(abs(cperm)>=abs(c0)))+1)/(B+1)

emp.pv

#0.3279

#Pvalue from Permutation test is 0.3279 which is very close to the p-value from actual test is 0.337955.

