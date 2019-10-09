example <- read.csv("C:/Users/Linna_hu/Desktop/588/HW1/tempsurv.csv", header = T)
#exponential 
library(survival)
fit<- survfit(Surv(survtime,status)~1, conf.type=c("plain"), data=example)
plot(0,0, xlim=c(0,40), ylim=c(0,1), xlab = "Patient time(months)", ylab="survival probability", pch= " ")
lines(fit, lty=1)
x<- seq(0,40, by=0.5)
sx<- exp(-0.0311*x)
lines(x,sx,lty=2)

#Weibull
alpha <- 1/0.46525153
lambda<- exp(-3.36717004/0.46525153)
sx<- exp(-lambda * x^alpha)
#the object "x" was created before
lines(x, sx, lty=4)
legend(25,1, c("KM estimate", "Exponential fit","Weibull fit"), lty = c(1,2,4), cex = 0.8)
