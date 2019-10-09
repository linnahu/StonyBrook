postscript(file = "fig4.2.ps", horizontal=F, height = 6, width = 8.5, pointsize = 14)
#par(mfrow=C(1,2))

example<- read.csv("C:/Users/Linna_hu/Desktop/588/HW1/myel.csv")
fit <- survfit(Surv(dur,status)~trt, data=example)
plot(fit, xlab="Patient time(months)", ylab = "survival probability", lty=c(1,2))
legend(1000,1, c("trt=1", "trt=2"), lty=c(1,2), cex=0.8)
dev.off()

survdiff(Surv(dur, status)~trt, data= example)
survdiff(Surv(dur, status)~trt, rho=1, data=example)
