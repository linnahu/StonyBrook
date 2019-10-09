library(KMsurv)

tis<- 0:10
ninit<- 146
nlost<- c(3,10,10,3,3,11,5,8,1,6)
nevent<- c(27,18,21,9,1,2,3,1,2,2)
lifetab(tis, ninit, nlost, nevent)

library(survival)
surv_object <- with(Siao_Sun_HW0, Surv(time, n.event==1))

fit <- survfit(Surv(time , n.event==1)~1 , data = Siao_Sun_HW0)
summary(fit)
