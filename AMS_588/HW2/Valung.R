library(survival)
valung <- read.csv("C:/Users/Linna_hu/Desktop/Valung.csv", header = T)
fit <- coxph(Surv(Time, Status)~., data=valung)
fit