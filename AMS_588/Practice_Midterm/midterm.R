library(survival)
midterm <- read.csv("C:/Users/Linna_hu/Desktop/588/Practice midterm/midterm.csv", header = T)
fit <- coxph(Surv(x, delta)~., data=midterm)
summary(fit)
