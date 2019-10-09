library(MASS)
E <- read.csv("C:/Users/Linna_hu/Desktop/AMS578_Group15/IDEgroup15.csv")
Y <- read.csv("C:/Users/Linna_hu/Desktop/AMS578_Group15/IDYgroup15.csv")
G <- read.csv("C:/Users/Linna_hu/Desktop/AMS578_Group15/IDGgroup15.csv")
merge1 <- merge(Y[,2:3],E[,2:8],by="ID")
ob <- merge(merge1, G[,2:27], by="ID")


ob1 <- ob[complete.cases(ob), ]     #remove NA
ob1 <- ob1[,2:33]

## Varaible selection
#for continuous IV, use correlation to check the linear relationship.
### choose the abs correlation bigger than 0.05
cor(ob1[,1:7])   #E1,E3,E4,E5

### use t-test for binary IVs,  Y vs.Gj: G6,G20,G22,
g <- NA
for (i in 1:25) {
  Y0 <- ob1$Y[ob1[,i+7]==0]
  Y1 <- ob1$Y[ob1[,i+7]==1]
  T <- t.test(Y0,Y1)
  g[i] <- T$p.value 
}
g<0.05



##
library(car)

fit0 <- lm(Y ~ 1, data = ob1)
fit1 <- formula(lm(Y~(.)^2, data=ob1))
fit2 <- step(fit0, direction = "forward", scope=fit1)
summary(fit2)               
vif(fit2)

# E3 G20 G6 G3 E1 G23 G25
fit3 <- formula(lm(Y ~ (E3 + G20 + G6 + G3 + E1 + G23 + G25)^3, data = ob1))
fits <- stepAIC(fit0, direction = "forward", scope=fit3)
summary(fits)


fit4 <- formula(lm(Y ~ (E1 + E3 + G3 + G6 + G20)^3, data = ob1))
fitss <- stepAIC(fit0, direction = "forward", scope=fit3)
summary(fitss)



fit5 <- lm(Y ~ E3 + G6:G3:E1, data = ob1)
summary(fit5)

Anova(fit5)
#four-way interactions which is not good
fit6 <- formula(lm(Y~(E1 + E3 + G3 + G6 + G20)^4, data = ob1))
fit7 <- step(fit0, direction = "forward", scope = fit6)
summary(fit7)
fit8 <- lm(Y~E3+G20:G6:G3:E1, data = ob1)
summary(fit8)

BIC(fit8)
BIC(fit5)
##

#Residuals
par(mfrow=c(2,2))
plot(fit5)



shapiro.test(y)       #p-value= 0.02031<0.05
y=ob1[,1]
qqnorm(y)
y=ob1[,1] + 100000000000
summary(powerTransform(y))   # Est Power is 1.0709

 
 
cor(ob1)    # E3, E4, G3, G20, G22,
 
 
 