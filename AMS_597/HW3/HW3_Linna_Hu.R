### Question 1
logret <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header = T)
### (a)
attach(logret)
fit1 <- lm(Pfizer~Exxon + Citigroup)
summary(fit1)
fit1$coefficients
# the coefficients of Exxon is 0.287635991,
# the coefficients of Citigroup is 0.185976651

###(b)
pp<- predict(fit1, interval = "confidence")
plot(Exxon, Pfizer)
matlines(sort(Exxon), pp[order(Exxon),], lty = c(1,2,2), col= c("Black", "red","red"))

plot(Citigroup, Pfizer)
pp <- predict(fit1, int = "c")
matlines(sort(Citigroup), pp[order(Citigroup),], lty = c(1,2,2), col= c("Black", "red","red"))

###(c)
anova(fit1)
# the p-value is less than 0.05, so we can reject the null hypothesis and
# conclude that the regression effect is significant.

###(d)
fit2 <- lm(Pfizer~ Exxon + Citigroup -1)
fit2$coefficients
# Exxon Citigroup 
#0.2509692 0.1881182 

###(e)
cor(Pfizer, Exxon)    #  0.3520965
cor.test(Pfizer, Exxon)
# P-value is 0.004328 which is smaller than 0.05
# so we can reject the null hypothesis, the correlation is not equal to 0.


##### Question2
####(a)
logret <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header = T)
attach(logret)
group1 <- c(Citigroup, AmerExp)
group2 <- c(Exxon, GenMotor)
group3 <- c(Intel)
y1 <- c(group1, group2)
group1 <- c(rep(1,length(group1)), rep(2,length(group2)))
ydata1 <- data.frame(y=y, group = factor(group))
fit3 <- lm(y~group, data=ydata)
anova(fit3)
#p-value = 0.7681 is greater than 0.05
# we cannnot conclude that the mean of group 1 and 2 are different.

####(b)
y2 <- c(group1, group2, group3)
group2 <- c(rep(1,length(group1)), rep(2, length(group2)), rep(3, length(group3)))
ydata2 <- data.frame(y=y, group= factor(group))
fit4 <- lm(y~group, data=ydata)
anova(fit4)
#p-value = 0.5642 is greater than 0.05
# we cannnot conclude that the mean of group 1 and 2 are different.
detach(logret)


#### Question3
####(a)
data("ChickWeight")
attach(ChickWeight)
summary(ChickWeight)
anova(lm(weight ~ Time + Diet))

####(b)
subset <- ChickWeight[Time==2, ]
anova(lm(weight ~ Diet, data = subset))
x <-anova(lm(weight ~ Diet, data = subset))
TukeyHSD(x)
detach(ChickWeight)


####Question 4
logret <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/d_logret_6stocks.txt", header = T)
attach(logret)
### (a)
prop.test(length(Pfizer[Pfizer>0]), length(Pfizer), 0.55)
#  p-value = 0.05303 which is greater than 0.05,
#  so we cannot reject the null hypothesis that the proportion is 0.55

###(b)
prop.test(length(Pfizer[Intel>0]), length(Intel),0.55,  alternative = "greater")
#p-value = 0.8812> 0.05, we cannot reject the null hypothesis that the proportion is not larger than 0.55.

### (c)
prop.test(c(length(Pfizer[Pfizer>0]), length(Intel[Intel>0])), c(length(Pfizer), length(Intel)))
# p-value = 0.7221 > 0.05, cannot reject the null hypothesis.
# conclude that the proportions of Pfizer and Intel are same.

###(d)
group1 <- c(Citigroup, AmerExp)
group2 <- c(Exxon, GenMotor)
group3 <- c(Intel)
n1<-sum(group1<(-0.1))
n2<-sum(group1>=-0.1 & group1<0)
n3<-sum(group1>=0 & group1<0.1)
n4<-sum(group1>=0.1)
n5<-sum(group2<(-0.1))
n6<-sum(group2>=-0.1 & group2<0)
n7<-sum(group2>=0 & group2<0.1)
n8<-sum(group2>=0.1)
n9<-sum(group3<(-0.1))
n10<-sum(group3>=-0.1 &group3<0)
n11<-sum(group3>=0 & group3<0.1)
n12<-sum(group3>=0.1)
effects <- matrix(c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12), nrow=3, byrow = T)
colnames(effects) <- c("r<-0.1", "-0.1<=r<0", "0<=r<0.1", "r>=0.1")
rownames(effects) <- c("group1", "group2", "group3")
effects
chisq.test(effects)
#  p-value = 0.0002055, reject the null hypothesis,
# conclude that the group and return range effects are independent.


#### Question 5
library(MASS)
attach(mcycle)
fit1 <- lm(accel ~ times)
summary(fit1)  #Adjusted R-squared:  0.08089 
fit2 <- lm(accel~times + I(times^2))
summary(fit2) #Adjusted R-squared:  0.1306 

fit3 <- lm(accel~poly(times,degree=3,raw=TRUE))
summary(fit3)  #Adjusted R-squared:  0.3147 
fit4 <- lm(accel~ times + I(times^3))
summary(fit4)  #Adjusted R-squared:  0.09882 
anova(fit1,fit3) # significant
anova(fit2,fit3) # significant
fit5 <- lm(accel~poly(times,degree=4,raw=TRUE))
summary(fit5)  #Adjusted R-squared:  0.3095 
anova(fit3,fit5)  # not significant
 # so we can see that fit3 is the best polynomial regression model.
detach(mcycle)

#### Question 6
data <- read.table("http://www.ams.sunysb.edu/~pfkuan/Teaching/AMS597/Data/HW3Qn6Data.txt",header = T)
install.packages("leaps")
install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(caret)
library(leaps)
models <- regsubsets(y~., data = data, nvmax = 6)
summary(models)
res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)## Adj.R2 CP BIC
##      3  2   1
#Based on BIC, I choose y~x3.

### Another way
library(MASS)
fit <- lm(y~., data = data)
stepfit <- stepAIC(fit, direction = "forward", trace = FALSE)
summary(stepfit)
fit2 <- lm(y~x3, data = data)
summary(fit2)
# Based on stepwise regression, I still choose y~x3
