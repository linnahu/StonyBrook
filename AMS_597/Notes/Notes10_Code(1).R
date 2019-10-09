# Generalized Inverse
# Least Squares Estimation
### g-inverse ###
X <- matrix(c(2,4,1,2),nrow=2)
library(MASS)
ginv(X)
X%*%ginv(X)%*%X
ginv(X)%*%X%*%ginv(X)
t(ginv(X)%*%X)
ginv(X)%*%X


#not include in final
### THIS IS AN EXAMPLE FROM
### https://datascienceplus.com/cubic-and-smoothing-splines-in-r/

require(splines)
require(ISLR)
attach(Wage) 
agelims <- range(age)
age.grid <-seq(from=agelims[1], to = agelims[2])

# quadratic regression
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
fit.quad <- lm(wage~age+I(age^2))
yhat <- predict(fit.quad,newdata=data.frame(age=age.grid))
points(age.grid,yhat,type='l',col='red',lwd=2)

# Cubic Spline with 3 Knots (cutpoints) 
#3 cutpoints at ages 25 ,50 ,60
fit <- lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage )
summary(fit)

#Plotting the Regression Line to the scatterplot   
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

#fitting smoothing splines using smooth.spline(X,Y,df=...)
fit1 <- smooth.spline(age,wage,df=16) #16 degrees of freedom
fit1a <- smooth.spline(age,wage,df=5) #16 degrees of freedom
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
lines(fit1,col="red",lwd=2)
lines(fit1a,col="blue",lwd=2)

#Plotting both cubic and Smoothing Splines 
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df","Cubic Spline"),col=c("red","darkgreen"),lwd=2)

# Implementing regular Cross Validation (cv=TRUE) or GCV (cv=FALSE) to select value of smoothing parameters and Implement Smoothing Splines:
fit2 <- smooth.spline(age,wage)
fit2

#It selects $\lambda=0.0348627$ and df = 6.468299 
plot(age,wage,col="grey")
#Plotting Regression Line
lines(fit2,lwd=2,col="purple")
legend("topright",("Smoothing Splines with 6.47 df selected by GCV"),col="purple",lwd=2)

### test training scheme
set.seed(123)
trainID <- sample(1:dim(Wage)[1],round(dim(Wage)[1]*0.7))
Wage.train <- Wage[trainID,]
Wage.test <- Wage[-trainID,]

fit1T <- lm(wage~age+I(age^2),data=Wage.train)
fit2T <- lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage.train )
fit3T <- smooth.spline(Wage.train$age,Wage.train$wage)

yhat1T <- predict(fit1T,newdata=data.frame(age=Wage.test$age))
mse1T <- mean((Wage.test$wage-yhat1T)^2)
yhat2T <- predict(fit2T,newdata=data.frame(age=Wage.test$age))
mse2T <- mean((Wage.test$wage-yhat2T)^2)
