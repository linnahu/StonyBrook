####### 582 project

#####fraction factorial 2-level factors design
library(FrF2)
x <- FrF2(128,10)
View(x)
write.csv(x,  file= "C:/Users/Linna_hu/Desktop/582/design582_group10.csv")



####read data
mydata <- read.csv("C:/Users/Linna_hu/Desktop/Project_StonyBrook/Fall 2018/582/project/total10.csv", header = T)
y <- mydata[,11]
qqnorm(y)       #### y follows normal 
gdata<-mydata[,-c(1,2)]



##find potential significant main effect
MEPlot(lm(y~. , data=gdata))      ## A.B.F.G.I
#find potential significant 2-way interaction
IAPlot(lm(y~.^2,data=gdata))     ## no 2-way significant
#normal plot
DanielPlot(lm(y~.^3,data=gdata),alpha=0.01,main="Normal Plot")      ##I.A.G.B
######## A.G.B.I  is significant from three plot



###model
fit<-lm(y~.^3,data=gdata)
summary(step(fit))    ##A.B.F.G.I. BH.BI.FI.IJ.ACG.AEH.AEJ.AHJ.BHJ.CEG

fit2<-lm(y ~ A + B + F + G + I + B:H + B:I + F:I + I:J + A:C:G + A:E:H + A:E:J + A:H:J + B:H:J + C:E:G, data=gdata)
summary(fit2)   ###A.B.G.I.AEJ.AHJ. BHJ.FI

fit3 <- lm(y~ A+B+G+I +A:E:J + A:H:J + B:H:J+F:I, data = gdata)
summary(fit3)      ##A.B.G.I.I:F

fit4 <- lm(y~A+B+G+I+F:I, data=gdata)
summary(fit4)   ###A.B.G.I
fit5 <- lm(y~ A+B+G+I, data=gdata)
summary(fit5)
anova(fit5)
#model: y=A+B+G+I



##########find out the alias combination
aliases(lm(y~.^5, data=gdata))
#A = B:D:F:J = B:C:D:E:H = B:C:F:G:I                                    
#B = A:C:D:E:H = A:C:F:G:I = A:D:F:J  
#G = C:D:I:J = A:B:C:F:I = D:E:F:H:I      
#I = C:D:G:J = A:B:C:F:G = D:E:F:G:H     

#######residual plot
library(lattice)
data.residual=resid(fit5)	
data.fitted=fitted(fit5)	
plot(data.fitted,data.residual)	
xyplot(data.residual~data.fitted,type=c("smooth","p"),col="black")	

qqnorm(data.residual)
qqline(data.residual,col="red")

##############Cross validation
ludata <- read.csv("C:/Users/Linna_hu/Desktop/Project_StonyBrook/Fall 2018/582/total5 (1).csv", header = T)

library(caret)
train_control_cv=trainControl(method= "repeatedcv", number= 5, repeats=16)
lmfit= train(y~ B:C:E:H:I, data=ludata, trControl= train_control_cv, method= "lm")
lmfit
library(DAAG)
cv1= CVlm(data=ludata, m=5, form.lm= formula(y~B:C:E:H:I))

fit6<- lm(y~ A:C:D:E:H, data=gdata)
summary(fit6)

fit7<- lm(y~ A:C:D:E:H + A:B:C:F:I , data=gdata)
summary(fit7)

fit8<- lm(y~ A:C:D:E:H + A:B:C:F:I + C:D:G:J, data=gdata)
summary(fit8)


fit9<- lm(y~ A:C:D:E:H + A:B:C:F:I + C:D:G:J +A, data=gdata)
summary(fit9)
