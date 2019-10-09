#step 1: merge data, omit NA, transform Y.
Etable <- read.csv("/Users/Catherine/Desktop/IDEgroup3.csv",header = TRUE)
Ytable <- read.csv("/Users/Catherine/Desktop/IDYgroup3.csv",header = TRUE)
Gtable <- read.csv("/Users/Catherine/Desktop/IDGgroup3.csv",header = TRUE)
#----order 3 data table according ID
Etable1 <- Etable[order(Etable$ID),]
Ytable1 <- Ytable[order(Ytable$ID),]
Gtable1 <- Gtable[order(Gtable$ID),]
#----make 3 data tale together, newdata the merged one,but with NA.
mydata1 <- merge(Ytable1[,-1],Etable1[,-1],by="ID")
newdata <- merge(mydata1,Gtable1[,-1],by="ID")
#----delete NAs from newdata, finaldata is the target table.
finaldata <- na.omit(newdata[,-1])
#----check Y's normality, if need, box-cox transformation.
plot(density(finaldata$Y))
p <- powerTransform(finaldata$Y)
Ytrans <- bcPower(finaldata$Y,p$lambda)
plot(density(Ytrans))        ##??? lamda+9.53975, very extreme
finaldata2 <- data.frame(Ytrans,finaldata[,-1])
### finaldata2 is the new dataset with transformed y as DV.
#====================================
# STEP 2ï¼? select varibles
### for continuous IV, use correlation to check the linear relationship.
cor(finaldata2[,c(1:7)])   
### choose the abs correlation bigger than 0.05, which are E1,E3,E4,E5

### use t-test for binary IVs,  Y vs.Gj: only G1 is significant.
g <- NA
for (i in 1:25) {
  y1 <- finaldata2$Ytrans[finaldata[,i+7]==0]
  y2 <- finaldata2$Ytrans[finaldata[,i+7]==1]
  tt <- t.test(y1,y2)
  g[i] <- tt$p.value 
}
g<0.05

# use selected explain varibles to compose new dataset.
select.data <- subset(finaldata2,select = c("Ytrans","E1","E3","E4","E5","G1"))

#=====================================
#STEP 3: regression
#only intercept
null <- lm(Y~1,data = select.data)
summary(null)

# full model
full<- lm(Ytrans~.,data = select.data)
summary(full)       #### Adjusted R-squared 0.178,BIC=51262.99

### second order intereaction model
model1 <- lm(Ytrans~.^2,data = select.data)
summary(model1)    #### adj R2=0.2063, BIC=51283.75
#### select Y~E1:E3+E4:E5
model11 <- lm(Ytrans~ E1:E3+E4:E5,data = select.data)
summary(model11)   #### Adj R2=0.2034, BIC=51211.54, the lowest one.
summary(step(model1))   #### stepwise model1, Ytrans~G1+E1:E3+E1:E4+E4:E5
model2 <- lm(Ytrans~G1+E1:E3+E1:E4+E4:E5,data = select.data)
summary(model2)   #### Adj R2=0.2125
BIC(model2)       #### BIC= 51212.53
### 3rd order intereaction model
model3 <- lm(Ytrans~.^3, data = select.data)
summary(model3)
# three interactions only E4:E5:G1 is moderate significant.
model4 <- step(model3,direction = "both")
summary(model4) 
BIC(model4)     #### BIC=51283.76
# try to add three intereaction term
model5 <- lm(Ytrans~E1:E3+E4:E5,data = select.data)
summary(model5)   ##### Adj R2=0.2034
BIC(model5)     ####BIC=51211.54, Adj R2 is the same with model11, so we choose the simpler one.
#============================================
#step 4: model checking
##### The best model we get is model11: Y~ E1:E3+E4:E5
par(mfrow=c(2,2))
plot(model11)
### the residual looks normal, residual vs. Fitted value looks symmetric.

