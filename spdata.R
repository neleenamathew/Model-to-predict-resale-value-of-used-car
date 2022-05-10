install.packages("ISLR")
library(ISLR)
install.packages("MLmetrics")
library(MLmetrics)
attach(spdata)

#spliting the data
smp_siz=floor(0.80*nrow(spdata))
set.seed(10000)
train_ind=sample(seq_len(nrow(spdata)),size = smp_siz)
train =spdata[train_ind,]
test=spdata[-train_ind,]

#calculating regression for a multi variable model
reg<- lm(log(Price)~log(duration)+log(kilometer)+fuelType+log(powerPS)+gearbox+`0tRepairedDamage`,data=spdata)
summary(reg)

#scatter plot
plot(reg)
return 
scatter.smooth(x=cardata$price,y=cardata$powerPS, main= "Price ~ Kilometer")

#trying our luck with MAPE
# FOR TEST
lmMod<- lm(log(price)~log(duration)+log(kilometer)+fuelType+log(powerPS)+gearbox+`0tRepairedDamage`,data= test)
summary(lmMod)
pred <- predict(lmMod,test)
View(pred)
actual <-predict(reg,test)
View(actual)

actuals_pred<- data.frame(cbind(actuals=actual,predicts=pred))
View(actuals_pred)
na.omit(actuals_pred)
View(actuals_pred)
mape1<- abs(actuals_pred$predicts-actuals_pred$actuals)
View(mape1)
mape2<- (mape1/actuals_pred$actuals)*100
View(mape2)
mapefinal<-mean(mape2, trim=0,na.rm = TRUE)
View(mapefinal)

#FOR TRAIN
lmMod1<- lm(log(price)~log(duration)+log(kilometer)+fuelType+log(powerPS)+gearbox+`0tRepairedDamage`,data= train)
summary(lmMod1)
pred1 <- predict(lmMod1,train)
View(pred1)
actual1 <-predict(reg,train)
View(actual1)
actuals_pred1<- data.frame(cbind(actuals1=actual1,predicts1=pred1))
View(actuals_pred1)
na.omit(actuals_pred1)
View(actuals_pred1)
mapetrain1<- abs(actuals_pred1$predicts1-actuals_pred1$actuals1)
View(mapetrain1)
mapetrain2<- (mapetrain1/actuals_pred1$actuals1)*100
View(mapetrain2)
mapetrainfinal<-mean(mapetrain2, trim=0,na.rm = TRUE)
View(mapetrainfinal)






