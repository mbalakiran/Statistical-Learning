rm(list=ls())
library(ISLR)
data(Weekly)
summary(Weekly)
boxplot(Volume~Direction,data=Weekly)
boxplot(log(Volume)~Direction,data=Weekly)
plot(Weekly$Today[1:52]~seq(1:52),type="l")
plot(Weekly)
cor(Weekly[,-c(1,ncol(Weekly))])
cor(log(Weekly$Volume), Weekly$Today)
g2<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family=binomial,data=Weekly)
summary(g2)
my.pred<-predict(g2,type="response")
Weekly$Pred<-ifelse(my.pred>0.5,"UP","Down")
xtabs(~Direction+Pred,data=Weekly)
prop.table(xtabs(~Direction+Pred,data=Weekly),1)
Train.data<-Weekly[Weekly$Year<2009,]
Test.data<-Weekly[Weekly$Year>=2009,]
g3<-glm(Direction~Lag2,family=binomial,data=Train.data)
summary(g3)
my.pred<-predict(g3,type="response",newdata=Test.data)
Test.data$Pred<-ifelse(my.pred>0.5,"UP","Down")
prop.table(xtabs(~Direction+Pred,data=Test.data),1)

