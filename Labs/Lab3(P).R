rm(list=ls())
library(ISLR)
library(MASS)
library(class)
library(car)
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
#### Start Lab 3 Here ##################
boxplot(Lag2~Direction,Train.data)
ld1<-lda(Direction~Lag2,data=Train.data)
predl<-predict(ld1,newdata=Test.data)
Test.data$Pred<-predl$class
prop.table(xtabs(~Direction+Pred,data=Test.data),1)
#prop.table(table(Test.data$Direction,Test.data$Pred),2)
qd1<-qda(Direction~Lag2,data=Train.data)
predq<-predict(qd1,newdata=Test.data)
Test.data$Pred<-predq$class
prop.table(xtabs(~Direction+Pred,data=Test.data),1)


tkn<-matrix(Train.data$Lag2,nrow=nrow(Train.data))
tstkn<-matrix(Test.data$Lag2,nrow=nrow(Test.data))
kn1<-knn(train=tkn,test=tstkn, cl = Train.data$Direction,k=3)
prop.table(xtabs(~Test.data$Direction+kn1),1)

g3<-glm(Direction~Lag1+Lag2+log(Volume),family=binomial,
        data=Train.data)
my.pred<-predict(g3,type="response",newdata=Test.data)
Test.data$Pred<-ifelse(my.pred>0.5,"UP","Down")
prop.table(xtabs(~Direction+Pred,data=Test.data),1)
ld1<-lda(Direction~Lag2+Lag1+log(Volume),data=Train.data)
predl<-predict(ld1,newdata=Test.data)
Test.data$Pred<-predl$class
prop.table(xtabs(~Direction+Pred,data=Test.data),1)
qd1<-qda(Direction~Lag2+Lag1+log(Volume),data=Train.data)
predq<-predict(qd1,newdata=Test.data)
Test.data$Pred<-predq$class
prop.table(xtabs(~Direction+Pred,data=Test.data),1)

tkn<-as.matrix(Train.data[,c(2,3)])
tstkn<-as.matrix(Test.data[,c(2,3)])
tkn<-cbind(tkn,log(Train.data$Volume))
tstkn<-cbind(tstkn,log(Test.data$Volume))
kn1<-knn(train=tkn,test=tstkn, cl = Train.data$Direction,k=3)
prop.table(xtabs(~Test.data$Direction+kn1),1)
