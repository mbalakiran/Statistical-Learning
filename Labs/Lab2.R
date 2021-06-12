install.packages("class")
library(ISLR)
library(caret)
library(ggplot2)
library(MASS)
library(class)
#10.1 graphical summary 
?Weekly
set<-Weekly
(set)
names(set)
plot(set$Lag1)
hist(set$Lag1)
hist(set$Lag2)
hist(set$Lag3)
hist(set$Lag4)
hist(set$Lag5)
hist(set$Volume)
hist(set$Today)
summary(set)
ggplot(set, aes(Lag1)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Lag2)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Lag3)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Lag4)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Lag5)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Volume)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Today)) +
  geom_histogram(bins = 45)
ggplot(set, aes(Lag1, Today)) + geom_point()

ggplot(set, aes(Lag1, Lag2)) + geom_point() +
  facet_wrap(~ Direction)
ggplot(set, aes(Lag2, Lag3)) + geom_point() +
  facet_wrap(~ Direction)
ggplot(set, aes(Lag3, Lag4)) + geom_point() +
  facet_wrap(~ Direction)
ggplot(set, aes(Lag5, Volume)) + geom_point() +
  facet_wrap(~ Direction)
ggplot(set, aes(Volume, Today)) + geom_point() +
  facet_wrap(~ Direction)
plot(set)
#hist(set)
names(set)
com <- glm(Direction ~Lag1+Lag2 + Lag3+ Lag4 +Lag5+  Volume,family=binomial(link='logit'),data=set)
summary(com)
coef(com)
#summary(com)$coef[,4]
#com1<-predict(com)
#com1
predict.glm(com)
pred <- predict(com, type = "response")
pred

levels(set$Direction) <- c(FALSE,TRUE)
# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(pred>0.5), reference = set$Direction)

a<-as.factor(set$Direction)
a
b<-set$Direction
b
c<-set$Direction
c

##(d) Now fit the logistic regression model using a training data period
#from 1990 to 2008, with Lag2 as the only predictor. Compute the
#confusion matrix and the overall fraction of correct predictions
#for the held out data (that is, the data from 2009 and 2010).

set
trainingdata <- subset(set, Year =!2009&&2010)
trainingdata
testingdata <- set[ which(set$Year>=2009), ]
testingdata
dpred<- glm(Direction ~ Lag2,family=binomial(link='logit'),data=trainingdata)
summary(dpred)

predd<-predict(dpred)
predict.glm(com)
pred1 <- predict(dpred, testingdata, type = "response")
final<-testingdata$Direction
levels(final) <- c(FALSE,TRUE)


# use caret and compute a confusion matrix
confusionMatrix(data = as.factor(pred1>0.5), reference = final)


dpred<- glm(Direction ~ Lag2,family=binomial(link='logit'),data=trainingdata)
prob = predict(dpred, testingdata, type="response")
cpred = rep("Down", dim(testingdata)[1])
cpred[prob > .5] = "Up"
table(cpred, testingdata$Direction)
#mean(cpred == testingdata$Direction)

#lab3

lda.fit = lda(Direction~Lag2, data= trainingdata)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, newdata=testingdata, type="response")
lda.class = lda.pred$class
table(lda.class, testingdata$Direction)
mean(lda.class == testingdata$Direction)

# (f) Repeat (d) using QDA.

qda.fit = qda(Direction~Lag2, data= trainingdata)
qda.fit

qda.pred = predict(qda.fit, newdata=testingdata, type="response")
qda.class = qda.pred$class
table(qda.class, testingdata$Direction)
mean(qda.class==testingdata$Direction)

training.data = Weekly[Weekly$Year<2009,]
test.data = Weekly[Weekly$Year>2008,]


# (g) Repeat (d) using KNN with K = 1.
set.seed(1)
train.X = cbind(training.data$Lag2)
test.X = cbind(test.data$Lag2)
train.Y = cbind(training.data$Direction)
knn.pred = knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.data$Direction)
knn.pred3 = knn(train.X, test.X, train.Y, k=3)
table(knn.pred3, test.data$Direction)
#table(knn.pred ,test.X)
#mean(knn.pred3==testingdata$Direction)
