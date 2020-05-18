install.packages('glmnet')
library(MASS)
library(glmnet)
setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 1")
data <- read.csv("ANES2016.csv", header = TRUE)
View(data)
is.na(data)
sum(is.na(data))
summary(data)
data$Trump
data$Trump[data$Trump< 0] <- "NA"
data$Trump[data$Trump >0 & data$Trump < 4] <- "Liberal"
data$Trump[data$Trump>=4 & data$Trump <= 7] <- "Conservative"
data$Trump
attach(data)
View(data)

summary(data)
subtru <- subset(data,Trump=="Liberal" | Trump =="Conservative")
summary(subtru)
View(subtru)
summary(subtru)
attach(subtru)
sum(is.na(subtru))
subtrum<-na.omit(subtru)
sum(is.na(subtrum))
summary(subtrum)
subtrum$Trump <- as.factor(subtrum$Trump)
dim(subtrum)
dim(subtru)
dim(data)
#plot(subtrum)
#cor(subtrum[,-c(1,ncol(subtrum))])
trump <- glm(Trump~., data=subtrum, family = binomial)
trump <- glm(Trump~Media+FamSize+Hilary+Age+Partner+SpouseEdu+Employment+Birthplace+GBirth+Dependent+Housing+Income+Education2+PartyID+Marital, family = binomial, data = subtrum)
summary(trump)
mypred <- predict(trump, type= "response")
subtrum$Pred<-ifelse(mypred>0.5,"Liberal","Conservative")
xtabs(~Trump+Pred,data = subtrum)
mean(subtrum$Trump==subtrum$Pred)
#Accurary
trumpaov <- aov(Trump~Media+FamSize+Hilary+Age+Partner+SpouseEdu+Employment+Birthplace+GBirth+Dependent+Housing+Income+Education2+PartyID+Marital, data = data)
trumpaov <- aov(Trump~.,data=data)
summary(trumpaov)
plot(trumpaov,1)
plot(trumpaov,2)
plot(trumpaov,3)
plot(trumpaov,4)
plot(trumpaov,5)
plot(trumpaov,6)
###############################
Train.data<-sample(1:nrow(subtrum),size=2600,replace = F)
#Train.data<-subtrum[subtrum$Age<50,]
#Test.data<-subtrum[subtrum$Age>=50,]
Test.data <- -Train.data
View(Train.data)
View(Test.data)
View(Test.data)
trump3 <- glm(Trump~Media+FamSize+Hilary+Age+Partner+SpouseEdu+Employment+Birthplace+GBirth+Dependent+Housing+Income+Education2+PartyID+Marital, family = binomial, data = Train.data)
summary(trump3)
mypred3 <- predict(trump3, type= "response",newdata = Test.data)
Test.data$Pred<-ifelse(mypred3>0.5,"Liberal","Conservative")
xtabs(~Trump+Pred,data = Test.data)
mean(Test.data$Trump==Test.data$Pred)

#########
trump4 <- glm(Trump~Media+FamSize+Hilary+Age+Partner+SpouseEdu+Employment+Birthplace+GBirth+Dependent+Housing+Income+Education2+PartyID+Marital, family = binomial, data = Train.data)
summary(trump4)
lda.fit <- lda(Trump~ Hilary+SpouseEdu+Birthplace+Dependent+Income+Education2, data= Train.data)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, newdata=Test.data, type="response")
lda.class = lda.pred$class
table(lda.class, Test.data$Trump)
mean(lda.class == Test.data$Trump)

qda.fit = qda(Trump~ Hilary+SpouseEdu+Birthplace+Dependent+Income+Education2, data= Train.data)
qda.fit

qda.pred = predict(qda.fit, newdata=Test.data, type="response")
qda.class = qda.pred$class
table(qda.class, Test.data$Trump)
mean(qda.class==Test.data$Trump)


############NOT COMING
train <- sample(1:dim(subtrum), dim(subtrum) * 0.8)
test <- -train
#train <- subtrum[subtrum$Age<50,]
#test <- subtrum[subtrum$Age>=50,]
sutrain <- subtrum[train, ]
sutest <- subtrum[test, ]
View(sutrain)
summary(subtrum)
train.mat <- model.matrix(Trump ~Hilary+SpouseEdu+Birthplace+Dependent+Income+Education2, data = sutrain)
test.mat <- model.matrix(Trump ~ Hilary+SpouseEdu+Birthplace+Dependent+Income+Education2, data = sutest)
grid <- 10 ^ seq(10, -2, length = 100)
fit.ridge <- glmnet(train.mat, sutrain$Trump, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
