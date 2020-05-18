install.packages("class")
library(MASS)
library(glmnet)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(class)
set.seed(1)
setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 1")
data <- read.csv("ANES2016.csv", header = TRUE)
data3 <- data
dt<- read.csv("ANES2016.csv", header = TRUE)
dt<-na.omit(dt)
View(data)
data$Trump[data$Trump >0 & data$Trump < 4] <- "Liberal" 
data$Trump[data$Trump>=4 & data$Trump <= 7] <- "Conservative"
data$Trump
attach(data)
View(data)
subtrump <- subset(data,Trump=="Liberal" | Trump =="Conservative")
sum(is.na(subtrump))
subtrump<-na.omit(subtrump)
subtrump$Trump <- as.factor(subtrump$Trump)
summary(subtrump)
#plot(subtrump)
#cor(subtrump[,-c(1,ncol(subtrump))])
pairs(subtrump, col=subtrump$Trump)

View(data3)
sum(is.na(data3))
data3 <- na.omit(data3)
data3 <- cor(data3)

my_data <- data3[, c(2,3,4,5,14,15,16)]
chart.Correlation(my_data, histogram=TRUE, pch=19)
my_data <- data3[, c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

qplot(x=Var1,y=Var2,data=melt(cor(dt)),fill = value,geom="tile",colour=I("white"))

#correlation between variables
variables_correlation <- cor(as.matrix(dt))

corrplot(variables_correlation, method="circle",
         diag = FALSE , type = "upper")
 
plot(Trump~.,data=subtrump)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = data3, col = col, symm = TRUE)


trumpaov <- aov(Trump~Media+FamSize+Hilary+Age+Partner+SpouseEdu+Employment+Birthplace+GBirth+Dependent+Housing+Income+Education2+PartyID+Marital, data = data)
trumpaov <- aov(Trump~.,data=data)
summary(trumpaov)
plot(trumpaov,1)
plot(trumpaov,2)
plot(trumpaov,3)
plot(trumpaov,4)
plot(trumpaov,5)
plot(trumpaov,6)

####
subtrump2 <- subtrump
trump <- glm(Trump~., data=subtrump2, family = binomial)
trump <- glm(Trump~Hilary+Age+Partner+SpouseEdu+GBirth+Dependent+Income
             +Education2, family = binomial, data = subtrump2)
#trump <- glm(Trump~Media+FamSize+Hilary+Age+Partner+Employment+Birthplace
  #           +GBirth+Income+Education2+PartyID+Marital, family = binomial, data = subtrump2)

summary(trump)
mypred <- predict(trump, type= "response")
subtrump2$Pred<-ifelse(mypred>0.5,"Liberal","Conservative")
xtabs(~Trump+Pred,data = subtrump2)
mean(subtrump2$Trump==subtrump2$Pred)
View(subtrump2)

#######2
data2 <- subtrump
View(data2)

sum(is.na(data2))
samples<-sample(1:nrow(subtrump2),size=3000,replace = F)
train.data<- subtrump2[samples,]
test.data <- subtrump2[-samples,]
lda.fit <- lda(PartyID~., data= train.data)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
prop.table(xtabs(~PartyID+lda.class,data=test.data),1)
table(lda.class, test.data$PartyID)
mean(lda.class == test.data$PartyID)

##
lda.fit <- lda(PartyID~Hilary+Age+Partner+SpouseEdu+GBirth+Dependent+Income
               +Education2, data= train.data)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, newdata=test.data, type="response")
lda.class = lda.pred$class
prop.table(xtabs(~PartyID+lda.class,data=test.data),1)
table(lda.class, test.data$PartyID)
mean(lda.class == test.data$PartyID)


qda.fit = qda(PartyID~., data= train.data)
qda.fit

qda.pred = predict(qda.fit, newdata=test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$PartyID)
mean(qda.class==test.data$PartyID)

##
qda.fit = qda(PartyID~Hilary+Age+Partner+SpouseEdu+GBirth+Dependent+Income
              +Education2, data= train.data)
qda.fit

qda.pred = predict(qda.fit, newdata=test.data, type="response")
qda.class = qda.pred$class
table(qda.class, test.data$PartyID)
mean(qda.class==test.data$PartyID)

############################Method 3: K-Nearest Neighbours (KNN)########################

train.PartyID <-train.data$PartyID
train.data.knn1 <-cbind(train.data$Hilary,train.data$Education2,
                        train.data$Media,train.data$Age, train.data$Birthplace, 
                        train.data$Income, train.data$FamSize, train.data$Partner,
                        train.data$SpouseEdu, train.data$Dependent, train.data$Housing)
test.data.knn1 <- cbind(test.data$Hilary,test.data$Education2,test.data$Media,test.data$Age, test.data$Birthplace, 
                        test.data$Income, test.data$FamSize, test.data$Partner,
                        test.data$SpouseEdu, test.data$Dependent, test.data$Housing)

knn.pred1=knn(train=train.data.knn1,test=test.data.knn1,cl=train.PartyID,k=3)
prop.table(xtabs(~test.data$PartyID+knn.pred1),1)
mean(knn.pred1==test.data$PartyID)

knn.pred1=knn(train=train.data.knn1,test=test.data.knn1,cl=train.PartyID,k=10)
prop.table(xtabs(~test.data$PartyID+knn.pred1),1)
mean(knn.pred1==test.data$PartyID)

#train.PartyID <-train.data$PartyID
#train.data.knn <-cbind(train.data$Hilary,train.data$Education2,train.data$Media,train.data$Age)
#test.data.knn <- cbind(test.data$Hilary,test.data$Education2,test.data$Media,test.data$Age)

#train.PartyID <-train.data$PartyID
#train.data.knn1 <-cbind(train.data$Hilary,train.data$Education2,train.data$Media,train.data$Age)
#test.data.knn1 <- cbind(test.data$Hilary,test.data$Education2,test.data$Media,test.data$Age)
#knn.pred1=knn(train=train.data.knn,test=test.data.knn1,cl=train.PartyID,k=3)
#prop.table(xtabs(~test.data$PartyID+knn.pred1),1)
#mean(knn.pred1==test.data$PartyID)




##########
#train.mat <- model.matrix(PartyID ~Hilary+Trump+SpouseEdu
#                          +Birthplace+Dependent+Income+Education2, data = train.data)
#test.mat <- model.matrix(PartyID ~ Hilary+Trump+SpouseEdu+Birthplace
#                         +Dependent+Income+Education2, data = test.data)
#grid <- 10 ^ seq(10, -2, length = 100)
#fit.ridge <- glmnet(train.mat, train.data$PartyID, alpha = 0, lambda = grid, thresh = 1e-12)
#cv.ridge <- cv.glmnet(train.mat, train.data$PartyID, alpha = 0, lambda = grid, thresh = 1e-12)


#bestlam.ridge <- cv.ridge$lambda.min
#bestlam.ridge

#pred.ridge <- predict(fit.ridge, newx = test.mat, s = bestlam.ridge)
#mean((pred.ridge - test.data$PartyID)^2)

#####

#fit.lasso <- glmnet(train.mat, train.data$PartyID, alpha = 1, lambda = grid, thresh = 1e-12)

#cv.lasso <- cv.glmnet(train.mat, train.data$PartyID, alpha = 1, lambda = grid, thresh = 1e-12)

#bestlam.lasso <- cv.lasso$lambda.min
#bestlam.lasso

#pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)

#mean((pred.lasso - test.data$PartyID)^2)
#predict(fit.lasso, s = bestlam.lasso, type = "coefficients")

#set.seed(1)
#train.X = cbind(train.data$PartyID)
#test.X = cbind(test.data$PartyID)
#train.Y = cbind(training.data$Direction)
#knn.pred = knn(train.X, test.X, train.Y, k=1)
#table(knn.pred, test.data$Direction)
#corrl<-cbind(diff(log(subtrump(Trump))),diff(log(subtrump(Media))),diff(log(subtrump(FamSize))),diff(log(subtrump(Age))),diff(log(subtrump(Hilary))))
#View(corrl)
#chart.Correlation(subtrump)