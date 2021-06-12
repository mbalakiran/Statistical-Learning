rm(list = ls())
install.packages('caret')
library(MASS)
library(ISLR)
library(readxl)
library(dplyr)
library(tree)
library(e1071)
library (randomForest)
library (gbm)
library(rpart)
library(ipred)
library(caret)
setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 2")
# Reading excel file as dtaframe
df <-read_excel("Data_Cortex_Nuclear.xls")


# Removing the index
df <- df[,c(2:82)]

# Replacing null values
for(i in 1:77){
  df[is.na(df[,i]), i] <- min(df[,i], na.rm = TRUE)
}
########### Training and Testing Dataframe ###########

# Multi-class Dataset
df1 <- df[,c(1:77,81)]
# Binary-class Dataset
df2 <- df[,c(1:78)]
set.seed(201345)
df1$class <- as.factor(df1$class)
df2$Genotype <- as.factor(df2$Genotype)
# Sampling the dataset
train <- sample(1:nrow(df),750)

# Multi-class Train and Test Dataset
df1_train <- df1[train,]
df1_test <- df1[-train,]
View(df1_train)

# Binary-class Train and Test Dataset
df2_train <- df2[train,]
df2_test <- df2[-train,]

# c) Using bagging, random forest, and boosting perform 
# the same classification task. Compare the results of the three methods.
###Random Forest--Multi Class

ran.multi <- randomForest(class~., data=df1_train, mtry=9, 
                              importance =TRUE)
yhat.ran.multi <- predict(ran.multi ,newdata = df1_test)
ran.test.multi <- as.factor(df1_test$class)
table(predict=yhat.ran.multi, truth=ran.test.multi)
#48+37+50+35+42+33+44+35 = 324/330 = 0.98
confusionMatrix(yhat.ran.multi,ran.test.multi)

varImpPlot(ran.multi)

###Random Forest--Binary Class

ran.bi <- randomForest(Genotype~., data=df2_train, mtry=9, 
                              importance =TRUE)
yhat.ran.bi <- predict(ran.bi ,newdata = df2_test)
table(predict=yhat.ran.bi, truth=df2_test$Genotype)
#173+152 = 325/330 = 0.98
confusionMatrix(yhat.ran.bi,df2_test$Genotype)

varImpPlot(ran.bi)

####
####### Bagging- Multi Class

# bagging: multi-class

bag <- bagging(class ~ ., data = df1_train, coob=TRUE)
summary(bag)
print(bag)


yhat.bag <- predict(bag ,df1_test,type="class")
df1_test$class <- as.factor(df1_test$class)
confusionMatrix(yhat.bag, df1_test$class)

table(yhat.bag,df1_test$class)
varImpPlot(bag)
#49+37+50+35+41+33+44+35 = 324/330 = 0.98
scomb <- list(list(model=slda, predict=function(object, newdata)
  + predict(object, newdata)$x))

double_bag <- bagging(class ~ ., data = df1_train, comb=scomb)
summary(double_bag)

yhat.bag <- predict(double_bag ,df1_test,type="class")
confusionMatrix(yhat.bag, df1_test$class)

table(yhat.bag,df1_test$class)
#49+37+50+35+41+33+44+35 = 324/330 = 0.98
# bagging: binary-class

bag <- bagging(Genotype ~ ., data = df2_train, coob=TRUE)
summary(bag)
print(bag)


yhat.bag <- predict(bag ,df2_test,type="class")
confusionMatrix(yhat.bag, df2_test$Genotype)

table(yhat.bag,df2_test$Genotype)
#166+148 = 314/330 = 0.95

scomb <- list(list(model=slda, predict=function(object, newdata)
  + predict(object, newdata)$x))

double_bag <- bagging(Genotype ~ ., data = df2_train, comb=scomb)
summary(double_bag)

yhat.bag <- predict(double_bag ,df2_test,type="class")
confusionMatrix(yhat.bag, df2_test$Genotype)

table(yhat.bag, df2_test$Genotype)

##############Boosting

#####################
boost.binary <- df2
boost.binary$Genotype <- ifelse(boost.binary$Genotype == "Control",0,1)
View(boost.binary)
bb_train <- boost.binary[train,]
bb_test <- boost.binary[-train,]
#####################Boost Binary
bbinary <- gbm(Genotype~., data=bb_train, distribution = "bernoulli",
                        n.trees =5000, interaction.depth =4)
summary(bbinary)
yhat.bbinary <- predict(bbinary, newdata=bb_test,n.trees=5000)
yhat <- (yhat.bbinary - min(yhat.bbinary)) / (max(yhat.bbinary) - min(yhat.bbinary))
data_pred <- ifelse(yhat <= 0.5,0,1)
ran.test.binary <- bb_test$Genotype
table(predict=data_pred,truth=ran.test.binary)
#171+157 = 328/330 = 0.993

########## Boosting Multiclass
bmulti <- gbm(class~., data=df1_train, distribution = "multinomial",
                       n.trees =5000, verbose = F, shrinkage = 0.01, interaction.depth =4)
summary(bmulti)
yhat.bmulti <- predict(bmulti, newdata=df1_test,n.trees=5000,type="response")
pred=as.matrix(yhat.bmulti[,,1])
p.pred <- apply(pred, 1, which.max)
ran.test.multi <- df1_test$class
table(p.pred, ran.test.multi)
#50+38+50+35+43+33+43+35 = 327/330 = 0.99