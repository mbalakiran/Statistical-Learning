rm(list = ls())
library(MASS)
library(ISLR)
library(readxl)
library(dplyr)
library(tree)
library(e1071)
library(vctrs)
library(tibble)
library(VIM)
library(tidyverse)
library(caret)

setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 2")
# Reading excel file as dtaframe
df <-read_excel("Data_Cortex_Nuclear.xls")
#sum(is.na(df$Genotype))
#sum(is.na(df$Treatment))
#sum(is.na(df$Behavior))
#sum(is.na(df$class))

# Removing the index
df <- df[,c(2:82)]

# Replacing null values
for(i in 1:77){
  df[is.na(df[,i]), i] <- min(df[,i], na.rm = TRUE)
}

#Check whether nullvalues removed or not
sum(is.na(df))
View(df)
summary(df)
View(df)
unique(df$Genotype)
unique(df$class)
unique(df$Treatment)
unique(df$Behavior)

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
#df1_train$class <- as.factor(df1_train$class)
#df1_test$class <- as.factor(df1_test$class)
View(df1_train)

# Binary-class Train and Test Dataset
df2_train <- df2[train,]
df2_test <- df2[-train,]
#df2_train$Genotype <- as.factor(df2_train$Genotype)
#df2_train$Genotype <- as.factor(df2_train$Genotype)
# 1. a) Use the 77 proteins as predictors for decision trees and support vector machines 
# models to make binary and multiple class classification

################ Multi-class: Decision Tree #####################
tree.df1_train <- tree(class ~., df1_train)
summary(tree.df1_train)

x11()
plot(tree.df1_train)
text(tree.df1_train ,pretty =0)
yhat.bag <- predict(tree.df1_train ,df1_test,type="class")
table(yhat.bag,df1_test$class)
#table(yhat.bag,df1_test$class)
confusionMatrix(yhat.bag,df1_test$class)
#  Mean: 42+23+40+28+19+25+38+31= 246/330 = 0.7454545

################ Binary-class: Decision Tree #####################
tree.df2_train <- tree(df2_train$Genotype ~., df2_train)
summary(tree.df2_train)
x11()
plot(tree.df2_train)
text(tree.df2_train ,pretty =0)
yhat.bag2 <- predict(tree.df2_train ,df2_test, type="class")
table(yhat.bag2,df2_test$Genotype)
confusionMatrix(yhat.bag2,df2_test$Genotype)
# > 149+134 = 283 -> 283/330= 0.8575758

####################################################
#Use cross-validation in order to determine the optimal level of tree complexity.
#Does pruning the tree improve the test MSE?
################ Multi-class: Decision Tree #####################
set.seed(367634)
cv.df1_train <-cv.tree(tree.df1_train, FUN=prune.misclass)
names(cv.df1_train)
cv.df1_train
par(mfrow =c(1,2))
plot(cv.df1_train$size ,cv.df1_train$dev ,type="b")
plot(cv.df1_train$k ,cv.df1_train$dev ,type="b")
prune.df1_train <- prune.misclass(tree.df1_train , best =20)
### plot the pruned tree
x11()
plot(prune.df1_train)
text(prune.df1_train ,pretty =0)
pred.test3 <- predict(prune.df1_train ,df1_test,type="class")
table(pred.test3,df1_test$class)
confusionMatrix(pred.test3,df1_test$class)
#42+23+40+28+19+25+38+31 = 246/330 = 0.745
################ Binary-class: Decision Tree #####################
set.seed(367634)
cv.df2_train <-cv.tree(tree.df2_train ,FUN=prune.misclass)
names(cv.df2_train)
cv.df2_train
par(mfrow =c(1,2))
plot(cv.df2_train$size ,cv.df2_train$dev ,type="b")
plot(cv.df2_train$k ,cv.df2_train$dev ,type="b")
prune.df2_train <- prune.misclass(tree.df2_train , best =18)
### plot the pruned tree
x11()
plot(prune.df2_train)
text(prune.df2_train ,pretty =0)
pred.test4 <- predict(prune.df2_train ,df2_test,type="class")
table(pred.test4,df2_test$Genotype)
#148+132 = 280/329 = 0.85
confusionMatrix(pred.test4,df2_test$Genotype)
####################################################

########## Multi-class:support vector classifier-Linear ################
## Linear
svmfit_linear <- svm(class~., data=df1_train, kernel ="linear", cost = 0.1,
                     scale =FALSE)
ypred <- predict(svmfit_linear ,df1_test)
table(predict = ypred , truth= df1_test$class)
#24+21+50+34+36+27+26+35 = 253/330= 0.76
confusionMatrix(ypred,df1_test$class)
## Linear
svmfit_linear <- svm(class~., data=df1_train, kernel ="linear", cost = 0.01,
                     scale =FALSE)
ypred <- predict(svmfit_linear ,df1_test)
table(predict = ypred , truth= df1_test$class)
#19+9+48+30+18+1+10+27 =  162/330 = 0.49
confusionMatrix(ypred,df1_test$class)
#  Fit a support vector classifier to the data with various values of cost, Report the 
# cross-validation errors associated with different values of this parameter
set.seed(24583)
tune.out1 <- tune(svm, class ~., data=df1_train, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1 

#Number of Support Vectors:  327
#Best model 1 & Applying it
svmfit_linear <- svm(class~., data=df1_train, kernel ="linear", cost = 1,
                     scale =FALSE)
ypred <- predict(svmfit_linear ,df1_test)
table(predict = ypred , truth= df1_test$class)
#46+27+50+35+36+32+42+35 = 303/330= 0.91
confusionMatrix(ypred,df1_test$class)
############### Multi-class:support vector classifier-Radial #####################
svmfit_RBF1 <- svm(class~., data=df1_train, kernel ="radial", gamma = 0.1,
                   cost = 0.1)
ypredr1 <- predict(svmfit_RBF1 ,df1_test)
table(predict = ypredr1 , truth=df1_test$class)
#50+0+43+31+0+0+0+35 = 159/330 = 0.48
confusionMatrix(ypredr1,df1_test$class)
#################
set.seed(142342)
tune.outr1 <- tune(svm, class ~., data=df1_train, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outr1)
bestmodr1 <- tune.outr1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#best parameters:
#cost gamma
#100 0.001

#Number of Support Vectors:  361

svmfit_RBF1 <- svm(class~., data=df1_train, kernel ="radial", gamma = 0.001,
                   cost = 100)
ypredr1 <- predict(svmfit_RBF1 ,df1_test)
table(predict = ypredr1 , truth=df1_test$class)
#50+38+50+34+44+33+45+35 = 329/330 = 0.996
confusionMatrix(ypredr1,df1_test$class)
#####################Multi-class:support vector classifier-Polynomial######################

svmfit_poly1 <- svm(class~., data=df1_train, kernel="polynomial",degree = 2, gamma = 0.1,
                    cost = 0.01)

ypredp1 <- predict(svmfit_poly1 ,df1_test)
table(predict = ypredp1 , truth=df1_test$class)
#50+37+50+34+40+31+39+32 = 313/330 = 0.948
confusionMatrix(ypredp1,df1_test$class)
#Tune
set.seed(142342)
tune.outp1 <- tune(svm, class ~., data=df1_train, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(tune.outp1)
bestmodp1 <- tune.outp1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  5 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  404
svmfit_poly2 <- svm(class~., data=df1_train, kernel="polynomial",degree = 3,
                    cost = 5)

ypredp2 <- predict(svmfit_poly2 ,df1_test)
table(predict = ypredp2 , truth=df1_test$class)
#50+38+50+35+44+33+45+35 = 330/330 = 100
confusionMatrix(ypredp2,df1_test$class)
################# Binary Class: support vector classifier- Linear###################

svmfit_linearbl1 <- svm(Genotype~., data = df2_train, kernel ="linear", cost = 0.01,
                        scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,df2_test)
table(predict = ypredbl1 , truth= df2_test$Genotype)
#141+62 = 203/330 = 0.615
confusionMatrix(ypredbl1,df2_test$Genotype)
##Tune
set.seed(142342)
tune.outbl1 <- tune(svm, Genotype ~., data=df2_train, kernel = "linear",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outbl1)
bestmod1 <- tune.outbl1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  0.1 

#Number of Support Vectors:  152
svmfit_linearbl1 <- svm(Genotype~., data = df2_train, kernel ="linear", 
                        cost = 0.1, scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,df2_test)
table(predict = ypredbl1 , truth= df2_test$Genotype)
confusionMatrix(ypredbl1,df2_test$Genotype)
#141+122 = 263/330 = 0.796
####Radial
svmfit_RBFbr1 <- svm(Genotype~., data=df2_train, kernel ="radial", 
                     gamma = 1, cost = 1)
ypredrb1 <- predict(svmfit_RBFbr1 ,df2_test)
table(predict = ypredrb1 , truth= df2_test$Genotype)
#173+6 = 179/330 =  0.5424242
confusionMatrix(ypredrb1,df2_test$Genotype)
set.seed(242442)
tune.outrbr1 <- tune(svm, Genotype ~., data=df2_train, kernel = "radial",
                     ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                   gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outrbr1)
bestmodrb1 <- tune.outrbr1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#best parameters:
#cost gamma
#5  0.01

#Number of Support Vectors:  246

svmfit_RBFbr1 <- svm(Genotype~., data=df2_train, kernel ="radial", 
                     gamma = 0.01, cost = 5)
ypredrb1 <- predict(svmfit_RBFbr1 ,df2_test)
table(predict = ypredrb1 , truth= df2_test$Genotype)
#173+157 = 330/330 =  100
confusionMatrix(ypredrb1,df2_test$Genotype)
#####polynomial
svmfit_polypb2 <- svm(Genotype~., data=df2_train, kernel="polynomial",
                      degree = 2, gamma = 1, cost = 0.01)
ypredpb2 <- predict(svmfit_polypb2 ,df2_test)
table(predict = ypredpb2 , truth= df2_test$Genotype)
# 170+155 = 325/330 = 0.9848485
confusionMatrix(ypredpb2,df2_test$Genotype)
###############################
tune.outpb1 <- tune(svm, as.factor(Genotype) ~., data=df2_train, kernel = "polynomial",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                  degree=c(2,3,4)))
summary(tune.outpb1)
bestmodpb1 <- tune.outpb1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  5 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  280
svmfit_polypb2 <- svm(Genotype~., data=df2_train, kernel="polynomial",
                      degree = 3, gamma = 1,cost = 5)
ypredpb2 <- predict(svmfit_polypb2 ,df2_test)
table(predict = ypredpb2 , truth= df2_test$Genotype)
#172+156 = 328/330 = 0.993
confusionMatrix(ypredpb2,df2_test$Genotype)

#b) Perform principal component analysis on the 77 numerical features. 
#Use an appropriate number of principal components as predictors and 
#perform the same classification task.

###PCA

pr.out <- prcomp(df[,c(1:77)], scale = TRUE)
biplot(pr.out , scale = 0)
attach(df)

pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of
         Variance Explained", ylim=c(0,1) ,type="b")

plot(cumsum (pve ), xlab="Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim=c(0,1),
     type="b")

comp <- data.frame(pr.out$x[,1:9])
View(comp)
attach(comp)

########### Training and Testing Dataframe ###########

# Multi-class Dataset
dfc <- df[81]
View(dfc)

# Binary-class Dataset
dfg <- df[78]
dfg <- df[78]
View(dfg)
set.seed(201345)
################ Multi-class: Decision Tree #####################
dfmd <- data.frame(comp,dfc)
train <- sample(1:nrow(dfmd),750)
df_trainm <- dfmd[train,]
df_testm <- dfmd[-train,]
View(df_testm)
df_trainm$class <- as.factor(df_trainm$class)
df_testm$class <- as.factor(df_testm$class)
##
tree.mc = tree(class~.,df_trainm)
summary(tree.mc)
x11()
plot(tree.mc)
text(tree.mc ,pretty =0)
tree.predm1=predict (tree.mc ,df_testm ,type ="class")
table(tree.predm1,df_testm$class)
#25+26+31+28+27+15+6+20 = 178/330 = 0.53
confusionMatrix(tree.predm1,df_testm$class)

#####BINARY
dfbn <- data.frame(comp,dfg)
View(dfbn)
dfbn$Genotype <- as.factor(dfbn$Genotype)
train <- sample(1:nrow(dfbn),750)
df_trainb <- dfbn[train,]
df_testb <- dfbn[-train,]

#df_testb <- as.factor(df_testb$Genotype)
#df_trainb <- as.factor(df_trainb$Genotype)
tree.bi <- tree(Genotype~., df_trainb)
#or
tree.bi =tree(as.factor(Genotype)~., df_trainb)
summary(tree.bi)
x11()
plot(tree.bi)
text(tree.bi ,pretty =0)
tree.predm1= predict(tree.bi ,df_testb ,type ="class")
table(tree.predm1,df_testb$Genotype)
#136+122 = 258/330 = 0.781
confusionMatrix(tree.predm1,df_testb$Genotype)
###CV

################ Multi-class: Decision Tree #####################
set.seed(201345)
cv.train_df1 <- cv.tree(tree.mc, FUN=prune.misclass)
cv.train_df1
par(mfrow =c(1,2))
plot(cv.train_df1$size ,cv.train_df1$dev ,type="b")
plot(cv.train_df1$k ,cv.train_df1$dev ,type="b")
pruned_tree1 <- prune.misclass(tree.mc, best = 28)
plot(pruned_tree1)
text(pruned_tree1 ,pretty =0)
pred.test3 <- predict(pruned_tree1 ,df_testm,type="class")
test_df1cv1 <- as.factor(df_testm$class)
table(pred.test3,test_df1cv1)
#25+26+31+28+25+15+6+19 = 175/330 = 53
confusionMatrix(pred.test3,test_df1cv1)

################ Binary-class: Decision Tree #####################
cv.train_df2 <- cv.tree(tree.bi, FUN=prune.misclass)
cv.train_df2
plot(cv.train_df2$size ,cv.train_df2$dev ,type="b")
pruned_tree2 <- prune.misclass(tree.bi, best = 20)
plot(pruned_tree2)
text(pruned_tree2 ,pretty =0)

pred.test4 <- predict(pruned_tree2 ,df_testb,type="class")
test_df2s2 <- as.factor(df_testb$Genotype)
table(pred.test4,test_df2s2)
#140+124 = 264/330 = 0.8
confusionMatrix(pred.test4,test_df2s2)


########## Multi-class:support vector classifier-Linear ################
## Linear
svmfit_linear1 <- svm(class~., data=df_trainm, kernel ="linear", 
                      cost = 0.01, scale =FALSE)
ypredl1 <- predict(svmfit_linear1 ,df_testm)
table(predict = ypredl1 , truth= df_testm$class)
#35+26+43+22+29+18+18+35 = 226/330 = 0.68
confusionMatrix(ypredl1,df_testm$class)

#######

#  Fit a support vector classifier to the data with various values of cost, Report the 
# cross-validation errors associated with different values of this parameter
set.seed(24583)
tune.out1 <- tune(svm, class ~., data=df_trainm, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  5 

#Number of Support Vectors:  409
#Best model 1 & Applying it
svmfit_linear <- svm(class~., data=df_trainm, kernel ="linear", cost = 5,
                     scale =FALSE)
ypred <- predict(svmfit_linear ,df_testm)
table(predict = ypred , truth= df_testm$class)
#34+32+40+29+25+25+25+34 = 244/330= 0.73
confusionMatrix(ypred,df_testm$class)

############### Multi-class:support vector classifier-Radial #####################
svmfit_RBF1 <- svm(class~., data=df_trainm, kernel ="radial", gamma = 0.1,
                   cost = 0.1)
ypredr1 <- predict(svmfit_RBF1 ,df_testm)
table(predict = ypredr1 , truth=df_testm$class)
#35+37+37+27+31+4+12+35 = 218/330 = 0.66
confusionMatrix(ypredr1,df_testm$class)
###########
set.seed(142342)
tune.outr1 <- tune(svm, class ~., data=df_trainm, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outr1)
bestmodr1 <- tune.outr1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#best parameters:
#cost gamma
#10   0.1

#Number of Support Vectors:  454

svmfit_RBF1 <- svm(class~., data=df_trainm, kernel ="radial", gamma = 0.1,
                   cost = 10)
ypredr1 <- predict(svmfit_RBF1 ,df_testm)
table(predict = ypredr1 , truth=df_testm$class)
#49+37+47+33+44+33+37+35 = 315/330 = 0.95
confusionMatrix(ypredr1,df_testm$class)

#####################Multi-class:support vector classifier-Polynomial######################

svmfit_poly1 <- svm(class~., data=df_trainm, kernel="polynomial",
                    degree = 2, gamma = 0.1, cost = 0.01)

ypredp1 <- predict(svmfit_poly1 ,df_testm)
table(predict = ypredp1 , truth=df_testm$class)
#34+3+20+32+0+0+0+13 = 102/330 = 0.30
confusionMatrix(ypredp1,df_testm$class)

#Tune
set.seed(142342)
tune.outp1 <- tune(svm, as.factor(class) ~., data=df_trainm, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(tune.outp1)
bestmodp1 <- tune.outp1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  10 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  402

svmfit_poly2 <- svm(class~., data=df_trainm, kernel="polynomial",
                    degree = 3,cost = 10)

ypredp2 <- predict(svmfit_poly2 ,df_testm)
table(predict = ypredp2 , truth=df_testm$class)
#50+37+43+34+43+32+37+35 = 311/330 = 0.94
confusionMatrix(ypredp2,df_testm$class)

################# Binary Class: support vector classifier- Linear###################

svmfit_linearbl1 <- svm(Genotype~., data = df_trainb, kernel ="linear", 
                        cost = 0.01, scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,df_testb)
table(predict = ypredbl1 , truth= df_testb$Genotype)
#143+99 = 242/330 = 0.73
confusionMatrix(ypredbl1,df_testb$Genotype)

##Tune
set.seed(142342)
tune.outbl1 <- tune(svm, as.factor(Genotype) ~., data=df_trainb, kernel = "linear",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outbl1)
bestmod1 <- tune.outbl1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  5 

#Number of Support Vectors:  480
svmfit_linearbl1 <- svm(Genotype~., data = df_trainb, kernel ="linear", 
                        cost = 5, scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,df_testb)
table(predict = ypredbl1 , truth= df_testb$Genotype)
#140+102 = 242/330 = 0.796
confusionMatrix(ypredbl1,df_testb$Genotype)

####Radial
svmfit_RBFbr1 <- svm(Genotype~., data=df_trainb, kernel ="radial", 
                     gamma = 1, cost = 1)
ypredrb1 <- predict(svmfit_RBFbr1 ,df_testb)
table(predict = ypredrb1 , truth= df_testb$Genotype)
#166+158 = 324/330 =  0.98
confusionMatrix(ypredrb1,df_testb$Genotype)

set.seed(242442)
tune.outrbr1 <- tune(svm, as.factor(Genotype) ~., data=df_trainb, kernel = "radial",
                     ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                   gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outrbr1)
bestmodrb1 <- tune.outrbr1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#best parameters:
#cost gamma
#5     1
#Number of Support Vectors:  603

svmfit_RBFbr1 <- svm(Genotype~., data=df_trainb, kernel ="radial", 
                     gamma = 1, cost = 5)
ypredrb1 <- predict(svmfit_RBFbr1 ,df_testb)
table(predict = ypredrb1 , truth= df_testb$Genotype)
#165+161 = 326/330 =  0.98
confusionMatrix(ypredrb1,df_testb$Genotype)

#####polynomial
svmfit_polypb2 <- svm(Genotype~., data=df_trainb, kernel="polynomial",
                      degree = 2, gamma = 1, cost = 0.01)
ypredpb2 <- predict(svmfit_polypb2 ,df_testb)
table(predict = ypredpb2 , truth= df_testb$Genotype)
# 151+119 = 270/330 = 0.81
confusionMatrix(ypredpb2,df_testb$Genotype)

tune.outpb1 <- tune(svm, as.factor(Genotype) ~., data=df_trainb, kernel = "polynomial",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                  degree=c(2,3,4)))
summary(tune.outpb1)
bestmodpb1 <- tune.outpb1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  100 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  150
svmfit_polypb2 <- svm(Genotype~., data=df_trainb, kernel="polynomial",
                      degree = 3, gamma = 1,cost = 100)
ypredpb2 <- predict(svmfit_polypb2 ,df_testb)
table(predict = ypredpb2 , truth= df_testb$Genotype)
#159+150 = 309/330 = 0.93
confusionMatrix(ypredpb2,df_testb$Genotype)
###########






















































































rm(list = ls())
#install.packages("VIM")
library(MASS)
library(ISLR)
library(readxl)
library(dplyr)
library(tree)
library(e1071)
library(rlang)

library(tree)
library(readxl)
library(MASS)
library(ISLR)
library(dplyr)
library(e1071)
library(na.tools)
library(vctrs)
library(tibble)
library(VIM)
library(cluster)
setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 2")
df <- read_excel("Data_Cortex_Nuclear.xls")
View(df)
#sum(is.na(df$Genotype))
#sum(is.na(df$Treatment))
#sum(is.na(df$Behavior))
#sum(is.na(df$class))
df <- df[,c(2:82)]
dim(df)
#na_mean(df)
#for(i in 1:77){
##  a <- which_na(df[,i])
#  df[a,i] <- mean(df[,i],na.rm = T)
#}
sum(is.na(df))

which_na(df)
for(i in 1:77){
  df[is.na(df[,i]), i] <- mean(df[,i], na.rm = T)
}
#(ncol(df)-4)
for(i in 1:77){
  df[is.na(df[, i]),i] <- min(df[,i], na.rm =T)
}

sum(is.na(df))

summary(df)
View(df)
unique(df$Genotype)
unique(df$class)
unique(df$Treatment)
unique(df$Behavior)

########### Training and Testing###########

# Multi-class Dataset
df1 <- df[,c(1:77,81)]
View(df1)
sum(is.na(df1))

# Binary-class Dataset
df2 <- df[,c(1:78)]
sum(is.na(df2))
1080*0.70


set.seed(201345)

#####Multi Class
train <- sample(1:nrow(df1), 750)
df1_train <- df1[train]
df1_test <- df1[-train]

####Binary class
train <- sample(1:nrow(df2), 756)

df2_train <- df2[train]
df2_test <- df2[-train]

#### 2

aggr(df)


df2 <- df[,c(1:77)]
View(df2)
sum(is.na(df2))


df2 <- scale(df2)
#################################################################
wcc <- 0
for(i in 1:20) {
  
  km.output <- kmeans(df2[], centers = i, nstart = 20, iter.max = 50 )
  wcc[i] <- km.output$tot.withinss
}
summary(km.output)
#View(df[,-c(1)])
#Plotting model quality vs. number of clusters

plot(1:20, wcc, type="b", col="red", xlab="Number of clusters", ylab = "WCC",
     main = "Scree plot (Model quality vs. Number of clusters)")
#Based on scree plot the optimal number of cluster is between 4 and 7

k <- 3

#Making the model with 6 clusters
km.out <- kmeans(df2, centers = k, nstart = 20, iter.max = 50)

#Adding column with cluster numbers
df2 <- cbind(df2, clusterNumber = km.out$cluster)
View(df2)
#View the resulting model
clusplot(df2, km.out$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main = "Graphical Model Representation")

cdf <- as.data.frame(df2)
cluster.one <- cdf[cdf$clusterNumber==1,]
View(cluster.one)
cluster.two <- cdf[cdf$clusterNumber==2,]
View(cluster.two)
cluster.three <- cdf[cdf$clusterNumber==3,]
View(cluster.three)