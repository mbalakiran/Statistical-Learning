rm(list = ls())
library(MASS)
library(ISLR)
library(readxl)
library(dplyr)
library(tree)
library(e1071)

# Reading excel file as dtaframe
cortex_dataframe <-read_excel("Data_Cortex_Nuclear.xls")

# Removing the index
cortex_dataframe <- cortex_dataframe[,c(2:82)]

# Replacing null values
for(i in 1:77){
  cortex_dataframe[is.na(cortex_dataframe[,i]), i] <- min(cortex_dataframe[,i], na.rm = TRUE)
}

#Check whether nullvalues removed or not
sum(is.na(cortex_dataframe))
View(cortex_dataframe)

########### Training and Testing Dataframe ###########

# Multi-class Dataset
cortex_dataframe_c1 <- cortex_dataframe[,c(1:77,81)]

# Binary-class Dataset
cortex_dataframe_c2 <- cortex_dataframe[,c(1:78)]

set.seed(201345)

# Sampling the dataset
train <- sample(1:nrow(cortex_dataframe),750)

# Multi-class Train and Test Dataset
test_df1 <- cortex_dataframe_c1[-train,]
train_df1 <- cortex_dataframe_c1[train,]

# Binary-class Train and Test Dataset
train_df2 <- cortex_dataframe_c2[train,]
test_df2 <- cortex_dataframe_c2[-train,]

# 1. a) Use the 77 proteins as predictors for decision trees and support vector machines 
# models to make binary and multiple class classification

################ Multi-class: Decision Tree #####################
tree.train_df1 <- tree(as.factor(train_df1$class) ~., train_df1)
summary(tree.train_df1)

x11()
plot(tree.train_df1)
text(tree.train_df1 ,pretty =0)
yhat.bag <- predict(tree.train_df1 ,test_df1,type="class")
test_df1s <- as.factor(test_df1$class)
table(yhat.bag,test_df1s)
#  Mean: 42+23+40+28+19+25+38+31= 246/330 = 0.7454545

################ Binary-class: Decision Tree #####################
tree.train_df2 <- tree(as.factor(train_df2$Genotype) ~., train_df2)
summary(tree.train_df2)
x11()
plot(tree.train_df2)
text(tree.train_df2 ,pretty =0)
yhat.bag2 <- predict(tree.train_df2 ,test_df2, type="class")
test_df2s <- as.factor(test_df2$Genotype)
table(yhat.bag2,test_df2s)
# > 149+134 = 283 > 283/330= 0.8575758


#Use cross-validation in order to determine the optimal level of tree complexity.
#Does pruning the tree improve the test MSE?
################ Multi-class: Decision Tree #####################
set.seed (335345)
cv.train_df1 <- cv.tree(tree.train_df1, FUN=prune.misclass)
cv.train_df1
par(mfrow =c(1,2))
plot(cv.train_df1$size ,cv.train_df1$dev ,type="b")
plot(cv.train_df1$k ,cv.train_df1$dev ,type="b")
pruned_tree1 <- prune.misclass(tree.train_df1, best = 21)
plot(pruned_tree1)
text(pruned_tree1 ,pretty =0)
pred.test3 <- predict(pruned_tree1 ,test_df1,type="class")
test_df1cv1 <- as.factor(test_df1$class)
table(pred.test3,test_df1cv1)

# 42+23+40+28+19+25+38+31 = 246 > 246/330 = 0.7454545
# > 43+23+44+28+19+25+35+31 = 248 > 248/330 = 0.7515152

################ Binary-class: Decision Tree #####################
cv.train_df2 <- cv.tree(tree.train_df2, FUN=prune.misclass)
cv.train_df2
plot(cv.train_df2$size ,cv.train_df2$dev ,type="b")
pruned_tree2 <- prune.misclass(tree.train_df2, best = 19)
plot(pruned_tree2)
text(pruned_tree2 ,pretty =0)

pred.test4 <- predict(pruned_tree2 ,test_df2,type="class")
test_df2s2 <- as.factor(test_df2$Genotype)
table(pred.test4,test_df2s2)
# > 148+133 = 281 > 281/330 = 0.8515152


########## Multi-class:support vector classifier-Linear ################
## Linear
svmfit_linear1 <- svm(as.factor(class)~., data=train_df1, kernel ="linear", cost = 0.01,
                     scale =FALSE)
ypredl1 <- predict(svmfit_linear1 ,test_df1)
table(predict = ypredl1 , truth= as.factor(test_df1$class))

#   > 19+9+48+30+18+1+10+27 = 162 > 162/330 = [1] 0.4909091

#  Fit a support vector classifier to the data with various values of cost, Report the 
# cross-validation errors associated with different values of this parameter
set.seed(142342)
tune.out1 <- tune(svm, as.factor(class) ~., data=train_df1, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model

# Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  0.1
#Number of Support Vectors:  342

# Applying Linear with cost 0.1
svmfit_linear2 <- svm(as.factor(class)~., data=train_df1, kernel ="linear", cost = 0.1,
                      scale =FALSE)
ypredl2 <- predict(svmfit_linear2 ,test_df1)
table(predict = ypredl2 , truth= as.factor(test_df1$class))
# 24+21+50+34+36+27+26+35 > 253/330 = 0.7666667

############### Multi-class:support vector classifier-Radial #####################
svmfit_RBF1 <- svm(as.factor(class)~., data=train_df1, kernel ="radial", gamma = 0.1,
                  cost = 0.1)
ypredr1 <- predict(svmfit_RBF1 ,test_df1)
table(predict = ypredr1 , truth=test_df1$class)
##50+0+43+31+0+0+0+35=116/330=


#Report the cross-validation errors associated with different values of this parameter
set.seed(142342)
tune.outr1 <- tune(svm, as.factor(class) ~., data=train_df1, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outr1)
bestmodr1 <- tune.outr1$best.model

######Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  100
#gamma: 0.0001
#Number of Support Vectors:  632

# Radial with cost 100 and radial 0.0001

svmfit_RBF1 <- svm(as.factor(class)~., data=train_df1, kernel ="radial", gamma = 0.0001,
                   cost = 100)
ypredr1 <- predict(svmfit_RBF1 ,test_df1)
table(predict = ypredr1 , truth=test_df1$class)
## mean > 50+38+50+35+42+33+45+35 = 328 > 328/330 = 0.9939394

#####################Multi-class:support vector classifier-Polynomial######################

svmfit_poly1 <- svm(as.factor(class)~., data=train_df1, kernel="polynomial",degree = 2, gamma = 1,
                   cost = 1)

ypredp1 <- predict(svmfit_poly1 ,test_df1)
table(predict = ypredp1 , truth=test_df1$class)
# Mean > 50+38+50+34+44+33+45+35 = 329 > 329/330 =  0.9969697

#Report the cross-validation errors associated with different values of this parameter
set.seed(142342)
tune.outp1 <- tune(svm, as.factor(class) ~., data=train_df1, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(tune.outp1)
bestmodp1 <- tune.outp1$best.model

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  5 
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  404

# Radial with cost 5 and degree 3
svmfit_poly2 <- svm(as.factor(class)~., data=train_df1, kernel="polynomial",degree = 3,
                    cost = 5)

ypredp2 <- predict(svmfit_poly2 ,test_df1)
table(predict = ypredp2 , truth=test_df1$class)

#> 50+38+50+35+44+33+45+35 = 330 > 330/330 =100

################# Binary Class: support vector classifier- Linear###################

svmfit_linearbl1 <- svm(as.factor(Genotype)~., data = train_df2, kernel ="linear", cost = 1,
                     scale =FALSE)
ypredbl1 <- predict(svmfit_linearbl1 ,test_df2)
table(predict = ypredll1 , truth= as.factor(test_df2$Genotype))
#162+153 = 315 > 0.954

#  Fit a support vector classifier to the data with various values of cost, Report the 
# cross-validation errors associated with different values of this parameter
set.seed(142342)
tune.outbl1 <- tune(svm, as.factor(Genotype) ~., data=train_df2, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outbl1)
bestmod1 <- tune.outbl1$best.model

# Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  0.1
#Number of Support Vectors:  152

# Applying Linear with cost 0.1
svmfit_linearbl2 <- svm(as.factor(Genotype)~., data = train_df2, kernel ="linear", cost = 0.1,
                        scale =FALSE)
ypredbl2 <- predict(svmfit_linearbl2 ,test_df2)
table(predict = ypredbl2 , truth= as.factor(test_df2$Genotype))
#Mean > 141+122 = 263 > 263/330 = 0.7969697

################# Binary Class: support vector classifier- Radial###################
svmfit_RBFbr1 <- svm(as.factor(Genotype)~., data=train_df2, kernel ="radial", 
                  gamma = 1, cost = 1)
ypredrb1 <- predict(svmfit_RBFbr1 ,test_df2)
table(predict = ypredrb1 , truth= test_df2$Genotype)
#179/330 =  0.5424242

# Report the cross-validation errors associated with different values of this parameter
set.seed(142342)
tune.outrbr1 <- tune(svm, as.factor(Genotype) ~., data=train_df2, kernel = "radial",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                               gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outrbr1)
bestmodrb1 <- tune.outrbr1$best.model

######Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  5 
# gamma: 0.01
#Number of Support Vectors:  246

# radial with cost 5 and gamma 0.01 
svmfit_RBFbr2 <- svm(as.factor(Genotype)~., data=train_df2, kernel ="radial", 
                     gamma = 0.01, cost = 5)
ypredrb2 <- predict(svmfit_RBFbr2 ,test_df2)
table(predict = ypredrb2 , truth= test_df2$Genotype)
# Mean: 173+157 = 330 =100

################# Binary Class: support vector classifier- Polynomial###################
svmfit_polypb2 <- svm(as.factor(Genotype)~., data=train_df2, kernel="polynomial",degree = 2, gamma = 1,
                    cost =1)
ypredpb2 <- predict(svmfit_polypb2 ,test_df2)
table(predict = ypredpb2 , truth= test_df2$Genotype)
# > 170+155 = 325/330 = 0.9848485

# Report the cross-validation errors associated with different values of this parameter
set.seed(142342)
tune.outpb1 <- tune(svm, as.factor(Genotype) ~., data=train_df2, kernel = "polynomial",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                               degree=c(2,3,4)))
summary(tune.outpb1)
bestmodpb1 <- tune.outpb1$best.model

#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  polynomial 
#cost:  5
#degree:  3 
#coef.0:  0 

#Number of Support Vectors:  280

# Polynomial with cost 5 and degree 3

svmfit_polypb2 <- svm(as.factor(Genotype)~., data=train_df2, kernel="polynomial",degree = 3, gamma = 1,
                      cost =5)
ypredpb2 <- predict(svmfit_polypb2 ,test_df2)
table(predict = ypredpb2 , truth= test_df2$Genotype)
# MEan > 172+156 = 328 > 328/330 = 0.9939394