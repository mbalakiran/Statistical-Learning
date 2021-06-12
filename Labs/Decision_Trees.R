### Question 7
# In the lab, we applied random forests to the Boston data using mtry=6
# and using ntree=25 and ntree=500. Create a plot displaying the test
# error resulting from random forests on this data set for a more comprehensive
# range of values for mtry and ntree. You can model your
# plot after Figure 8.10. Describe the results obtained
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
attach(Boston)
##########################################################################
#### split into training and testing dataset
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
##########################################################################
#### built a random forest model
bag.boston <- randomForest(medv~.,data=Boston , subset =train,
                           mtry = 11, ntree =25)
yhat.bag <- predict(bag.boston ,newdata = Boston[-train,])
boston.test <- Boston[-train ,"medv"]
mean((yhat.bag - boston.test)^2)
##########################################################################
#### range the value of mtry
error_value <- c()
for (i in 2:12) {
  
  bag.boston <- randomForest(medv~.,data=Boston , subset =train,
                             mtry=i, ntree = 500)
  yhat.bag <- predict(bag.boston ,newdata = Boston[-train,])
  boston.test <- Boston[-train ,"medv"]
  error_value <- c(error_value, mean((yhat.bag - boston.test)^2))
 
}
plot(error_value, type = "l")
##########################################################################
#### range the value for ntree 
error_value <- c()
for (i in 10:1000) {
  
  bag.boston <- randomForest(medv~.,data=Boston , subset =train,
                             mtry = 5, ntree = i)
  yhat.bag <- predict (bag.boston ,newdata = Boston[-train,])
  boston.test <- Boston[-train ,"medv"]
  error_value <- c(error_value, mean((yhat.bag - boston.test)^2))
  
  
}
plot(error_value, type = "l")
##########################################################################
#### contour plot
ntrees <- seq(10, 1000, by=15)
error_value <- c()
for (i in 2:12) {
  
  for (j in ntrees) {
    
    bag.boston <- randomForest(medv~.,data=Boston , subset =train,
                               mtry= i, ntree = j)
    yhat.bag <- predict (bag.boston ,newdata = Boston[-train,])
    boston.test <- Boston[-train ,"medv"]
    error_value <- c(error_value, mean((yhat.bag - boston.test)^2))
    
    
    
  }
}
plot(error_value, type = "l")
######### create a contour plot
x <- c(rep(2,67),rep(3,67),rep(4,67),rep(5,67),rep(6,67),rep(7,67),
       rep(8,67),rep(9,67),rep(10,67),rep(11,67),rep(12,67))
y <- rep(seq(10, 1000, by=15),11)
z <- matrix(error_value, nrow = 11, ncol = 67)
contour(seq(2:12),seq(10, 1000, by=15),z,nlevels = 15)
########## another try
df <- data.frame(x=x,y=y,z=error_value)
library(plotly)    
p <- plot_ly(data = df, x=~x,y=~y, z=~z, type = "contour", colorscale='Jet')
p
################################################################################
#### exercise 8
# In the lab, a classification tree was applied to the Carseats data set after
# converting Sales into a qualitative response variable. Now we will
# seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.
# (a) Split the data set into a training set and a test set.
attach(Carseats)
set.seed(3)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
Carseats.train <- Carseats[train,]
# (b) Fit a regression tree to the training set. Plot the tree, and interpret
# the results. What test MSE do you obtain?
tree.carseats <- tree(Sales ~., Carseats.train)
summary(tree.carseats)
x11()
plot(tree.carseats)
text(tree.carseats ,pretty =0)
yhat.bag <- predict(tree.carseats ,newdata = Carseats.test)
carseat.test <- Carseats.test$Sales
mean((yhat.bag - carseat.test)^2)
# (c) Use cross-validation in order to determine the optimal level of
# tree complexity. Does pruning the tree improve the test MSE?
cv.carseats <- cv.tree(tree.carseats)
cv.carseats
plot(cv.carseats$size ,cv.carseats$dev ,type="b")
pruned_tree <- prune.tree(tree.carseats, best = 5)
yhat.bag <- predict(pruned_tree ,newdata = Carseats.test)
carseat.test <- Carseats.test$Sales
mean((yhat.bag - carseat.test)^2)
# (d) Use the bagging approach in order to analyze this data. What
# test MSE do you obtain? Use the importance() function to determine
# which variables are most important.
set.seed(1)
bag.carseats  <- randomForest(Sales ~ .,data=Carseats.train,
                            mtry=10, importance =TRUE)
yhat.bag <- predict(bag.carseats ,newdata = Carseats.test)
carseat.test <- Carseats.test$Sales
mean((yhat.bag - carseat.test)^2)
bag.carseats
importance(bag.carseats)
varImpPlot(bag.carseats)
# (e) Use random forests to analyze this data. What test MSE do you
# obtain? Use the importance() function to determine which variables
# are most important. Describe the effect of mtry, the number of
# variables considered at each split, on the error rate
# obtained.
bag.carseats  <- randomForest(Sales ~.,data=Carseats.train,
                              mtry=6, importance =TRUE)
yhat.bag <- predict(bag.carseats ,newdata = Carseats.test)
carseat.test <- Carseats.test$Sales
mean((yhat.bag - carseat.test)^2)

#####################################################################
# ntrees <- seq(10, 1000, by=15)
# error_value <- c()
# for (i in 2:10) {
#   
#   for (j in ntrees) {
#   
#     rf.car <- randomForest(Sales ~ .,data = Carseats.train,
#                                mtry = i, ntree = j)
#     yhat.rf <- predict(rf.car ,newdata = Carseats.test)
#     car.test <- Carseats.test["Sales"]
#     error_value <- c(error_value, mean((unname(yhat.rf) - car.test[,1])^2))
#     
#     
#     
#   }
# }
# plot(error_value, type = "l")
# ######### create a contour plot
# x <- c(rep(2,67),rep(3,67),rep(4,67),rep(5,67),rep(6,67),rep(7,67),
#        rep(8,67),rep(9,67),rep(10,67))
# y <- rep(seq(10, 1000, by=15),9)
# df <- data.frame(x=x,y=y,z=error_value)
# library(plotly)    
# p <- plot_ly(data = df, x=~x,y=~y, z=~z, type = "contour", colorscale='Jet')
# p
####################################################################
# Question 11
# This question uses the Caravan data set.
# (a) Create a training set consisting of the first 1,000 observations,
# and a test set consisting of the remaining observations.
data <- Caravan
data$Purchase <- ifelse(data$Purchase == "No",0,1)
caravan.training <- data[1:1000,]
caravan.test <- data[1001:5822,]
# (b) Fit a boosting model to the training set with Purchase as the
# response and the other variables as predictors. Use 1,000 trees,
# and a shrinkage value of 0.01. Which predictors appear to be
# the most important?
library(gbm)
boost.caravan <- gbm(Purchase ~ . ,data=caravan.training, distribution = "bernoulli",
                     n.trees = 1000 , interaction.depth = 4, shrinkage = 0.01,
                     verbose = F)
summary(boost.caravan)
# (c) Use the boosting model to predict the response on the test data.
# Predict that a person will make a purchase if the estimated probability
# of purchase is greater than 20 %. Form a confusion matrix.
# What fraction of the people predicted to make a purchase
# do in fact make one? How does this compare with the results
# obtained from applying KNN or logistic regression to this data
# set?
yhat.boost <- predict(boost.caravan , newdata = caravan.test,
                    n.trees = 1000)
yhat <- (yhat.boost - min(yhat.boost)) / (max(yhat.boost) - min(yhat.boost))
data_pred <- ifelse(yhat <= 0.2,0,1)
table(data_pred, caravan.test$Purchase)
247 / (247+2743)
###############################################################

