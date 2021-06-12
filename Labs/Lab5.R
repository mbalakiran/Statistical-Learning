#install.packages("pls")
library(ISLR)
library(glmnet)
library(pls)
?College
names(College)
View(College)
set.seed(100)
#lr<-glm(Apps~.,data=College)
#summary(lr)
#train <- sample(dim(College)[1], dim(College)[1] / 2)
#a
train <- sample(1:dim(College)[1], dim(College)[1] * 0.8)
#dim(train)[1]
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]
#B
fit.lm <- lm(Apps ~ ., data = College.train)
summary(fit.lm)
pred.lm <- predict(fit.lm,College.test)
mean((pred.lm - College.test$Apps)^2)
#mean((pred.lm != College.test$Apps)^2)
#C
train.mat <- model.matrix(Apps ~ ., data = College.train)
test.mat <- model.matrix(Apps ~ ., data = College.test)
grid <- 10 ^ seq(10, -2, length = 100)
fit.ridge <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
#fit.ridge1 <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid)
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
#cv.ridge1 <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
#bestlam.ridge1<- cv.ridge1$lambda.min
pred.ridge <- predict(fit.ridge, newx = test.mat, s = bestlam.ridge)
mean((pred.ridge - College.test$Apps)^2)
#sqrt(mean((pred.ridge - College.test$Apps)^2))
#D
fit.lasso <- glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - College.test$Apps)^2)
predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
#E
fit.pcr <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, College.test, ncomp = 10)
mean((pred.pcr - College.test$Apps)^2)
