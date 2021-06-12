library (randomForest)
library (gbm)
library(dplyr)
set.seed(201345)

########### Training and Testing Dataframe ###########

# Multi-class Dataset
cortex_dataframe_c1 <- cortex_dataframe[,c(1:77,81)]

# Binary-class Dataset
cortex_dataframe_c2 <- cortex_dataframe[,c(1:78)]
View(cortex_dataframe_c2)

###########################################

# Sampling the dataset
train <- sample(1:nrow(cortex_dataframe),750)

# Multi-class Train and Test Dataset
train_df1 <- cortex_dataframe_c1[train,]
test_df1 <- cortex_dataframe_c1[-train,]


# Binary-class Train and Test Dataset
train_df2 <- cortex_dataframe_c2[train,]
test_df2 <- cortex_dataframe_c2[-train,]


#####################
boost.data.binary <- cortex_dataframe_c2
boost.data.binary$Genotype <- ifelse(boost.data.binary$Genotype == "Control",0,1)
View(boost.data.binary)
boost.train.binary <- boost.data.binary[train,]
boost.test.binary <- boost.data.binary[-train,]
#####################



#####Bagging-Multi Classification##########
bag.cor.multi <- randomForest(as.factor(class)~., data=train_df1, mtry=77, importance =TRUE)
yhat.bag.multi <- predict(bag.cor.multi ,newdata = test_df1)
cor.test.multi <- as.factor(test_df1$class)
table(predict=yhat.bag.multi, truth=as.factor(boston.test.multi))
# Accuracy:   50+38+48+35+43+33+41+32= 320 > 0.969697

#####Randome Forest-Multi Classification##########
ran.cor.multi <- randomForest(as.factor(class)~., data=train_df1, mtry=9, importance =TRUE)
yhat.ran.multi <- predict(ran.cor.multi ,newdata = test_df1)
ran.test.multi <- test_df1$class
table(predict=yhat.ran.multi, truth=ran.test.mult)
# Accuracy:   > 50+37+50+35+43+33+44+35 = 327 > 327/330 = 0.9909091
varImpPlot(ran.cor.multi)

#####Bagging-Binary Classification##########
bag.cor.binary <- randomForest(as.factor(Genotype)~., data=train_df2, mtry=77, importance =TRUE)
yhat.bag.binary <- predict(bag.cor.binary ,newdata = test_df2)
cor.test.binary <- test_df2$Genotype
table(predict=yhat.bag.binary, truth=cor.test.binary)
# Accuracy:   > 163+146 = 309 > 309/330 = 0.9363636

#####Randome Forest-Binary Classification##########
ran.cor.binary <- randomForest(as.factor(Genotype)~., data=train_df2, mtry=9, importance =TRUE)
yhat.ran.binary <- predict(ran.cor.binary ,newdata = test_df2)
ran.test.binary <- test_df2$Genotype
table(predict=yhat.ran.binary, truth=ran.test.binary)
# Accuracy:   > 173+151 = 324 > 324/330 = 0.9818182
varImpPlot(ran.cor.binary)


##########Boosting-Binary Classification##############
boost.car.binary <- gbm(Genotype~., data=boost.train.binary, distribution = "bernoulli",
                        n.trees =5000, interaction.depth =4)
summary(boost.car.binary)
yhat.boost.binary <- predict(boost.car.binary, newdata=boost.test.binary,n.trees=5000)
yhat <- (yhat.boost.binary - min(yhat.boost.binary)) / (max(yhat.boost.binary) - min(yhat.boost.binary))
data_pred <- ifelse(yhat <= 0.5,0,1)
ran.test.binary <- test_df2$Genotype
table(predict=data_pred,truth=ran.test.binary)
# Accuracy : > 59+157 = 216 > 261/330 = 0.7909091
##########Boosting-Multi Classification##############
boost.car.multi <- gbm(class~., data=train_df1, distribution = "multinomial",
                        n.trees =5000, verbose = F, shrinkage = 0.01, interaction.depth =4)
summary(boost.car.multi)
yhat.boost.multi <- predict(boost.car.multi, newdata=test_df1,n.trees=5000,type="response")
pred=as.matrix(yhat.boost.multi[,,1])
p.pred <- apply(pred, 1, which.max)
ran.test.multi <- test_df1$class
table(p.pred, ran.test.multi)
# ACcuracy: > 50+38+50+35+43+33+43+35 = 327 > 327/330 = 0.9909091
