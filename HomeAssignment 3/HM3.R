library(VIM)
library(factoextra)
library(tidyverse)  
library(cluster)    
library(factoextra)
library(gridExtra)
library(tree)
library(cluster)
setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 3")
a<- read.table("five-personality.txt", header = TRUE, sep = "", dec = ".")
View(a)

sum(is.na(a))
summary(a)

aggr(a)

length(a)



df <- sample(1:nrow(a),200000)
df_train <- a[df,]

wss <- (nrow(df_train)*sum(apply(df_train,2,var)))

for (i in 1:10) wss[i] <- sum(kmeans(df_train,
                                     centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",col="dark violet",
     ylab="Within groups sum of squares")

k5 <- kmeans(df_train, centers = 5, nstart = 25)

k5

k5$cluster

df_train$new_cluster<-as.factor(k5$cluster)

View(df_train)

k1<-subset(df_train, new_cluster==1)
k2<-subset(df_train, new_cluster==2)
k3<-subset(df_train, new_cluster==3)
k4<-subset(df_train, new_cluster==4)
k5<-subset(df_train, new_cluster==5)
View(k1)
View(k2)
View(k3)
View(k4)
View(k5)

###############################################
count_1<-0
count_2<-0
count_3<-0
count_4<-0
count_5<-0
personality.data_k1 <- data.frame(count_1, count_2,count_3,count_4,count_5)
ncol(k1)
k1[2]###column_2
person_id<-(as.integer(rownames(k1)))
person_id[2]
for (i in 1:nrow(k1)){
  count_1<-0
  count_2<-0
  count_3<-0
  count_4<-0
  count_5<-0
  for(j in 1 :10){
    
    if(k1[i,j]>=4)
    {
      count_1<-count_1+1
    }
    
  }
  for(j in 11 :20){
    
    if(k1[i,j]>=4)
    {
      count_2<-count_2+1
    }
    
  }
  for(j in 21 :30){
    
    if(k1[i,j]>=4)
    {
      count_3<-count_3+1
    }
    
  }
  for(j in 31 :40){
    
    if(k1[i,j]>=4)
    {
      count_4<-count_4+1
    }
    
  }
  for(j in 41 :50){
    
    if(k1[i,j]>=4)
    {
      count_5<-count_5+1
    }
    
    
    
  }
  
  personality.data_k1[i,] <- data.frame(count_1, count_2,count_3,count_4,count_5)
  
}
head(personality.data_k1)
###################################1
personality.data_k2 <- data.frame(count_1, count_2,count_3,count_4,count_5)

for (i in 1:nrow(k2)){
  count_1<-0
  count_2<-0
  count_3<-0
  count_4<-0
  count_5<-0
  for(j in 1 :10){
    
    if(k2[i,j]>=4)
    {
      count_1<-count_1+1
    }
    
  }
  for(j in 11 :20){
    
    if(k2[i,j]>=4)
    {
      count_2<-count_2+1
    }
    
  }
  for(j in 21 :30){
    
    if(k2[i,j]>=4)
    {
      count_3<-count_3+1
    }
    
  }
  for(j in 31 :40){
    
    if(k2[i,j]>=4)
    {
      count_4<-count_4+1
    }
    
  }
  for(j in 41 :50){
    
    if(k2[i,j]>=4)
    {
      count_5<-count_5+1
    }
    
    
    
  }
  
  personality.data_k2[i,] <- data.frame(count_1, count_2,count_3,count_4,count_5)
  
}
head(personality.data_k2)
###################################2
personality.data_k3 <- data.frame(count_1, count_2,count_3,count_4,count_5)

for (i in 1:nrow(k3)){
  count_1<-0
  count_2<-0
  count_3<-0
  count_4<-0
  count_5<-0
  for(j in 1 :10){
    
    if(k3[i,j]>=4)
    {
      count_1<-count_1+1
    }
    
  }
  for(j in 11 :20){
    
    if(k3[i,j]>=4)
    {
      count_2<-count_2+1
    }
    
  }
  for(j in 21 :30){
    
    if(k3[i,j]>=4)
    {
      count_3<-count_3+1
    }
    
  }
  for(j in 31 :40){
    
    if(k3[i,j]>=4)
    {
      count_4<-count_4+1
    }
    
  }
  for(j in 41 :50){
    
    if(k3[i,j]>=4)
    {
      count_5<-count_5+1
    }
    
    
    
  }
  
  personality.data_k3[i,] <- data.frame(count_1, count_2,count_3,count_4,count_5)
  
}
head(personality.data_k3)
####################################################3
personality.data_k4 <- data.frame(count_1, count_2,count_3,count_4,count_5)

for (i in 1:nrow(k4)){
  count_1<-0
  count_2<-0
  count_3<-0
  count_4<-0
  count_5<-0
  for(j in 1 :10){
    
    if(k4[i,j]>=4)
    {
      count_1<-count_1+1
    }
    
  }
  for(j in 11 :20){
    
    if(k4[i,j]>=4)
    {
      count_2<-count_2+1
    }
    
  }
  for(j in 21 :30){
    
    if(k4[i,j]>=4)
    {
      count_3<-count_3+1
    }
    
  }
  for(j in 31 :40){
    
    if(k4[i,j]>=4)
    {
      count_4<-count_4+1
    }
    
  }
  for(j in 41 :50){
    
    if(k4[i,j]>=4)
    {
      count_5<-count_5+1
    }
    
    
    
  }
  
  personality.data_k4[i,] <- data.frame(count_1, count_2,count_3,count_4,count_5)
  
}
head(personality.data_k4)
##########################################4
personality.data_k5 <- data.frame(count_1, count_2,count_3,count_4,count_5)

for (i in 1:nrow(k5)){
  count_1<-0
  count_2<-0
  count_3<-0
  count_4<-0
  count_5<-0
  for(j in 1 :10){
    
    if(k5[i,j]>=4)
    {
      count_1<-count_1+1
    }
    
  }
  for(j in 11 :20){
    
    if(k5[i,j]>=4)
    {
      count_2<-count_2+1
    }
    
  }
  for(j in 21 :30){
    
    if(k5[i,j]>=4)
    {
      count_3<-count_3+1
    }
    
  }
  for(j in 31 :40){
    
    if(k5[i,j]>=4)
    {
      count_4<-count_4+1
    }
    
  }
  for(j in 41 :50){
    
    if(k5[i,j]>=4)
    {
      count_5<-count_5+1
    }
    
    
    
  }
  
  personality.data_k5[i,] <- data.frame(count_1, count_2,count_3,count_4,count_5)
  
}
head(personality.data_k5)
####################################5

personality.data_k1<-personality.data
View(personality.data_k1)
View(personality.data_k2)
View(personality.data_k3)
View(personality.data_k4)
View(personality.data_k5)

##################################################
par(mfrow=c(3,2))
hist(personality.data_k1$count_1,main="personality type-1 for cluster-1")
hist(personality.data_k1$count_2,main="personality type-2 for cluster-1")
hist(personality.data_k1$count_3,main="personality type-3 for cluster-1")
hist(personality.data_k1$count_4,main="personality type-4 for cluster-1")
hist(personality.data_k1$count_5,main="personality type-5 for cluster-1")
#####################################################1
par(mfrow=c(3,2))
hist(personality.data_k2$count_1,main="personality type-1 for cluster-2")
hist(personality.data_k2$count_2,main="personality type-2 for cluster-2")
hist(personality.data_k2$count_3,main="personality type-3 for cluster-2")
hist(personality.data_k2$count_4,main="personality type-4 for cluster-2")
hist(personality.data_k2$count_5,main="personality type-5 for cluster-2")
#######################################################2
par(mfrow=c(3,2))
hist(personality.data_k3$count_1,main="personality type-1 for cluster-3")
hist(personality.data_k3$count_2,main="personality type-2 for cluster-3")
hist(personality.data_k3$count_3,main="personality type-3 for cluster-3")
hist(personality.data_k3$count_4,main="personality type-4 for cluster-3")
hist(personality.data_k3$count_5,main="personality type-5 for cluster-3")
######################################################3
par(mfrow=c(3,2))
hist(personality.data_k4$count_1,main="personality type-1 for cluster-4")
hist(personality.data_k4$count_2,main="personality type-2 for cluster-4")
hist(personality.data_k4$count_3,main="personality type-3 for cluster-4")
hist(personality.data_k4$count_4,main="personality type-4 for cluster-4")
hist(personality.data_k4$count_5,main="personality type-5 for cluster-4")
########################################################4
par(mfrow=c(3,2))
hist(personality.data_k5$count_1,main="personality type-1 for cluster-5")
hist(personality.data_k5$count_2,main="personality type-2 for cluster-5")
hist(personality.data_k5$count_3,main="personality type-3 for cluster-5")
hist(personality.data_k5$count_4,main="personality type-4 for cluster-5")
hist(personality.data_k5$count_5,main="personality type-5 for cluster-5")
##########################################################5

######################Second Method
########### Training and Testing dataset preparation #######################

df <- a

dim(df)

n <- nrow(df)
ntrain <- round(n*0.20)  
tindex <- sample(n, ntrain)   

# Train 
df1 <- df[tindex,]
dim(df1)  

# remaining data
df2 <- df[-tindex,]
dim(df2)

###################### part 1 ##############################

# use dataset df1

# K-means
x <- as.data.frame(scale(df1))
names(x)

k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(x, k, nstart=50,iter.max = 15 )$tot.withinss})
wss


plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


data_Cluster <- kmeans(x, 5, nstart = 20)
data_Cluster

cluster_data<-df1
cluster_data$cluster<-data_Cluster$cluster


clusplot(cluster_data, cluster_data$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)


cluster_data_km<-cluster_data
cluster_data_km$cluster<-as.factor(cluster_data_km$cluster)

tree.df1_km<- tree(cluster~., data=cluster_data_km)
plot(tree.df1_km)
text(tree.df1_km ,pretty =0)


# Hierarchical

X = df1
cc<-scale(X)

fit = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
plot(fit)

# draw dendogram with red borders around the 8 clusters
rect.hclust(fit, k=5, border="red")

groups <- cutree(fit, k=5) # cut tree into 8 clusters

cluster_data<-df1
cluster_data$cluster<-groups


clusplot(cluster_data, cluster_data$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)


cluster_data_hc<-cluster_data
cluster_data_hc$cluster<-as.factor(cluster_data_hc$cluster)

tree.df1_hc<- tree(cluster~., data=cluster_data_hc)
plot(tree.df1_hc)
text(tree.df1_hc ,pretty =0)



########################## Part 2 #################################

# prediction 

# use remaining dataset i.e df2

dim(df2)

# sample and select few from remaining dataset i.e df2
n <- nrow(df2)  
ntrain <- round(n*0.001)  
tindex <- sample(n, ntrain)   


df_prediction_set <- df2[tindex,]
dim(df_prediction_set)


# K-means
x <- as.data.frame(scale(df_prediction_set))
names(x)

data_Cluster <- kmeans(x, 5, nstart = 20)
data_Cluster

cluster_data_pred_set<-df_prediction_set
cluster_data_pred_set$cluster<-data_Cluster$cluster


clusplot(cluster_data_pred_set, cluster_data_pred_set$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)


cluster_data_pred_set_km<-cluster_data_pred_set
cluster_data_pred_set_km$cluster<-as.factor(cluster_data_pred_set_km$cluster)

ypred_km <- predict(tree.df1_km ,cluster_data_pred_set_km,type = "class")

cm = as.matrix(table(ypred_km ,cluster_data_pred_set_km$cluster)) # create the confusion matrix
cm

n = sum(cm)
diag = diag(cm)
accuracy = sum(diag) / n 
accuracy



# Hierarchical

X = df_prediction_set
cc<-scale(df_prediction_set)

fit = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
plot(fit)

# draw dendogram with red borders around the 8 clusters
rect.hclust(fit, k=5, border="red")

groups <- cutree(fit, k=5) # cut tree into 8 clusters

cluster_data_pred_set<-df_prediction_set
cluster_data_pred_set$cluster<-groups


clusplot(cluster_data_pred_set, cluster_data_pred_set$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels=2, lines=0)


cluster_data_pred_set_hc<-cluster_data_pred_set
cluster_data_pred_set_hc$cluster<-as.factor(cluster_data_pred_set_hc$cluster)

ypred_hc <- predict(tree.df1_hc ,cluster_data_pred_set_hc,type="class")

cm = as.matrix(table(ypred_hc ,cluster_data_pred_set_hc$cluster)) # create the confusion matrix
cm

n = sum(cm)
diag = diag(cm)
accuracy = sum(diag) / n 
accuracy



##########################################################################

#                           @@@@@@@@@@@@@@@@@@@@@@@@

#########################################################################