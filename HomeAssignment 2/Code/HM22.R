rm(list = ls())
library(MASS)
library(ISLR)
library(readxl)
library(dplyr)
library(tree)
library(e1071)


setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 2")
# Reading excel file as dtaframe
df <-read_excel("Data_Cortex_Nuclear.xls")

# Removing the index
df <- df[,c(2:82)]

# Replacing null values
for(i in 1:77){
  df[is.na(df[,i]), i] <- min(df[,i], na.rm = TRUE)
}
# K-means


# Determine number of clusters
wss <- (nrow(df[,-c(78:81)])-1)*sum(apply(df[,-c(78:81)],2,var))
for (i in 1:15) wss[i] <- sum(kmeans(df[,-c(78:81)],
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


data_Cluster <- kmeans(df[,-c(78:81)], 2, nstart = 20)
data_Cluster

cluster_data<-df
cluster_data<-cbind(cluster_data,data_Cluster$cluster)

table(data_Cluster$cluster, cluster_data$Treatment)
table(data_Cluster$cluster, cluster_data$Behavior)
table(data_Cluster$cluster, cluster_data$Genotype)


# hieracial

# Ward Hierarchical Clustering
d <- dist(df[,-c(78:81)], method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2")
plot(fit) # display dendogram

# draw dendogram with red borders around the 8 clusters
rect.hclust(fit, k=8, border="red")

groups <- cutree(fit, k=8) # cut tree into 8 clusters

cluster_data<-df
cluster_data<-cbind(cluster_data,groups)

table(cluster_data$groups, cluster_data$Genotype)



#################################2 method

rm(list = ls())
dev.off()
loadPackages <- function(package0){
  package1 <- package0[!(package0 %in% installed.packages()[, "Package"])]
  if (length(package1))
    install.packages(package1, dependencies = TRUE)
  sapply(package0, require, character.only = TRUE)
}

Packages <- c("GGally","car", "MASS","gplots","rcompanion","outliers",
              "tseries",                                    
              "corrplot", "RColorBrewer",                   
              "dplyr","ggplot2") 
loadPackages(Packages)


setwd("~/Master Program/Statistical Learning/Assignments/HomeAssignment 2")
# Reading excel file as dtaframe
rat_data <-read_excel("Data_Cortex_Nuclear.xls")
rat_data2<- read_excel("Data_Cortex_Nuclear.xls")
summary(rat_data2)
rat_data$class<- as.factor(rat_data$class)
rat_data$Genotype <- as.factor(rat_data$Genotype)
sum(is.na(rat_data))

ncol(rat_data)


for(i in 2:78){
  
  
  d<-rat_data[i]
  d<-na.omit(d)
  
  if(sum(is.na(rat_data[i]))>0){
    
    rat_data[i][is.na(rat_data[i])] <- sum(d)/nrow(d)
    
  }
  
  
  
  
}
for(i in 2:78){
  
  
  d<-rat_data[i]
  d<-na.omit(d)
  
  if(sum(is.na(rat_data2[i]))>0){
    
    rat_data2[i][is.na(rat_data2[i])] <- sum(d)/nrow(d)
    
  }
  
  
  
  
}
dim(rat_data)
sum(is.na(rat_data2))

X = rat_data[2:78]
cc<-scale(X)
dendrogram = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
plot(dendrogram)


n<-as.dendrogram(dendrogram)
nodePar<-list(lab.cex=0.9,pch=c(10,19),cex=0.7,col=c("green","yellow"))
plot(n,xlab="Height",nodePar=nodePar, main="cluster",edgePar=list(col=c("red","blue"),lwd=2:1),horiz=TRUE)




hc = hclust(d = dist(cc, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 6)


library(cluster)
clusplot(X,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE)

###############kmeans
library(factoextra)
cc<-scale(X)
distance <- get_dist(cc)

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


k2 <- kmeans(cc, centers = 8, nstart = 25)
str(k2)
fviz_cluster(k2, data = cc)
x
rat_data$cluster<-k2$cluster
View(rat_data)

table(rat_data$cluster, rat_data$class)

Qq1 <- subset(rat_data,cluster==1)
Qq2 <- subset(rat_data,cluster==2)
Qq3 <- subset(rat_data,cluster==3)
Qq4 <- subset(rat_data,cluster==4)
Qq5<- subset(rat_data,cluster==5)
Qq6 <- subset(rat_data,cluster==6)
Qq7 <- subset(rat_data,cluster==7)
Qq8 <- subset(rat_data,cluster==8)
sum(is.na(Qq1))
par(mfrow=c(3,3))
plot(as.factor(Qq1$class),main="K-means cluster-1")
plot(as.factor(Qq2$class),main="K-means cluster-2")
plot(as.factor(Qq3$class),main="K-means cluster-3")

plot(Qq4$class,main="K-means cluster-4")
plot(Qq5$class,main="K-means cluster-5")
plot(Qq6$class,main="K-means cluster-6")
plot(Qq7$class,main="K-means cluster-7")
plot(Qq8$class,main="K-means cluster-8")

#####2
Y <- rat_data[2:78]
cc<-scale(Y)
distance <- get_dist(cc)

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


k2 <- kmeans(cc, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = cc)
x
rat_data$cluster<-k2$cluster
View(rat_data)

table(rat_data$cluster, rat_data$Genotype)

Qq1 <- subset(rat_data,cluster==1)
Qq2 <- subset(rat_data,cluster==2)
sum(is.na(Qq1))
par(mfrow=c(3,3))
plot(as.factor(Qq1$Genotype),main="K-means cluster-1")
plot(as.factor(Qq2$Genotype),main="K-means cluster-2")


table(rat_data$cluster, rat_data$Behavior)
table(rat_data$cluster, rat_data$Treatment)

Qq1 <- subset(rat_data,cluster==1)
Qq2 <- subset(rat_data,cluster==2)
sum(is.na(Qq1))
par(mfrow=c(3,3))
plot(as.factor(Qq1$Genotype),main="K-means cluster-1")
plot(as.factor(Qq2$Genotype),main="K-means cluster-2")

