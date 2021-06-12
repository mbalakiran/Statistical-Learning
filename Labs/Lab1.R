library(ISLR)
library(MASS)
library(car)
data(Auto)
l1<-lm(mpg~horsepower,data=Auto)
summary(l1)
## degrees of freedom = number of observations - number of parameters
predict(l1,newdata=data.frame(horsepower=98))
predict(l1,newdata=data.frame(horsepower=98),interval="confidence")
predict(l1,newdata=data.frame(horsepower=98),interval="predict")
cf1<-coef(l1)
plot(mpg~horsepower,data=Auto)
abline(a=cf1[1],b=cf1[2])
plot(l1)
h1<-hatvalues(l1)
plot(h1)
abline(a=2*2/nrow(Auto),b=0)
plot(Auto[,-9])
cor(Auto[,-9])
## Any correlation between two independent variables exceeding 0.8 is dangerous!
l2<-lm(mpg~.,data=Auto[,-9])
summary(l2)
vif(l2)
#in R if you write lm(y~x1*x2) then it means
# lm(y~x1+x2+x1:x2)
l3<-lm(mpg~factor(cylinders)*displacement+horsepower+weight+acceleration+year+
       factor(origin)+horsepower:weight,data=Auto)
summary(l3)
anova(l3)
l4<-lm(mpg~factor(cylinders)*I(log(displacement))+I(sqrt(horsepower))+weight+acceleration+year+
         factor(origin)+I(sqrt(horsepower)):weight,data=Auto)
summary(l4)
## Ex. 13
set.seed(1)
x<-rnorm(100)
e<-rnorm(100,sd=sqrt(0.25))
y<--1+0.5*x+e
length(y)
#b_0=-1, b_1=0.5
plot(y~x)
l5<-lm(y~x)
summary(l5)
