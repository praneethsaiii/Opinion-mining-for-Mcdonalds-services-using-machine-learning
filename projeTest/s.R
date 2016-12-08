library(e1071)
library(MASS)
data<-read.csv("sentimentanalysis.csv")
plot(data)

plot(data$emotion ,data$policies_violated,col=data$polarity)
s<-sample(1525,1200)
col<-c("emotion","policies_violated","polarity")
train<-data[s,col]
test<-data[-s,col]
svmfit<-svm(polarity~.,data=train,kernal="linear",cost=.1,scale=FALSE)
print(svmfit)
plot(svmfit,train[,col])

plot(iris)
plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)
s<-sample(150,100)
col<-c("Petal.Length","Petal.Width","Species")
train<-iris[s,col]
test<-iris[-s,col]
svm<-svm(Species~.,data=train,kernal="linear",cost=.1,scale=FALSE)
print(svm)
plot(svm,train[,col])

