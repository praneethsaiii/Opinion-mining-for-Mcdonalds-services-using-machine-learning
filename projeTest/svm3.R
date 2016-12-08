library(tm)
library(NLP)
library(e1071)
library(Rstem)
library(ggplot2)
library(RCurl)
library(ggmap)
library(xml2)
library(sentiment)
library(RTextTools)
library(RColorBrewer)
library(class)
library(gmodels)
library(MASS)
data<-read.csv("sentimentanalysis.csv")
dtMatrix <- create_matrix(data["review"])
container <- create_container(dtMatrix, data$polarity, trainSize=1:1300,testSize=1301:1513, virgin=FALSE)


# train a SVM Model
model <- train_model(container, algorithm = "SVM")
predictionData <- dtMatrix[1:1300,]
predMatrix <- create_matrix(predictionData)
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)
results
plot(results)

2>
  
  dataset<-read.csv("sentimentanalysis.csv")
index <- 1:nrow(dataset)

testindex <- sample(index, trunc(length(index)*30/100))

testset <- dataset[1:1300,]

trainset <- dataset[1301:1525,]
names(dataset)
#cl <- mat.df[, "polarity"]
tuned <- tune.svm(polarity~., data = trainset, gamma = 10^(-6:-1), cost = 10^(-1:1))
summary(tuned)
model  <- svm(polarity~., data = trainset, kernel = "radial", gamma = 0.001, cost = 10) 
summary(model)
prediction <- predict(model, testset[,-7])
tab <- table(pred = prediction, true = testset[,7])
tab
barplot(tab)

