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
data<-read.csv("test.csv")
matrix= create_matrix(data$review, language="english", 
                      removeStopwords=TRUE, removeNumbers=TRUE, 
                      stemWords=TRUE) 
container = create_container(matrix, as.numeric(as.factor(data[,1])),
                             trainSize=1:1300, testSize=1301:1513,virgin=FALSE)
models = train_models(container, algorithms="SVM")
results = classify_models(container, models)
results
table(as.numeric(as.factor(data[1301:1513, 2])), results[,"SVM_LABEL"])
plot(results)




recall_accuracy(as.numeric(as.factor(data[1301:1513, 2])), results[,"SVM_LABEL"])
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
N=4
set.seed(2014)

cross_validate(container,N,"SVM")
