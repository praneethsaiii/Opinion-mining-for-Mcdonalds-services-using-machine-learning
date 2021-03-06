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
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE) 
container = create_container(matrix, as.numeric(as.factor(data[,2])),
                             trainSize=1:1000, testSize=1001:1513,virgin=FALSE)
models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))
results = classify_models(container, models)
table(as.numeric(as.factor(data[1001:1513, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(data[1001:1513, 2])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(data[1001:1513, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(data[1001:1513, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(data[1001:1513, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(data[1001:1513, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(data[1001:1513, 2])), results[,"SVM_LABEL"])
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")