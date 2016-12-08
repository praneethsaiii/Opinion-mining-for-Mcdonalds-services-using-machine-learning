library(plyr)
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
lapply(libs,require,character.only=TRUE)
options(stringsAsFactors = FALSE)
candidates<-c("review")
pathname<-"test.csv"
cleancorpus<-function(corpus)
  {
  corpus.tmp<-tm_map(corpus,removePunctuation)
  corpus.tmp<-tm_map(corpus.tmp,stripWhitespace)
corpus.tmp<-tm_map(corpus.tmp,tolower)
corpus.tmp<-tm_map(corpus.tmp,removeWords,stopwords("english"))
return(corpus.tmp)
}
generateTDM <- function(cand,path){
  s.dir <- sprintf("%s/%s",path,cand)
  s.cor <- Corpus(DirSource(directory = s.dir))
  s.cor.cl <- cleancorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm,0.7)
  result <- list(name=cand,tdm=s.tdm)
  
}
tdm <- lapply(candidates,generateTDM,path= pathname)
bindCandidateToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat,stringAsFactors=FALSE)
  s.df <- cbind(s.df,rep(tdm[["review"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}
candTDM <- lapply(tdm,bindCandidateToTDM)
tdm.stack <- do.call(rbind.fill,candTDM)
tdm.stack[is.na(tdm.stack)] <- 0
head(tdm.stack)
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) = 0.7))
test.idx <- (1:nrow(tdm.stack))
head(test.idx)
head(train.idx)
tdm.cand <- tdm.stack[,"targetcandidate"]
tdm.stack.nl <-tdm.stack[,colnames(tdm.stack)%in% "targetcandidate"]
knn.pred <- knn(tdm.stack.nl[train.idx, ],tdm.stack.nl[test.idx, ],tdm.cand[train.idx])
conf.mat <- table("predictions" = knn.pred, Actual= tdm.cand[test.idx])
(accuracy <- sum(diag(conf.mat))/length(test.idx) *100)


2

library(rpart)
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
df <- data.frame(data)
textdata <- df[df$data, ]
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:punct:]]", "", textdata)
textdata = gsub("[[:digit:]]", "", textdata)
textdata = gsub("http\\w+", "", textdata)
textdata = gsub("[ \t]{2,}", "", textdata)
textdata = gsub("^\\s+|\\s+$", "", textdata)
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}
textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]
names(textdata) = NULL
pima_n <- as.data.frame(lapply(data[,c(1,2,3,4)],try.error))
x_train <- pima_n[1:1000, ]
x_test <- pima_n[1001, ]
x <- cbind(x_train,x_test)
# grow tree 
fit <- rpart(x_test , data = x,method="class")
summary(fit)
#Predict Output 
predicted= predict(fit,x_test)

3
