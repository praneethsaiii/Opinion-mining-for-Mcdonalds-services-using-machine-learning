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
pima_data<-read.csv("sentimentanalysis.csv")
names(pima_data)<-c("x", "y", "z", "p","s","review","emotion","polarity")
str(pima_data)
table(pima_data$polarity)
normalize <- function(x)
{
  return((x-min(x))/max(x)-min(x))
}
normalize(c(1,2,3,4,5))
set.seed(9850)
pima_n = as.matrix(pima_data)
pima_n <- as.data.frame(lapply(pima_data[,c(1,2,3,4)], normalize))
df_train <- pima_n[1:1300, ]
df_test <- pima_n[1301:1513, ]
df_trainlab <- pima_data[1:1300, 5]
df_testlab <- pima_data[1301:1513, 5]
pima_pred <- knn(train = df_train, test = df_test, cl=df_trainlab, k=39)
CrossTable( x=df_testlab, y=pima_pred, prop.chisq=FALSE)


2.

library(tm) # Text mining: Corpus and Document Term Matrix

#library(class) # KNN model

#library(SnowballC) # Stemming words
library(FNN)
library(wordcloud)
#Read csv with two columns: text and category
library(ggplot2)

df <- read.csv("sentimentanalysis.csv", sep =",", header = TRUE)
ctg <- df$polarity
ctg[-1000]

# Create corpus
docs <- Corpus(VectorSource(df$review))

# Clean corpus
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))


dtm <- DocumentTermMatrix(docs)


mat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)
mat.df

mat.df <- cbind(mat.df, df$review)
colnames(mat.df)[ncol(mat.df)] <- "review"

mat.df
colnames(mat.df)[ncol(mat.df)] <- "review"
cl <- mat.df[, "review"]
modeldata <- mat.df[,!colnames(mat.df) %in% "review"]

modeldataTest <- modeldata[1300,]
modeldataTrain<- modeldata[-1300,]


kdist <- knnx.dist(modeldataTrain,modeldataTest,k=38)
kdist
plot(kdist)
pred <- knn(modeldataTrain,modeldataTest,cl[-1300],k=38)
summary(pred)
plot(pred,kdist)
write.csv(kdist,file="knn.csv")


