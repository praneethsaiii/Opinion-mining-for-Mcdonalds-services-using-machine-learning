library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(SnowballC)
library(tm)
mydata<-readLines("tripnewyork1.txt")
df <- data.frame(mydata)
myCorpus<-Corpus(VectorSource(df$mydata))

mycorpus<-tm_map(myCorpus,PlainTextDocument)

mycorpus<-tm_map(myCorpus,removePunctuation)

mycorpus<-tm_map(myCorpus,removeWords,stopwords(kind = "en"))

mycorpus<-tm_map(myCorpus,stemDocument)
mycorpus<-tm_map(myCorpus,removeNumbers)

dataframe<-data.frame(text=unlist(sapply(mycorpus, `[`, "review")), 
                      stringsAsFactors=F)
write.csv(dataframe,file="pretanewyork1.csv")


