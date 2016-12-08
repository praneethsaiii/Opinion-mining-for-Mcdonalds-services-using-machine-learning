library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(sentiment)
library(SnowballC)
library(tm)
mydata<-read.csv("test.csv")

myCorpus<-Corpus(VectorSource(mydata$review))

mycorpus<-tm_map(myCorpus,PlainTextDocument)

mycorpus<-tm_map(myCorpus,removePunctuation)

mycorpus<-tm_map(myCorpus,removeWords,stopwords(kind = "en"))

mycorpus<-tm_map(myCorpus,stemDocument)

dataframe<-data.frame(text=unlist(sapply(mycorpus, `[`, "content")), 
                      stringsAsFactors=F)
write.csv(dataframe,file="preprocessed.csv")
pal<-brewer.pal(8,"Dark2")

wordcloud(mycorpus,min.freq = 3, max.words = Inf,width=100, height=100, random.order = FALSE,colors = pal)

