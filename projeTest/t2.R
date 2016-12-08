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
library(wordcloud)
library(RColorBrewer)
data<-read.csv("test.csv")
summary(data)
pos_words=scan("positive-words.txt",what="character",comment.char = ";")
neg_words=scan("negative-words.txt",what="character",comment.char = ";")
function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub("[[:digit:]]", "", sentence)
    sentence = gsub("http\\w+", "", sentence)
    sentence = gsub("[ \t]{2,}", "", sentence)
    sentence = gsub("^\\s+|\\s+$", "", sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    neg.matches = match(words, neg.words)
    pos.matches = match(words, pos.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=sentences)
  hist(scores.df)
  return(scores.df)
  barplot(scores.df)
}
data.scores=score.sentiment(data$review,pos_words,neg_words,.progress = 'text')
print(data.scores)
plot.default(data.scores)
write.csv(data.scores,file="posneg.csv")


