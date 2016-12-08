#library("twitteR")
#library("tm")
#library("RColorBrewer")
#library(ROAuth)
#library(plyr)
#library(dplyr)
#library(stringr)
#library(ggplot2)
#library(streamR)
#library(httr)




  if (file.exists(paste(searchterm, '_table.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_table.csv'), row.names=F)
  
  
  table <- read.csv(file=paste(searchterm, '_table.csv'))
  table <- rbind(table, df)
  table <- subset(table, !duplicated(table$text))
  write.csv(table, file=paste(searchterm, '_table.csv'), row.names=F)
  
  
  
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    scores <- laply(sentences, function(sentence, pos.words, neg.words){
      sentence <- gsub('[[:punct:]]', "", sentence)
      sentence <- gsub('[[:cntrl:]]', "", sentence)
      sentence <- gsub('\\d+', "", sentence)
      sentence <- iconv(sentence,"utf-8","ASCII",sub="")
      sentence <- tolower(sentence)
      word.list <- str_split(sentence, '\\s+')
      words <- unlist(word.list)
      pos.matches <- match(words, pos.words)
      neg.matches <- match(words, neg.words)
      pos.matches <- !is.na(pos.matches)
      neg.matches <- !is.na(neg.matches)
      score <- sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words, .progress=.progress)
    scores.df <- data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  pos <- scan('working directory/positive dictionary', what='character', comment.char=';') 
  neg <- scan('working directory/negative dictionary', what='character', comment.char=';') 
  pos.words <- c(pos, 'kudos')
  neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfail','jerk','doesn\'t','screwed')
  
  Dataset <- table
  Dataset$review <- as.factor(Dataset$review)
  scores <- score.sentiment(Dataset$review, pos.words, neg.words, .progress='text')
  write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #write results to file
  
  
  stat <- scores
  stat$created <- table$created
  stat$created <- as.Date(stat$created)
  stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
  by.tweet <- group_by(stat, tweet, created)
  by.tweet <- summarise(by.tweet, number=n())
  write.csv(by.tweet, file=paste(searchterm, '_sntmnt.csv'), row.names=TRUE)
  
  #line graph
  ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=2) +
    geom_point(aes(group=tweet, color=tweet), size=4) +
    theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1)) +
    ggtitle(searchterm)
  
  ggsave(file=paste(searchterm, '_graph.jpeg'))
  

