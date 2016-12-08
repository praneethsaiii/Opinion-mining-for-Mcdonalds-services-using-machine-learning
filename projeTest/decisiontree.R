library(party)
df<-read.csv("sentimentdata.csv")
data<-data.frame(df)
library(Amelia)
missmap(data, main = "Missing values vs observed")
str(data)
pd=sample(2,nrow(data),replace=TRUE,prob=c(0.9,0.1))
train=data[pd==1,]
test=data[pd==2,]
dctree <- ctree((polarity ~ emotion) , data=train)
pre=predict(dctree,test)
pre
t=pre==test$polarity
table(t)
print(dctree)
plot(dctree)
plot(pre)
table(pre)
accuracy1=mean(pre == test[ , 3])*100 
print("Round Accuracy:")
print(accuracy1)
write.csv(pre,file="decpre.csv")


pctree <- ctree((emotion ~ polarity) , data=train)
pre=predict(pctree,test)
pre
t=pre==test$emotion
table(t)
print(pctree)
plot(pctree)
accuracy1=mean(pre == test[ , 2])*100 
accuracy=accuracy+accuracy1
print("Round Accuracy:")
print(accuracy1)
write.csv(pre,file="decpre.csv",append=TRUE)