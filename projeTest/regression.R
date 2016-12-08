library(VGAM)
# load data
data<-read.csv("sentimentanalysis.csv",header=T,na.strings=c(""))
sapply(data,function(x) sum(is.na(x)))
sapply(data, function(x) length(unique(x)))
library(Amelia)
missmap(data, main = "Missing values vs observed")
df<- subset(data,select=c(6,7))
#df$policies_violated[is.na(df$policies_violated)] <- mean(df$policies_violated,na.rm=T)
#df$policies_violated.confidence[is.na(df$policies_violated.confidence)] <- mean(df$policies_violated.confidence,na.rm=T)
is.factor(df$emotion)
is.factor(df$polarity)
contrasts(df$emotion)
contrasts(df$polarity)
data <- df[!is.na(df$polarity),]
rownames(df) <- NULL
train <- df[1:1300,]
test <- df[1301:1510,]
model <- glm(polarity ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
plot(model)
library(pscl)
pR2(model)
#df2<- subset(test,select=c(6,7))
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$polarity)
print(paste('Accuracy',misClasificError))
plot(fitted.results)
barplot(table(fitted.results))


