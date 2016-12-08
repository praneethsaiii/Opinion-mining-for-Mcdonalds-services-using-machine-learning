df<-read.csv("sentimentanalysis.csv")
data<-data.frame(df)
library(randomForest)
train=data[1:1200,]
test=data[1201:1525,]
fit <- randomForest(polarity ~ emotion,   data=test)
print(fit) # view results 
importance(fit)
plot(fit)
summary(fit)


library(ROCR)
pred <- predict(fit,newdata=test)
perf <- performance(pred,"prec","rec")

plot(perf, avg= "threshold", colorize=T, lwd= 3,
     main= "... Precision/Recall graphs ...")
plot(perf, lty=3, col="grey78", add=T)



