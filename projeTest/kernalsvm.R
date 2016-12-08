library(e1071)
data <- read.csv("sentimentanalysis.csv")
str(data)
train <- data[1:1200, ]
test <- data[1201:1525, ]
library(kernlab)
letter_classifier <- ksvm(emotion ~ ., data = train,
                          kernel = "vanilladot")
print(letter_classifier)
class(letter_classifier)
plot(letter_classifier)
letter_predictions <- predict(letter_classifier, test)
head(letter_predictions)
table(letter_predictions, test$emotion)
agreement <- letter_predictions == test$emotion
table(agreement)
prop.table(table(agreement))
barplot(table(letter_predictions))
accuracy1=mean(agreement)*100 
print("Round Accuracy:")
print(accuracy1)


letter_classifier <- ksvm(polarity ~ ., data = train,
                          kernel = "vanilladot")
letter_classifier
letter_predictions <- predict(letter_classifier, test)
head(letter_predictions)
table(letter_predictions, test$polarity)

agreement <- letter_predictions ==test$polarity
table(agreement)
prop.table(table(agreement))
barplot(table(letter_predictions))

accuracy1=mean(agreement)*100 
accuracy=accuracy+accuracy1
print("Round Accuracy:")
print(accuracy1)
