#machine learning algorithm Random forest on Bank Note Authentication
setwd("~/Github/Introtoml")
#install.packages("randomForest")
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("caret", dependencies = TRUE)
library(randomForest)
library(caret)


dataframe <- read.csv("data_banknote_authentication.txt", header = FALSE)
colnames(dataframe)<- c("Variance","Skewness", "Curtosis", "Entropy","Class")
dataframe$Class<-as.factor(dataframe$Class)
splitData <- sample(2, nrow(dataframe), replace = TRUE, prob = c(0.7,0.3))
trainingData <- dataframe[splitData==1,]
testData <- dataframe[splitData==2,]


set.seed(222)

rfModel <- randomForest(Class ~ ., data=trainingData)
print(rfModel)
print(attributes(rfModel))

#Prediction and Confusion Matrix for training data

p1 <- predict(rfModel, trainingData)
print(head(p1))
confusionMatrix(p1, trainingData$Class)


#Prediction & confustion matrix - test data

p2 <- predict(rfModel, testData)
confusionMatrix(p2, testData$Class)

#error rate of random forest

plot(rfModel)
