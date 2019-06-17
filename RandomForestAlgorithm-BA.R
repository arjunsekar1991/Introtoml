#machine learning algorithm Random forest on Bank Note Authentication
setwd("~/Github/Introtoml")
#install.packages("randomForest")
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("caret", dependencies = TRUE)
library(randomForest)
#install.packages("rfUtilities")
library("rfUtilities")
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
rf.cv <- rf.crossValidation(rfModel, trainingData, p=0.10, n=99, ntree=501)
#Prediction and Confusion Matrix for training data

par(mfrow=c(1,2)) 
plot(rf.cv, type = "cv", main = "CV producers accuracy")
plot(rf.cv, type = "model", main = "Model producers accuracy")

par(mfrow=c(1,2)) 
plot(rf.cv, type = "cv", stat = "oob", main = "CV oob error")
plot(rf.cv, type = "model", stat = "oob", main = "Model oob error")	
p1 <- predict(rfModel, trainingData)
print(head(p1))
confusionMatrix(p1, trainingData$Class)


#Prediction & confustion matrix - test data

p2 <- predict(rfModel, testData)
confusionMatrix(p2, testData$Class)

#error rate of random forest

plot(rfModel)
