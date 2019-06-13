#machine learning algorithm decision tree on Bank Note Authentication
setwd("~/Github/Introtoml")
#install.packages("rpart")
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages('caret', dependencies = TRUE)
library(caret)
#install.packages("rpart.plot")


library(rpart.plot)


dataframe <- read.csv("data_banknote_authentication.txt", header = FALSE)
colnames(dataframe)<- c("Variance","Skewness", "Curtosis", "Entropy","Class")

dataframe$Class<-as.factor(dataframe$Class)
splitData <- sample(2, nrow(dataframe), replace = TRUE, prob = c(0.7,0.3))
trainingData <- dataframe[splitData==1,]
testData <- dataframe[splitData==2,]

tree.train = rpart(Class~., data=trainingData)
fancyRpartPlot(tree.train)

#prediction for training
tree.predict1 <-predict(tree.train, trainingData, type="class") 
print(tree.predict1)
confusionMatrix(tree.predict1, trainingData$Class)

#prediction for testData
tree.predict2 <- predict(tree.train, testData, type="class")
confusionMatrix(tree.predict2, testData$Class)
