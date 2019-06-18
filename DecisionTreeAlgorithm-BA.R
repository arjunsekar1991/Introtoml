#machine learning algorithm decision tree on Bank Note Authentication
setwd("~/Github/Introtoml")
#install.packages("rpart")
library(rpart)
#install.packages("rattle")
library(rattle)
#install.packages('caret', dependencies = TRUE)
library(caret)


dataframe <- read.csv("data_banknote_authentication.txt", header = FALSE)
colnames(dataframe)<- c("Variance","Skewness", "Curtosis", "Entropy","Class")

dataframe$Class<-as.factor(dataframe$Class)
splitData <- sample(2, nrow(dataframe), replace = TRUE, prob = c(0.7,0.3))
trainingData <- dataframe[splitData==1,]
testData <- dataframe[splitData==2,]



trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(100)
tree.train = train(Class~., method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10,trainingData)
fancyRpartPlot(tree.train$finalModel)

#prediction for training
tree.predict1 <-predict(tree.train, trainingData) 
print(tree.predict1)
confusionMatrix(tree.predict1, trainingData$Class)

#prediction for testData
tree.predict2 <- predict(tree.train, testData)
confusionMatrix(tree.predict2, testData$Class)
