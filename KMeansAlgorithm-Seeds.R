#setwd("~/Github/Introtoml")
#install.packages("ggplot2")
#install.packages("cluster")
library(ggplot2)
library(cluster)
dataframe <- read.table("seeds_dataset.txt", header = FALSE)
colnames(dataframe)<- c("Area","Perimeter", "Compactness", "length of kernel","width of kernel","asymmetry coefficient","length of kernel groove","KRC")
print(str(dataframe))
#convert NSP into factor variable
dataframe$KRC <- as.factor(dataframe$KRC)
print(str(dataframe))
ggplot(dataframe, aes(x = Area, y = Perimeter)) + geom_point(aes(color=KRC))

set.seed(10)
wheatSeedCluster <- kmeans(dataframe, 3, nstart = 20)

print(wheatSeedCluster)

print(table(wheatSeedCluster$cluster))

clusplot(dataframe,wheatSeedCluster$cluster, 
         shade = TRUE, labels = 3, lines = 0, 
         main = "2D plot")


str(wheatSeedCluster)
wss <- function(k) {kmeans(dataframe, k, nstart = 10 )$tot.withinss}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:15
library(purrr)
wss_values <- map_dbl(k.values, wss)
plot(k.values, wss_values, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")
