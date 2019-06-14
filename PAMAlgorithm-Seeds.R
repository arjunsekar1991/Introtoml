#https://www.datanovia.com/en/lessons/k-medoids-in-r-algorithm-and-practical-examples/
#https://github.com/archowdhury/Partitioning-Around-Medoids-PAM-using-R/blob/master/PAM%20on%20Iris%20dataset.R
setwd("~/Github/Introtoml")
dataframe <- read.table("seeds_dataset.txt", header = FALSE)
colnames(dataframe)<- c("Area","Perimeter", "Compactness", "length of kernel","width of kernel","asymmetry coefficient","length of kernel groove","KRC")
print(str(dataframe))
#convert NSP into factor variable
dataframe$KRC <- as.factor(dataframe$KRC)

install.packages(c("cluster", "factoextra"))
library(cluster)
library(factoextra)
pam2.res <- pam(dataframe, 3,metric = "euclidean", stand = FALSE)
print(pam2.res)
clusplot(pam2.res, main = "Cluster plot, k = 3", 
         color = TRUE)

library(factoextra)
gower_dist = daisy(dataframe,metric="gower")

summary(gower_dist)

gower_mat = as.matrix(gower_dist)

sil_width = c(NA)

for(i in 2:10)
{
  pam_fit = pam(gower_dist, diss = TRUE, k=i)
  sil_width[i] = pam_fit$silinfo$avg.width
}

plot(1:10,sil_width,xlab="Number of Clusters",ylab="Silhouette width")
lines(1:10, sil_width)

