##to bind different dataset rows together
newdata <- bind_rows(file1, file2, file3) ## load dplyr
boston <- read.csv(file = 'C:/Users/Khalid/Desktop/Data Science/bostonhousing.csv')
install.packages("dbscan")
library(dbscan)
library(mclust)
library(fpc)
library(factoextra)
summary(boston)
##setting data on linear scale to consider diff in values equally important since data ranges 0 to 397
boston_scaled <- LinearScaling(boston, mx = max(boston, na.rm = T), mn = min(boston, na.rm = T))
boston_scale <- scale(boston)
##reducing the feature dimension
boston_pca <- prcomp(boston, scale = TRUE)
##checking the PC
boston_pca$x[1:5,1:6]
## checking the proportion of variation in dataset
screeplot(boston_pca) ##shows first 1 or 2 component dominates and enough to summarise d data
boston_clusters <- kmeans(boston_pca$x[,1:2], centers = 6, nstart = 20)
print(boston_clusters)
boston_clusters$newcluster <- as.factor(boston_clusters$cluster)
boston_plot <- silhouette(boston_clusters$cluster, dist(boston_pca$x[,1:2]))
fviz_silhouette(boston_plot)
fviz_cluster(boston_plot)
str(boston_clusters)
##explaining variance in PCA
var_explained <- boston_pca$sdev^2/sum(boston_pca$sdev^2)
var_explained[1:5]
## function for calculating sum of sqr within grp to determine optimal k
wssplot <- function(data, nc=15){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}

wssplot(boston_pca$x[,1:2], nc=20)

### using EM clustering which doesn't require user to enter cluster number arbitrarily
boston_EM <- Mclust(boston_pca$x[,1:2])
plot(boston_EM)
summary(boston_EM)
### using DBSCAN clustering which also doesn't require user to enter cluster number like Kmean
boston_dbscan <- dbscan(boston_pca$x[,1:2], eps=2, MinPts = 25, scale = FALSE)
## low minPts means it will build more clusters from noise, so not a good idea
plot(boston_dbscan$cluster)
##getting the matrix
boston_matrix <- as.matrix(boston_pca$x[,1:2])
## Get a matrix with distances to the 1st, 2nd, ... NN.
kNNdist(boston_matrix, k=2, all = TRUE)
## Produce a k-NN distance plot to determine a suitable eps for
## DBSCAN (in the plot,the knee that is eps is around a distance of .5)
kNNdistplot(boston_matrix, k=2)
boston_dbscan <- dbscan(boston_pca$x[,1:2], eps=0.5, minPts = 2)
pairs(boston_pca$x[,1:2], col = boston_dbscan$cluster+1L)
### using hierarchical clustering
##first get the distance matrix
boston_hierarch <- dist(boston_matrix)
boston_hierarchy <- hclust(boston_hierarch, method = 'single')
plot(boston_hierarchy)
cut_boston <- cutree(boston_hierarchy, k=2)
plot(cut_boston)
##to visualise the clusters, use abline
rect.hclust(boston_hierarchy, k = 2, border = 2:6)
abline(h=2, col='red')
##or without pca
boston_hierarch2 <- dist(as.matrix(boston_scale))
boston_hierarchy2 <- hclust(boston_hierarch2, method = 'single')
plot(boston_hierarchy2)
cut_boston <- cutree(boston_hierarchy2, k=14)
rect.hclust(boston_hierarchy, k = 14, border = 14:36)
abline(h=14, col='red')
