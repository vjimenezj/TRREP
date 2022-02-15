###################################################
######### Comparing clustering algorithms #########
###################################################

# Installing required packages
if (!require("MASS")) install.packages("MASS")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("factoextra")) install.packages("factoextra")
if (!require("ppclust")) install.packages("ppclust")
if (!require("dbscan")) install.packages("dbscan")

library(MASS)
library(ggplot2)
library(ggrepel)
library(factoextra)
library(ppclust)
library(dbscan)


# Simulating three bivariate normal clusters
N<-20
mu1 <- c(0,0)
sigma1 <- matrix(nrow=2,c(1,0,0,1))
mu2 <- c(4,0)
sigma2 <- matrix(nrow=2,c(1,0,0,1))
mu3 <- c(5,5)
sigma3 <- matrix(nrow=2,c(1,-0.5,-0.5,1))
set.seed(123)
cluster1 <- data.frame(mvrnorm(N, mu = mu1, Sigma = sigma1 ))
cluster2 <- data.frame(mvrnorm(N, mu = mu2, Sigma = sigma2 ))
cluster3 <- data.frame(mvrnorm(N, mu = mu3, Sigma = sigma3 ))
data_clust <- rbind(cluster1, cluster2, cluster3)
col_clust <- c(rep("red", N), rep("blue", N), rep("black", N))

# Plotting simulated data
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(size = 5) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

# Plotting simulated data showing the cluster they belong to
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(col = col_clust, size = 5) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

#### 1 - Hierarchical clustering
# A) Single linkage (min)
dendr <- hclust(dist(data_clust), method = "single")
# Plotting result as dendrogram
plot(as.dendrogram(dendr))
# Plotting result in 2D
col_clust2 <- cutree(dendr, 2)
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(col = col_clust2, size = 2) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

# B) Complete linkage (max)
dendr <- hclust(dist(data_clust), method = "complete")
# Plotting result as dendrogram
plot(as.dendrogram(dendr))
# Plotting result in 2D
col_clust2 <- cutree(dendr, 3)
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(col = col_clust2, size = 3) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

#### 2 - K-means clustering
library(factoextra)
# Exploring k to choose the best value
fviz_nbclust(data_clust, FUNcluster = kmeans, method = "wss") 
fviz_nbclust(data_clust, FUNcluster = kmeans, method = "silhouette") 
fviz_nbclust(data_clust, FUNcluster = kmeans, method = "gap_stat") 
# Clustering the data with k=3
kmeans_clustering <- kmeans(data_clust, centers = 3, nstart = 25)
# Plotting the clustering results
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(col = kmeans_clustering$cluster, size = 3) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

#### 3 - Fuzzy c-means clustering
library(ppclust)
# Choosing same k as before: k = 3
fuzzy_clustering <- fcm(data_clust, centers=3)
# Results obtained
fuzzy_clustering
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(col = fuzzy_clustering$cluster, size = 3) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

#### 4 - DBSCAN clustering
library(dbscan)
kNNdistplot(data_clust, k = 5)
abline(h = 1, lty = 2)
dbscan_cluster <- dbscan(data_clust,eps=1,MinPts = 5)
dbscan_cluster
ggplot(data_clust, aes(x = X1, y = X2)) +
  geom_point(col = dbscan_cluster$cluster+1, size = 3) +
  geom_text_repel(label = rownames(data_clust)) +
  theme_bw()

