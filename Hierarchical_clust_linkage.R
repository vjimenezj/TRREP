# Installing required packages
if (!require("MASS")) install.packages("MASS")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")

### Checking the impact of the linkage method in hierarchical clustering
library(MASS)
library(ggplot2)
library(ggrepel)

N<-20
mu <- c(0,0)
sigma <- matrix(nrow=2,c(1,0,0,1))
set.seed(1)
example <- data.frame(mvrnorm(N, mu = mu, Sigma = sigma ))

ggplot(example, aes(x = X1, y = X2)) +
  geom_point() +
  geom_text_repel(label = rownames(example)) +
  theme_void()

distances <- dist(example)
similarities <- 1 - dist(example)/max(dist(example))

dendr <- hclust(dist(example), method = "single")
plot(as.dendrogram(dendr))
colcluster <- cutree(dendr, 3)
ggplot(example, aes(x = X1, y = X2)) +
  geom_point(col = colcluster, size = 5) +
  geom_text_repel(label = rownames(example)) +
  theme_bw()
