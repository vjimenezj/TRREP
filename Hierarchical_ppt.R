# Installing required packages
if (!require("MASS")) install.packages("MASS")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggrepel")) install.packages("ggrepel")

# Example in the ppt
library(MASS)
library(ggplot2)
library(ggrepel)

N<-4
mu <- c(0,0)
sigma <- matrix(nrow=2,c(1,0,0,1))
set.seed(1)
example <- data.frame(mvrnorm(N, mu = mu, Sigma = sigma ))

ggplot(example, aes(x = X1, y = X2)) +
  geom_point(size = 5, col = "blue") +
  geom_text_repel(label = rownames(example), size = 5) +
  theme_bw()

distances <- round(dist(example), 1)
similarities <- round(1 - dist(example)/max(dist(example)), 1)

dendr <- hclust(dist(example), method = "complete")
plot(as.dendrogram(dendr))