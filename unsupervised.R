?USArrests
# Quick 2nd order summary for data.frame.
colMeans(USArrests)
sapply(USArrests, sd)
cor(USArrests)

# PCA
pca_out <- prcomp(USArrests, scale = TRUE)
biplot(pca_out, scale = 0, cex = 0.7)

# K-means
set.seed(101)
n <- 100L
k <- 4L
x <- matrix(rnorm(2*n), n, 2)
centroids <- matrix(rnorm(2*k, sd = 2*sqrt(k)), k, 2)
clusters <- sample(1:k, n, replace = TRUE)
x <- x + centroids[clusters, ]
plot(x, col = clusters, pch = 19, asp = 1)
points(centroids, col = 1:k, pch = 3, cex = 2, lwd = 3)
(km_out <- kmeans(x, k, nstart = 15))
points(x, col = km_out$cluster, pch = 1, cex = 2, lwd = 2)
table(km_out$cluster, clusters)

# Hierarchical clustering
hc_complete <- hclust(dist(x))
plot(hc_complete, labels = clusters, hang = -1)
hc_single <- hclust(dist(x), method = 'single')
plot(hc_single, labels = clusters, hang = -1)
hc_average <- hclust(dist(x), method = 'average')
plot(hc_average, labels = clusters, hang = -1)
hc_cut <- cutree(hc_complete, k = 4)
table(hc_cut, clusters)
