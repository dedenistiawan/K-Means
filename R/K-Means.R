library(readr)
urlfile = "https://raw.githubusercontent.com/dedenistiawan/Dataset/main/BDT.csv"
data<-read.csv(url(urlfile), row.names = "Kabupaten")


#View the firt 3 rows of the data
head(data, n =3)

#Plot Disatance
library(ggplot2)
library(factoextra)
distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Running k-means clustering k=2
k2 <- kmeans(data, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = data)

#use several different values of k
k3 <- kmeans(data, centers = 3, nstart = 25)
k4 <- kmeans(data, centers = 4, nstart = 25)
k5 <- kmeans(data, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = data) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = data) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = data) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = data) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#Determining number Optimal Clusters
##Elbow Method
library(ggplot2)
library(factoextra)
fviz_nbclust(data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

##Average Silhouette Method
fviz_nbclust(data, kmeans, method = "silhouette")

##Gap Statistic Method
library(cluster)
set.seed(123)
gap_stat <- clusGap(data, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

#Computing k-means clustering
#Compute k-means with k = 2
set.seed(123)
km.res <- kmeans(data, 2, nstart = 25)
# Print the results
print(km.res)

aggregate(data, by=list(cluster=km.res$cluster), mean)

dd <- cbind(data, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster

# Cluster size
km.res$size

# Cluster means
km.res$centers
fviz_cluster(km.res, data = data)

fviz_cluster(km.res, data = data,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
