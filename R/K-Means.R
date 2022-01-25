data("USArrests") #Load the dataset USArrests
df <- scale(USArrests) # Scaling the data
# View the firt 3 rows of the data
head(df, n =3)

#Estimating the optimal number of clusters
library(ggplot2)
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

#Computing k-means clustering
#Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)
# Print the results
print(km.res)


aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)

# Cluster number for each of the observations
km.res$cluster
head(km.res$cluster, 4)

# Cluster size
km.res$size

# Cluster means
km.res$centers

fviz_cluster(km.res, data = df,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
