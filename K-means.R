library(datasets)
?attitude
str(attitude)
View(attitude)
summary(attitude)

# Subset the attitude data, we are picking only two columns to be able to plot the clusters
data = attitude[,c(3,4)]

# Plot subset data
plot(data, main = "% of favourable responses to Learning and Privilege", pch =20, cex =2)

# Perform K-Means with 2 clusters
set.seed(7)
km1 = kmeans(data, 2, nstart =100)
km1

km1$cluster
km1$centers
km1$withinss
km1$betweenss
km1$size

# Plot results
plot(data, col =(km1$cluster) , main="K-Means result with 2 clusters", pch=20, cex=2)

#Check for the optimal number of clusters given the data
mydata <- data
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method", pch=20, cex=2)

#Perform K-Means with the optimal number of clusters identified from the Elbow method
set.seed(7)
km2 = kmeans(data, 6, nstart=100)

# Examine the result of the clustering algorithm
km2

# Plot results
col =(km2$cluster +1)
plot(data, col = col , main="K-Means result with 6 clusters", pch=20, cex=2)
points(km2$centers, col=col, pch=19, cex=2)


# To get all the data instances that belong to cluster one you can use the following code
Cluster1Instances = data[km1$cluster == 1, ]

