###### Chapter 16 Cluster analysis
####
# Cluster analysis is a data-reduction technique designed to uncover subgroups 
#     of observations within a dataset. 
# A cluster is defined as a group of observations that are more similar to 
#     each other than they are to the observations in other groups. 
####
#  The two most popular clustering approaches are hierarchical agglomerative 
#     clusterin and partitioning clustering. 
# In agglomerative hierarchical clustering, each observation starts as its own
#     cluster. Clusters are then combined, two at a time, until all clusters 
#     are merged into a single cluster. 
# In the partitioning approach, you specify K: the number of clusters sought.
#     Observations are then randomly divided into K groups and reshuffled to 
#     form cohesive clusters
####
# Within each of these broad approaches, there are many clustering algorithms 
#     to choose from. 
# For hierarchical clustering, the most popular are single linkage, complete 
#     linkage, average linkage, centroid, and Ward’s method. 
# For partitioning, the two most popular are k-means and partitioning around
#     medoids (PAM). 
####
# 1 Choose appropriate attributes. 
#     The first (and perhaps most important) step is to select variables that
#     you feel may be important for identifying and understanding differences 
#     among groups of observations within the data. 
# 2 Scale the data. 
#     If the variables in the analysis vary in range, the 
#     variables with the largest range will have the greatest impact on the 
#     results. This is often undesirable, and analysts scale the data before
#     continuing. 
#       The most popular approach is to standardize each variable to
#       a mean of 0 and a standard deviation of 1.
#       Other alternatives include dividing each variable by its maximum 
#       value or subtracting the variable’s mean and dividing by the 
#       variable’s median absolute deviation. 
#       The three approaches are illustrated with the following code snippets:
#       df1 <- apply(mydata, 2, function(x){(x-mean(x))/sd(x)})
#       df2 <- apply(mydata, 2, function(x){x/max(x)})
#       df3 <- apply(mydata, 2, function(x){(x – mean(x))/mad(x)})
#     In this chapter, you’ll use the scale() function to standardize the 
#     variables to a mean of 0 and a standard deviation of 1. This is 
#     equivalent to the first code snippet (df1).
# 3 Screen for outliers. 
#     Many clustering techniques are sensitive to outliers, distorting
#     the cluster solutions obtained. 
#     You can screen for (and remove) univariate outliers using functions 
#     from the outliers package. The mvoutlier package contains functions 
#     that can be used to identify multivariate outliers. An alternative
#     is to use a clustering method that is robust to the presence of 
#     outliers. Partitioning around medoids is an example of the latter 
#     approach.
# 4 Calculate distances. 
#     Although clustering algorithms vary widely, they typically
#     require a measure of the distance among the entities to be clustered. 
#     The most popular measure of the distance between two observations is 
#     the Euclidean distance, but the Manhattan, Canberra, asymmetric binary,
#     maximum, and Minkowski distance measures are also available.
#     In this chapter, the Euclidean distance is used throughout. 
# 5 Select a clustering algorithm. 
#     Next, you select a method of clustering the data.
#     Hierarchical clustering is useful for smaller problems (say, 150 
#     observations orless) and where a nested hierarchy of groupings is
#     desired. 
#     The partitioning method can handle much larger problems but requires 
#     that the number of clusters be specified in advance. 
#     Once you’ve chosen the hierarchical or partitioning approach, you must
#     select a specific clustering algorithm. Again, each has advantages and
#     disadvantages.
# 6 Obtain one or more cluster solutions. 
# 7 Determine the number of clusters present. 
#     In order to obtain a final cluster solution,you must decide how many 
#     clusters are present in the data. This is a thorny problem, and many 
#     approaches have been proposed. It usually involves extracting various 
#     numbers of clusters (say, 2 to K) and comparing the quality of the
#     solutions. 
# 8 Obtain a final clustering solution.
#     Once the number of clusters has been determined, a final clustering i
#     s performed to extract that number of subgroups.
# 9 Visualize the results. 
#     Visualization can help you determine the meaning and usefulness of the 
#     cluster solution. The results of a hierarchical clustering are 
#     usually presented as a dendrogram. 
#     Partitioning results are typically visualized using a bivariate cluster 
#     plot. 
# 10 Interpret the clusters.
#     Once a cluster solution has been obtained, you must interpret
#     (and possibly name) the clusters. 
# 11 Validate the results. 
#     Validating the cluster solution involves asking the question.
####
# Every cluster analysis begins with the calculation of a distance, 
#     dissimilarity, or proximity between each entity to be clustered. 
data(nutrient, package = "flexclust")
t <- nutrient
head(t, 3)
#
d <- dist(t, method = "euclidean") # by default
as.matrix(d)[1:4, 1:4]
# Euclidean distances are usually the distance measure of choice for
#     continuous data. But if other variable types are present, alternative 
#     dissimilarity measures are required. You can use the daisy() function 
#     in the cluster package to obtain a dissimilarity matrix among 
#     observations that have any combination of binary, nominal, ordinal, 
#     and continuous attributes. Other functions in the cluster package can
#     use these dissimilarities to carry out a cluster analysis. For 
#     example, agnes() offers agglomerative hierarchical clustering, 
#     and pam() provides partitioning around medoids.
####
# As stated previously, in agglomerative hierarchical clustering, each case
#     or observation starts as its own cluster. Clusters are then combined 
#     two at a time until all clusters are merged into a single cluster. 
# The algorithm is as follows:
# 1 Define each observation (row, case) as a cluster.
# 2 Calculate the distances between every cluster and every other cluster.
# 3 Combine the two clusters that have the smallest distance. This reduces 
#     the number of clusters by one.
# 4 Repeat steps 2 and 3 until all clusters have been merged into a single 
#     cluster containing all observations.
# The primary difference among hierarchical clustering algorithms is their 
#     definitions of cluster distances (step 2). 
####
# Five of the most common hierarchical clustering methods:
# Single linkage: Shortest distance between a point in one cluster and a 
#     point in the other cluster. 
# Complete linkage: Longest distance between a point in one cluster and 
#     a point in the other cluster.
# Average linkage: Average distance between each point in one cluster and 
#     each point in the other  cluster (also called UPGMA 
#     [unweighted pair group mean averaging]).
# Centroid: Distance between the centroids (vector of variable means) of the
#     two clusters. For a single observation, the centroid is the variable’s 
#     values.
# Ward: The ANOVA sum of squares between the two clusters added up over all
#     the variables.
####
# Single-linkage clustering tends to find elongated, cigar-shaped clusters. 
#     It also commonly displays a phenomenon called chaining—dissimilar 
#     observations are joined into the same cluster because they’re similar
#     to intermediate observations between them.
# Complete-linkage clustering tends to find compact clusters of 
#     approximately equal diameter. It can also be sensitive to outliers. 
# Average-linkage clustering offers a compromise between the two. It’s less
#     likely to chain and is less susceptible to outliers. It also has a 
#     tendency to join clusters with small variances.
# Ward’s method tends to join clusters with small numbers of observations 
#     and tends to produce clusters with roughly equal numbers of 
#     observations. It can also be sensitive to outliers. 
# The centroid method offers an attractive alternative due to its simple
#     and easily understood definition of cluster distances. It’s also less 
#     sensitive to outliers than other hierarchical methods. But it may not 
#     perform as well as the average-linkage or Ward method
####
rownames(t) <- tolower(rownames(t))
#
standardized.t <- scale(t)
d <- dist(standardized.t) # get distance matrix
fit.average <- hclust(d, method = "average")
plot(fit.average, hang = -1, cex = 0.8, 
     main = "Average Linkage Clustering") # plot the results in the dendrogram
#  The hang option in the justifies the observation labels (causing them to 
#     hang down from 0).
# The dendrogram displays how items are combined into clusters and is read 
#     from the bottom up. 
# The height dimension indicates the criterion value at which clusters are 
#     joined. For average-linkage clustering, this criterion is the average
#     distance between each point in one cluster and each point in the other
#     cluster.
####
require("NbClust")
devAskNewPage(ask = TRUE)
nc <- NbClust(standardized.t, distance = "euclidean", 
        min.nc = 2, max.nc = 15, method = "average") # find the best suitted
#     number k for clustering
table(nc$Best.nc[1,])
tbl <- .Last.value
# 
devAskNewPage(ask = FALSE)
barplot(tbl, 
        xlab = "Number of clusters", ylab = "Number of criteria",
        main = "Number of clusters choosen by 26 criteria")
#
cluster <- cutree(fit.average, k = 5) # used to cut the tree into five 
#     clusters
table(cluster)
aggregate(t, by = list(cluster = cluster), median)
#
aggregate(as.data.frame(standardized.t), by = list(cluster = cluster),
          median)
#
plot(fit.average, hang = -1, cex = 0.8,
     main = "average linakge clustering \n5 cluster solution")
rect.hclust(fit.average, k = 5)
#  Hierarchical clustering can be particularly useful when you expect 
# nested clustering and a meaningful hierarchy. 
####
# In the partitioning approach, observations are divided into K groups and 
#     reshuffled to form the most cohesive clusters possible according to a 
#     given criterion. 
# This section considers two methods: k-means and partitioning around medoids (PAM).
####
# The most common partitioning method is the k-means cluster analysis. 
# the k-means algorithm is as follows:
# 1 Select K centroids (K rows chosen at random).
# 2 Assign each data point to its closest centroid.
# 3 Recalculate the centroids as the average of all data points in a 
#     cluster (that is, the centroids are p-length mean vectors, where p is 
#     the number of variables).
# 4 Assign data points to their closest centroids.
# 5 Continue steps 3 and 4 until the observations aren’t reassigned or the 
#     maximum number of iterations (R uses 10 as a default) is reached
####
#  But the use of means implies that all variables must be continuous, and 
#     the approach can be severely affected by outliers. It also performs 
#     poorly in the presence of non-convex (for example, U-shaped) clusters. 
# Because k-means cluster analysis starts with k randomly chosen centroids, 
#     a different solution can be obtained each time the function is invoked. 
# Additionally, this clustering approach can be sensitive to the initial
#     selection of centroids. The kmeans() function has an nstart option 
#     that attempts multiple initial configurations and reports on the best 
#     one. 
####
wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
}  # here, nc is the maximum number of clusters to consider
#
data(wine, package = "rattle")
t <- wine
head(t, 3)
# 
df <- scale(t[-1])
wssplot(df) # a band in graph can suggest the appropriate numbers of clusters
#
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")
table(nc$Best.nc[1,])
tbl <- .Last.value
barplot(tbl, xlab = "number of clusters",
        main = "number of clusters chosen by 26 criterians") #  Note that 
#     not all 30 criteria can be calculated for every dataset.
#
set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25)
fit.km$size
#
aggregate(t[-1], by = list(Cluster = fit.km$cluster), func)
fit.km$centers # the same as aggregate(df, by = list(Cluster = fit.km$cluster), func)
####
#  How well did k-means clustering uncover the actual structure of the 
#     data contained in the Type variable? 
# A cross-tabulation of Type (wine varietal) and cluster membership 
ct.km <- table(t$Type, fit.km$cluster)
flexclust::randIndex(ct.km) # quantify the aggreement bewteen type and cluster
#     using an adjusted Rand index
# The adjusted Rand index provides a measure of the agreement between two 
#     partitions, adjusted for chance. It ranges from -1 (no agreement) to 
#     1 (perfect agreement). 
####
# A more robust solution is provided by partitioning around medoids (PAM).
# Rather than representing each cluster using a centroid (a vector of 
#     variable means), each cluster is identified by its most representative
#     observation (called a medoid).
# Whereas k-means uses Euclidean distances, PAM can be based on any distance
#     measure. It can therefore accommodate mixed data types and isn’t 
#     limited to continuous variables.
####
#  The PAM algorithm is as follows:
# 1 Randomly select K observations (call each a medoid).
# 2 Calculate the distance/dissimilarity of every observation to each medoid.
# 3 Assign each observation to its closest medoid.
# 4 Calculate the sum of the distances of each observation from its 
#     medoid (total cost).
# 5 Select a point that isn’t a medoid, and swap it with its medoid.
# 6 Reassign every point to its closest medoid.
# 7 Calculate the total cost.
# 8 If this total cost is smaller, keep the new point as a medoid.
# 9 Repeat steps 5–8 until the medoids don’t change
####
set.seed(1234)
fit.pam <- cluster::pam(t[-1],  k = 3, stand = TRUE)
fit.pam$medoids # print the medoids, which are the actual point in the dataset
cluster::clusplot(fit.pam, main = "Bivariate cluster plot") # plot the cluster
#     solution
# The bivariate plot is created by plotting the coordinates of each
#     observation on the first two principal components (see chapter 14) 
#     derived from the 13 assay variables. Each cluster is represented by an
#     ellipse with the smallest area containing all its points.
# 
ct.pam <- table(t$Type, fit.pam$clustering)
flexclust::randIndex(ct.pam) # 0.6994957
####
# Cluster analysis is a methodology designed to identify cohesive subgroups 
#     in a dataset. 
# however, it also can find clusters where none exits
require("fMultivar")
set.seed(1234)
df <- rnorm2d(1000, rho = 0.5) # sample 1000 observations from a bivariate
#     normal distribution with a correlation of 0.5
df <- as.data.frame(df)
plot(df, main = "Bivariate normal distribution with rho = 0.5") # clearly, 
#     there is no cluster
wssplot(df)
nc <- NbClust(df, min.nc = 2, max.nc = 15, method = "kmeans")
par(opar)
barplot(table(nc$Best.nc[1, ]))
#
fit <- cluster::pam(df, k = 2)
df$clustering <- factor(fit$clustering)
#
require("ggplot2")
ggplot(data = df, aes(x = V1, y = V2, 
                          color = clustering, 
                          shape = clustering)) + 
  geom_point() + 
  ggtitle("Clutering of Bivariate Normal data")
####
# the Cubic Cluster Criteria (CCC) reported by NbClust can often help to
#     uncover situations where no structure exists.
plot(nc$All.index[,4], type = "o", ylab = "CCC", 
     xlab = "Number  of clusters", col = col[1]) #  When the CCC values are 
#     all negative and decreasing for two or more clusters, the 
#     distribution is typically unimodal.
# Try different clustering methods, and replicate the findings with new 
#     samples. If the same clusters are consistently recovered, you can
#     be more confident in the results.
####



