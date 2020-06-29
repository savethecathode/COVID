## K-means clustering
## Parameters: 2 parameters considered
## (1) population, (want to use population density but land area info is difficult to find)
## (2) Days between when the 1st cases are documented in the US and when the 1st cases are documented in a given locale


## Set seed of random number generator
set.seed(2394817)


## Test run clustering
#km <- kmeans(clustering_setup, centers = 3, nstart = 25, iter.max = 20)


#######################################################
## Function to compute within cluster sum of squares ##
#######################################################

wss <- function(k) {

  kmeans(

    kmeans_setup,
    centers = k,
    nstart = 25,
    iter.max = 20
    
  )$tot.withinss
  
}


## Sequence of k-values to test
k_seq <- seq(1, 15)


## Get sequence of within-cluster sum of squares
wss_seq <- map_dbl(k_seq, wss)


## Elbow plot
## Plot total intra-cluster variation (total within-cluster sum of squares) v. the number of clusters
plot(x = k_seq, y = wss_seq,
     main = "Total Intra-Cluster Variation",
     ylab = "Within-Cluster Sum of Squares",
     xlab = "Number of Clusters")


## Plot the percent of variance explained v. the number of clusters
pve <- -diff(wss_seq)/wss_seq[1:length(wss_seq)-1]

plot(k_seq[2:length(k_seq)], round(pve, 2)*100,
     main = "Percent of Variance Explained v. Cluster Size",
     ylab = "Percent of Variance Explained (%)",
     xlab = "Number of Clusters")

## Set first 0.25 value as the arbitrary cut-off
## Observation: 6 clusters brings the pve to about 0.2
abline(h = 25)

