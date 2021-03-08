################################################################
## Visualize clusters given the optimal number of clusters: 6 ##
################################################################
# NOTE: the cluster ID can change from one K-means run to the next

kmeans_clustering <- function(data = kmeans_setup,
                              k,
                              n_start = 25,
                              iter_max = 20) {
 
  # Use unscale of DMwR package to reverse action of scale() on kmeans_setup
  require("DMwR")
 
  # Generate 6 clusters
  kmeans_data <- kmeans(

    x = data,
    centers  = k,
    nstart   = n_start,
    iter.max = iter_max

  )
 
 
  # Put cluster data for each locale in a data.frame
  kmeans_data <- data.frame(kmeans_data$cluster)
 
  # Rename Cluster field
  kmeans_data <- kmeans_data %>%
    rename(Cluster = kmeans_data.cluster)
 
  # Return locale info in row names to an explicit column
  kmeans_data  <- rownames_to_column(.data = kmeans_data , var = "Locale")
 
  # Join clustering data with the variables used for clustering
  # *** Variables used for clustering have been 'unscaled',
  # (returned to their original values from the z-scores).
  clustering_data <- inner_join(
   
    x = kmeans_data,
    y = rownames_to_column(
  
      .data = data.frame(unscale(kmeans_setup, kmeans_setup)),
      var = "Locale"
  
    ),
    by = c("Locale" = "Locale")
   
  )

  return(clustering_data)
}


# Get clustering data for range of number-of-clusters
base_name <- "kmeans_clusters"

for (i in 2:7) {

  cluster_name <- paste0(base_name, i)
  assign(cluster_name, kmeans_clustering(k = i))

}


