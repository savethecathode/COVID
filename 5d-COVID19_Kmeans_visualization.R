################################################################
## Visualize clusters given the optimal number of clusters: 6 ##
################################################################

## Generate 6 clusters
kmeans_data <- kmeans(
 
  kmeans_setup,
  centers  = 6,
  nstart   = 25,
  iter.max = 20
 
)


## Put cluster data for each locale in a data.frame
kmeans_data <- data.frame(kmeans_data$cluster)


## Rename Cluster field
kmeans_data <- kmeans_data %>%
  rename(Cluster = kmeans_data.cluster)


## Return locale info in row names to an explicit column
kmeans_data  <- rownames_to_column(.data = kmeans_data , var = "Locale")


## Use unscale of DMwR package to reverse action of scale() on kmeans_setup
## install.packages("DMwR")
## library(DMwR)
## Join clustering data with the variables used for clustering
## *** Variables used for clustering have been 'unscaled', returned to their original values from the z-scores
clustering_data <- inner_join(
  
  x = kmeans_data,
  y = rownames_to_column(
    
    .data = data.frame(unscale(kmeans_setup, kmeans_setup)),
    var = "Locale"
    
  ),
  by = c("Locale" = "Locale")
  
)


## Plot clusters
clustering_data %>%
  ggplot(aes(Grace_period, Population, col = Cluster)) +
  geom_point() +
  scale_color_gradientn(colours = rainbow(6)) +
  theme_light() +
  labs(
    
    title = "K-means Clustering Analysis",
    subtitle = "Locales within JHU CSSE Data Set",
    caption = "*Grace period : days between the 1st recorded cases in the US and the 1st recorded cases for a given locale",
    x = "Grace Period",
    y = "Population",
    color = "Cluster"
    
  ) +
  theme(
    
    plot.title    = element_text(hjust =  0.5),
    plot.subtitle = element_text(hjust =  0.5),
    axis.title.y  = element_text(vjust =  3.0),
    axis.title.x  = element_text(vjust = -1.0),
    plot.caption  = element_text(vjust = -2.0, hjust = 0.35)
    
  )


## NOTE: the cluster ID can change from one K-means run to the next


## Get cluster containing NYC, NY
nyc_cluster_id <- clustering_data %>%
  filter(Locale == "New York City, New York") %>%
  pull(Cluster)


## Get info for locales in same cluster as NYC, NY
nyc_cluster <- clustering_data %>%
  filter(Cluster == nyc_cluster_id)

