# Add annotations to scatter ploot.
# Purpose: visualize clustering

# Plot clusters function
# n - specify the number of clusters
plot_kmeans <- function(data, n = NULL) {
  
  subset <- data[which(data$Locale == "Cook, Illinois" | 
                       data$Locale == "Queens, New York" | 
                       data$Locale == "King, Washington"), ]
  
  data %>%
    ggplot(aes(Grace_period, Population, col = Cluster)) +
    geom_point() +
    geom_text_repel(
      size = 3,
      data = subset,
      #nudge_x = 50,
      #nudge_y = 50,
      hjust = 2.5,
      vjust = -5,
      #hjust = -1,
      #vjust = -1,
      #direction = "x",
      direction = "both",
      #box.padding = 0.25,
      segment.size = 0.2,
      color = "black",
      segment.color = "black",
      aes(label = Locale)
    ) +
    scale_color_gradientn(colors = rainbow(n), n.breaks = n) +
    theme_light() +
    labs(
      title = bquote(.(n) ~ "K-means Clusters"),
      subtitle = "Locales for USA within JHU CSSE Data Set",
      caption = bquote(
        "*Grace period : days between the 1st recorded cases" ~
          "in the US and the 1st recorded cases for a given locale"),
      x = "Grace Period (Days)",
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
  
}

# Plot clusters
#plot_kmeans(kmeans_clusters3, 3)
plot_kmeans(kmeans_clusters4, 4)
#plot_kmeans(kmeans_clusters5, 5)
#plot_kmeans(kmeans_clusters6, 6)
#plot_kmeans(kmeans_clusters7, 7)

