#########################################
## Prepare data for K-means Clustering ##
#########################################


## Combine grace period information with population information
kmeans_setup <- inner_join(

  x = grace_periods,
  y = population_data_US,
  by = c("Locale" = "Locale")

)


## Omit State field
kmeans_setup <- kmeans_setup %>% select(-"State")


## Use tibble package to convert Locale field values to row names
## This is the data.frame structure for kmeans
kmeans_setup <- column_to_rownames(.data = kmeans_setup, var = "Locale")


# Compute Z-scores of fields for kmeans: Grace_period, Population
kmeans_setup <- scale(kmeans_setup)

