#######################################
# Prepare data for K-means Clustering #
#######################################

# Get grace period for each locale
locale_grace_periods <- map_dfr(locales, get_grace_period)


# Combine grace period information with population information
kmeans_setup <- inner_join(

  x = locale_grace_periods,
  y = population_data_US,
  by = c("Locale" = "Locale")

)

# Omit State field
kmeans_setup <- kmeans_setup %>% select(-"State")


# Use tibble package to convert Locale field values to row names
# This is the data.frame structure for K-means 
kmeans_setup <- column_to_rownames(.data = kmeans_setup, var = "Locale")


# Compute Z-scores of fields for k-means: Grace_period, Population
kmeans_setup <- scale(kmeans_setup)
