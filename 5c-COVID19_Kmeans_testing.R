######################
# K-means Clustering #
######################

# Parameters: 2 parameters considered
# (1) population
# (want to use population density but land area info is difficult to find).
# (2) Grace period - days between when the 1st cases are documented in the US
# and when the 1st cases are documented in a given locale.

# Note: The JHU CSSE COVID-19 data sets are evolving so be aware of potential
# issues in the future.
# There are several ways to address the kmeans() Warning msg:
# "Quick-TRANSfer stage steps exceeded maximum"
# 1. perform garbage collection with gc() to free up memory.
# 2. increase values for options nstart= and iter.max= 
# 3. Try a different algorithm (default: Hartigan-Wong)

# NOTE: the cluster ID can change from one K-means run to the next


# Set random number generator seed
set.seed(2394817)


# K-means test run
#km <- kmeans(clustering_setup, centers = 3, nstart = 25, iter.max = 20)


#####################################################
# Function to compute within cluster sum of squares #
#####################################################

wss <- function(k) {

  kmeans(

    x = kmeans_setup,
    centers = k,
    nstart = 25,
    iter.max = 20
    
  )$tot.withinss
  
}

# Sequence of k-values to test
k_seq <- seq(1, 10)


# Get sequence of within-cluster sum of squares
wss_seq <- map_dbl(k_seq, wss)


# Elbow plot
# Plot total intra-cluster variation
# (total within-cluster sum of squares) v. the number of clusters
plot(x = k_seq, y = wss_seq,
     main = "Total Intra-Cluster Variation",
     ylab = "Within-Cluster Sum of Squares",
     xlab = "Number of Clusters")


# Plot the percent of variance explained v. the number of clusters
pve <- -diff(wss_seq)/wss_seq[1:length(wss_seq)-1]

plot(k_seq[2:length(k_seq)], round(pve, 2)*100,
     main = "Percent of Variance Explained v. Number of Clusters",
     ylab = "Percent of Variance Explained (%)",
     xlab = "Number of Clusters")

# Draw arbitrary cut-off
# Observation: 6 clusters brings the pve to about 0.2
abline(h = 25, lty = 2)

