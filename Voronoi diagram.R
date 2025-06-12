# Load necessary libraries
#install.packages("readxl")
#install.packages("factoextra")
#install.packages("ggvoronoi") # For Voronoi diagram
#install.packages("deldir") # Dependency for ggvoronoi
#install.packages("ggplot2") # For visualization

library(factoextra)
library(deldir)

# Remove the first column if it's just an index
data <- myDataOriginal[, -1]

# Compute the Euclidean distance
dist_matrix <- dist(data, method = "euclidean")

# Perform hierarchical clustering using Single Linkage
hc_single <- hclust(dist_matrix, method = "single")

# Plot dendrogram
plot(hc_single, main = "Single Linkage Dendrogram", sub = "", xlab = "")
rect.hclust(hc_single, k = 4, border = "blue")

# Compute the Euclidean distance
dist_matrix <- dist(data, method = "euclidean")

# Perform hierarchical clustering using Ward’s method
hc_ward <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc_ward, main = "Ward’s Method Dendrogram", sub = "", xlab = "")
rect.hclust(hc_ward, k = 4, border = "blue")

# Set number of clusters
k <- 5

# Perform K-Means clustering
set.seed(42)  # Ensures reproducibility
kmeans_result <- kmeans(data, centers = k, nstart = 10)

# Add cluster assignments to the dataset
data$Cluster <- as.factor(kmeans_result$cluster)

# Create Voronoi diagram
ggplot(data, 
  aes(x = Microliths, y = Scrapers, color = Cluster)) +
  geom_point(size = 1) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +
  labs(title = "K-Means Clustering of Tools into 5 Groups",
       x = "Microliths",
       y = "Scrapers",
       color = "Cluster") +
  theme_minimal()
