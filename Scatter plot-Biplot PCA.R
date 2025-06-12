install.packages(c("MASS", "factoextra", "ggplot2", "readxl"))

library(MASS)         # For statistical computing
library(factoextra)   # For PCA visualization
library(ggplot2)      # For creating plots
library(readxl)       # For reading Excel files

head(myData)
myData.st <- scale(myData[ , -1]) # To remove the first column (-1), scale to convert the remaining values into z scores
head(myData.st)
pca <- prcomp(myData.st)
summary(pca) #To view the results and Importance of components
pca$rotation #To display the weights associated with variables

pca$x #To display the final principle component scores
plot(pca$x[, 1], pca$x[, 2], xlab = "PC1", ylab = "PC2", main = "PCA: PC1 vs. PC2")
biplot(pca, main = "PCA Biplot: PC1 and PC2")

#Scatter plot of the first two principal components
fviz_pca_ind(pca,
             axes = c(1, 2),   # PCs to plot, 1 and 2 represent the first and second components
             geom = "point", 
             col.ind = "blue",  # Points colored blue
             addEllipses = TRUE, # Add confidence ellipses
             repel = TRUE)       # Avoid overlapping labels

#Scatter plot with different row label colours and symbols
fviz_pca_ind(pca, 
             axes = c(1, 2),   # Plot first two principal components
             geom = "point",   # Use points to represent individuals
             repel = TRUE,     # Avoid label overlap
             habillage = myData$Site, # Color points based on grouping variable
             addEllipses = TRUE)  # Add confidence ellipses if groups are defined

#My Biplot to visualize variables and individuals
fviz_pca_biplot(pca, 
                axes = c(1, 2),   # PCs to plot, 1 and 2 represent the first and second components
                geom = c("point", "arrow"),  # Include both points (data points) and arrows (variables)
                repel = TRUE,     # Avoid overlapping text labels
                habillage = myData$Site, # Color points based on grouping variable (if applicable)
                col.var = "red",  # Variables in red
                col.ind = "blue") # Individuals in blue

biplot(pca, main = "Biplot: PC1 vs PC2 with Loadings")
