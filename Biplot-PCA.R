library(MASS)         # For statistical computing
library(factoextra)   # For PCA visualization
library(ggplot2)      # For creating plots
library(readxl)       # For reading Excel files

head(myData_five_elements_two)
myData_five_elements_two.st <- scale(myData_five_elements_two[ , -1]) # To remove the first column (-1), scale to convert the remaining values into z scores
head(myData_five_elements_two.st)
pca <- prcomp(myData_five_elements_two.st)
summary(pca) #To view the results and Importance of components
pca$rotation #To display the weights associated with variables

pca$x #To display the final principle component scores
plot(pca$x[, 1], pca$x[, 2], xlab = "PC1", ylab = "PC2", main = "PCA: PC1 vs. PC2")
biplot(pca, main = "PCA Biplot: PC1 and PC2")

#Scatter plot with different row label colours and symbols
fviz_pca_ind(pca, 
             axes = c(1, 2),   # Plot first two principal components
             geom = "point",   # Use points to represent individuals
             repel = TRUE,     # Avoid label overlap
             habillage = myData_five_elements_two$Site, # Color points based on grouping variable
             addEllipses = TRUE)  # Add confidence ellipses if groups are defined