#install.packages("ca")
#install.packages("ggplot2")
#install.packages("factoextra")

# Load required libraries
library(ca)
library(factoextra)
library(ggplot2)

head(myDataOriginal)
myData <- myDataOriginal[, -1] # Remove the first column because it's "Nr."

#Correspondence analysis
ca_result <- ca(myData) # Perform Correspondence Analysis

#Plot the Correspondence Analysis biplot
plot(ca_result, main = "Correspondence Analysis of Mesolithic Tool Assemblages")

#Compute Row and Column Totals
row_totals <- rowSums(myData)
col_totals <- colSums(myData)
grand_total <- sum(myData)

#Compute Row Profiles containing each row divided by its total
row_profiles <- myData / row_totals

#Compute Column Profiles each column divided by its total
col_profiles <- t(t(myData) / col_totals)

#Compute Average Profile
average_profile <- outer(row_totals, col_totals, FUN = "/") / grand_total

print("Average Profile:")
print(average_profile)

#Extract Singular Values
singular_values <- ca_result$sv

#Calculate Eigenvalues 
eigenvalues <- singular_values^2

#Calculate Total Inertia
total_inertia <- sum(eigenvalues)

#Calculate Relative Inertias (Proportion of each Eigenvalue)
relative_inertias <- eigenvalues / total_inertia
str(relative_inertias)

#Plot Scree Plot for Relative Inertias
barplot(relative_inertias * 100, 
        names.arg = seq_along(relative_inertias), 
        main = "Scree Plot of Relative Inertias",
        xlab = "Dimensions",
        ylab = "Relative Inertia (%)",
        col = "skyblue",
        border = "black")


#Chi-Square Distances for Each Site
chi_square_distances <- rowSums(((row_profiles - average_profile)^2) / average_profile)

#Identify the Most Different Assemblage (Highest Distance)
most_different_site <- which.max(chi_square_distances)

print("Chi-Square Distances for Each Site:")
print(chi_square_distances)

cat("\nThe most different assemblage is Site", most_different_site, "with a chi-square distance of", max(chi_square_distances), "\n")

#Plot the Chi-Square Distances
barplot(chi_square_distances, 
        names.arg = 1:length(chi_square_distances), 
        main = "Chi-Square Distances from Average Profile", 
        xlab = "Site Number", 
        ylab = "Chi-Square Distance", 
        col = "red", 
        border = "black")

#Compute Column Totals
col_totals <- colSums(myData)
grand_total <- sum(myData)

#Compute Column Profiles (Each column divided by its total)
column_profiles <- sweep(myData, 2, col_totals, FUN = "/")

#Compute Expected (Average) Profile
expected_profile <- outer(rowSums(myData), col_totals, FUN = "/") / grand_total

#Compute Variance Contribution of Each Tool Type
variance_contribution <- colSums((column_profiles - expected_profile / grand_total) ^ 2)

#Convert results to a DataFrame
variance_df <- data.frame(
  Tool_Type = colnames(myData),
  Variance_Contribution = variance_contribution
)

#Sort by variance contribution
variance_df <- variance_df[order(-variance_df$Variance_Contribution),]

#Identify the most influential variable
most_influential_variable <- variance_df$Tool_Type[1]

#Print the results
print("Variance Contribution of Each Tool Type:")
print(variance_df)

cat("The tool type responsible for most of the variance is: ", most_influential_variable)

#Create a bar plot for Variance Contribution
ggplot(variance_df, aes(x = reorder(Tool_Type, -Variance_Contribution), y = Variance_Contribution)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Contribution of Each Tool Type to Variance",
       x = "Tool Type", y = "Variance Contribution") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Extract the loadings for the first dimension (Factor 1)
factor_1_loadings <- ca_result$colcoord[, 1]  # Column coordinates for Dimension 1

#Compute the squared contributions of each variable to Factor 1
factor_1_contributions <- (factor_1_loadings^2) / sum(factor_1_loadings^2)

#Convert results to a DataFrame
factor_1_df <- data.frame(
  Tool_Type = colnames(myData),
  Contribution_to_Factor_1 = factor_1_contributions
)

#Sort by contribution to Factor 1
factor_1_df <- factor_1_df[order(-factor_1_df$Contribution_to_Factor_1), ]

#Identify the most influential variable for Factor 1
most_influential_variable_factor_1 <- factor_1_df$Tool_Type[1]

#Print the results
print("Contribution of Each Tool Type to Factor 1:")
print(factor_1_df)

cat("The tool type with the most influence on the first factor is ", most_influential_variable_factor_1)

# Create a bar plot for Contribution to Factor 1
ggplot(factor_1_df, aes(x = reorder(Tool_Type, -Contribution_to_Factor_1), y = Contribution_to_Factor_1)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Contribution of Each Tool Type to Factor 1 (First Dimension)",
       x = "Tool Type", y = "Contribution to Factor 1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Extract the row coordinates for the first dimension (Factor 1)
factor_1_row_coords <- ca_result$rowcoord[, 1]  # Row coordinates for Dimension 1

#Compute the squared contributions of each assemblage to Factor 1
factor_1_contributions <- (factor_1_row_coords^2) / sum(factor_1_row_coords^2)

#Convert results to a DataFrame
factor_1_sites_df <- data.frame(
  Site = 1:nrow(myData),  # Assign site numbers
  Contribution_to_Factor_1 = factor_1_contributions
)

#Sort by contribution to Factor 1
factor_1_sites_df <- factor_1_sites_df[order(-factor_1_sites_df$Contribution_to_Factor_1), ]

#Identify the most influential assemblage for Factor 1
most_influential_site_factor_1 <- factor_1_sites_df$Site[1]

print("Contribution of Each Site to Factor 1:")
print(factor_1_sites_df)

cat("The assemblage with the most influence on the first factor is ", most_influential_site_factor_1)

#Create a bar plot for Contribution to Factor 1
ggplot(factor_1_sites_df, aes(x = reorder(Site, -Contribution_to_Factor_1), y = Contribution_to_Factor_1)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Contribution of Each Site to Factor 1 (First Dimension)",
       x = "Site Number", y = "Contribution to Factor 1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ------------------------- question 12 - calculate the number of factors needed

#Extract Singular Values
singular_values <- ca_result$sv

#Compute Eigenvalues (Singular Values Squared)
eigenvalues <- singular_values^2

#Compute Total Inertia (Sum of Eigenvalues)
total_inertia <- sum(eigenvalues)

#Compute Relative and Cumulative Inertias
relative_inertia <- eigenvalues / total_inertia
cumulative_inertia <- cumsum(relative_inertia)

#Create a DataFrame for the results
inertia_df <- data.frame(
  Dimension = seq_along(relative_inertia),
  Eigenvalues = eigenvalues,
  Relative_Inertia = relative_inertia,
  Cumulative_Inertia = cumulative_inertia
)

#Identify the number of factors needed (threshold = 80%)
num_factors_needed <- min(which(cumulative_inertia >= 0.8))

#Print the results
print("Eigenvalues, Relative Inertia, and Cumulative Inertia:")
print(inertia_df)

cat("The number of factors required to explain at least 80% of the variance is: ", num_factors_needed)

#Create a Scree Plot (Variance Explained by Each Factor)
ggplot(inertia_df, aes(x = Dimension, y = Relative_Inertia * 100)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.7) +
  labs(title = "Scree Plot: Variance Explained by Each Factor",
       x = "Factor (Dimension)", y = "Variance Explained (%)") +
  geom_text(aes(label = round(Relative_Inertia * 100, 2)), vjust = -0.5, size = 3) +
  theme_minimal()

# ------------------------- question 13

# Extract the coordinates for the first two dimensions (factors)
factor_coords <- ca_result$rowcoord[, 1:2]

# Convert results to a DataFrame
scatter_df <- data.frame(
  Factor_1 = factor_coords[, 1],
  Factor_2 = factor_coords[, 2],
  Site = 1:nrow(myData)  # Site numbers
)

#Create the scatter plot for the first two factors
#ggplot(scatter_df, aes(x = Factor_1, y = Factor_2, label = Site)) +
#  geom_point(color = "blue", size = 3) +
#  geom_text(vjust = -0.5, size = 3) +
#  labs(title = "Scatter Plot of the First Two Factors",
#       x = "Factor 1",
#       y = "Factor 2") +
#  theme_minimal()

fviz_ca_row(
  ca_result, 
  title = "Row Biplot: Site Group Characterization",
  habillage = myDataOriginal$Nr., # Color points based on grouping variable
  col.row = "blue",   # Color for sites (rows)
  axes = c(1,2),
  repel = TRUE        # Avoid label overlap
)
