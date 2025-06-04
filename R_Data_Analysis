🔍 Analysis Summary

1. Principal Components Analysis (PCA)
Dataset: Mineral composition of Romano-British glassware
Goal: Reduce dimensionality and explore variance across samples.

Scaled the data and computed PCA using prcomp().
Identified antimony (Sb) as the main contributor to PC1.
Found that 7 components explain over 80% of variance.
Used biplots and ellipses to explore group structure and outliers.
Re-ran PCA using a subset of five key variables (Ti, Ca, Na, Fe, Zn) for a more interpretable model.
📈 Tools used: factoextra, ggplot2, prcomp

2. Correspondence Analysis (CA)
Dataset: Mesolithic tool assemblages from English archaeological sites
Goal: Analyze categorical count data to interpret variation among sites.

Used the ca package to perform CA.
Determined that Saws contributed most to variance.
Found Burins and assemblage 27 as most influential on Factor 1.
Two dimensions explained over 80% of variance.
Visualized row biplots to detect 4 distinct site groupings and outliers.
📈 Tools used: ca, ggplot2

3. Cluster Analysis
Dataset: Same as CA
Methods:

Hierarchical clustering: Compared Single Linkage vs. Ward’s method
Single Linkage → chained clusters, poor separation
Ward’s Method → compact, well-separated clusters
K-means clustering: Divided tool assemblages into 5 clusters
Used Voronoi-like scatter plots to visualize clustering.
📈 Tools used: hclust, kmeans, factoextra, ggplot2
