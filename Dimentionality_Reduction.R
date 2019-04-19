### Dimensionality Reduction in R

### 1. Principal component analysis (PCA)

# Explore cars with summary()
summary(cars)

# Get the correlation matrix with cor()
correl <- cor(cars[,9:19], use = "complete.obs")

# Use ggcorrplot() to explore the correlation matrix
ggcorrplot(correl)

# Conduct hierarchical clustering on the correlation matrix
ggcorrplot_clustered <- ggcorrplot(correl, hc.order = TRUE, type = "lower")
ggcorrplot_clustered

# Run a PCA for the 10 non-binary numeric variables of cars.
pca_output_ten_v <- PCA(cars[,9:19], ncp = 4, graph = FALSE)

# Get the summary of the first 100 cars.
summary(pca_output_ten_v, nbelements = 100)

# Get the variance of the first 3 new dimensions.
pca_output_ten_v$eig[,2][1:3]

# Get the cumulative variance.
pca_output_ten_v$eig[,3][1:3]

# Run a PCA with active and supplementary variables
pca_output_all <- PCA(cars, quanti.sup = 1:8, quali.sup = 20:21, graph = FALSE)

# Get the most correlated variables
dimdesc(pca_output_all, axes = 1:2)

# Run a PCA on the first 100 car categories
pca_output_hundred <- PCA(cars, quanti.sup = 1:8, quali.sup = 20:21, ind.sup = 101:nrow(cars), graph = FALSE)

# Trace variable contributions in pca_output_hundred
pca_output_hundred$var$contrib

# Run a PCA using the 10 non-binary numeric variables.
cars_pca <- dudi.pca(cars[,9:19], scannf = F, nf = 4)

# Explore the summary of cars_pca.
summary(cars_pca)

# Explore the summary of pca_output_ten_v.
summary(pca_output_ten_v)

# Modify the code to create a factor map for the individuals.
fviz_pca_var(pca_output_all, select.var = list(cos2 = 0.7), repel = TRUE)

# Modify the code to create a factor map for the individuals.
fviz_pca_ind(pca_output_all, select.ind = list(cos2 = 0.7), repel = TRUE)

# Create a barplot for the variables with the highest cos2 in the 1st PC.
fviz_cos2(pca_output_all, choice = "var", axes = 1, top = 10)

# Create a barplot for the variables with the highest cos2 in the 2nd PC.
fviz_cos2(pca_output_all, choice = "var", axes = 2, top = 10)

# Create a factor map for the top 5 variables with the highest contributions.
fviz_pca_var(pca_output_all, select.var = list(contrib = 5), repel = TRUE)

# Create a factor map for the top 5 individuals with the highest contributions.
fviz_pca_ind(pca_output_all, select.ind = list(contrib = 5), repel = TRUE)

# Create a barplot for the variables with the highest contributions to the 1st PC.
fviz_contrib(pca_output_all, choice = "var", axes = 1, top = 5)

# Create a barplot for the variables with the highest contributions to the 2nd PC.
fviz_contrib(pca_output_all, choice = "var", axes = 2, top = 5)

# Create a biplot with no labels for all individuals with the geom argument.
fviz_pca_biplot(pca_output_all)

# Create ellipsoids for wheeltype columns respectively.
fviz_pca_ind(pca_output_all, habillage = cars$wheeltype, addEllipses = TRUE)

# Create the biplot with ellipsoids
fviz_pca_biplot(pca_output_all, habillage = cars$wheeltype, addEllipses = TRUE, alpha.var = "cos2")

#######################

### 2. Advanced PCA & Non-negative matrix factorization (NNMF)

