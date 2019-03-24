### Anomaly Detection in R

### 1. Statistical outlier detection

# Explore contents of dataset
head(river)

# Summary statistics of river nitrate concentrations
summary(river$nitrate)

# Plot the distribution of nitrate concentration
boxplot(river$nitrate)

# Separate the histogram into 40 bins 
hist(river$nitrate, xlab = "Nitrate concentration", breaks = 40)

# The histogram you've generated displays a symmetrical bell shape 
# which is what would be expected of normal data, 
# which means you can proceed to use Grubbs' outlier test.

# Apply Grubbs' test to the river nitrate data
grubbs.test(river$nitrate)

# Apply Grubbs' test to the nitrate data
grubbs.test(river$nitrate)

# Use which.max to find row index of the max
which.max(river$nitrate)

# Runs Grubbs' test excluding row 156
grubbs.test(river$nitrate[-156])

# Print the value tested in the second Grubbs' test
max(river$nitrate[-156])

# View contents of dataset
head(river)

# Show the time series of nitrate concentrations with time
plot(nitrate ~ index, data = river, type = "o")

# Calculate the mean nitrate by month
monthly_mean <- tapply(river$nitrate, river$months, FUN = mean)
monthly_mean

# Plot the monthly means 
plot(monthly_mean, type = "o", xlab = "Month", ylab = "Monthly mean")

# Create a boxplot of nitrate against months
boxplot(nitrate ~ months, data = river)

# Use Seasonal-Hybrid ESD for nitrate concentrations
river_anomalies <- AnomalyDetectionVec(x = river$nitrate, period = 12, direction = 'both', plot = T)

# Print the anomalies
river_anomalies$anoms

# Print the plot
print(river_anomalies$plot)

#######################

### 2. Distance and density based anomaly detection

# View the contents of the wine data
head(wine)

# Scatterplot of wine pH against alcohol
plot(pH ~ alcohol, data = wine)

# Calculate the 5 nearest neighbors distance
wine_nn <- get.knn(wine, k = 5)

# View the distance matrix
head(wine_nn$nn.dist)

# Distance from wine 5 to nearest neighbor
wine_nn$nn.dist[5, 1]

# Row index of wine 5's nearest neighbor 
wine_nn$nn.ind[5, 1]

# Return data for wine 5 and its nearest neighbor
wine[c(5, wine_nn$nn.ind[5, 1]), ]

# Calculate the 5 nearest neighbors distance
wine_nn <- get.knn(wine, k = 5)

# Create score by averaging distances
wine_nnd <- rowMeans(wine_nn$nn.dist)

# Print row index of the most anomalous point
which.max(wine_nnd)

# Without standardization, features have different scales
summary(wine)

# Standardize the wine columns
wine_scaled <- scale(wine)

# Standardized features have similar means and quartiles
summary(wine_scaled)

# Print the 5-nearest neighbor distance score
wine_nnd[1:5]

# Append the score as a new column 
wine$score <- wine_nnd

# Scatterplot showing pH, alcohol and kNN score
plot(pH ~ alcohol, data = wine, cex = sqrt(score), pch = 20)

# Calculate the LOF for wine data
wine_lof <- lof(scale(wine), 5)

# Append the LOF score as a new column
wine$score <- wine_lof

# Scatterplot showing pH, alcohol and LOF score
plot(pH ~ alcohol, data = wine, cex = score, pch = 20)

# Scaled wine data
wine_scaled <- scale(wine)

# Calculate and append kNN distance as a new column
wine_nn <- get.knn(wine_scaled, k = 10)
wine$score_knn <- rowMeans(wine_nn$nn.dist)     

# Calculate and append LOF as a new column
wine$score_lof <- lof(wine_scaled, k = 10)

# Find the row location of highest kNN
which.max(wine$score_knn)

# Find the row location of highest LOF
which.max(wine$score_lof)

#######################

### 3. Isolation forest

# Build an isolation tree 
wine_tree <- iForest(wine, nt = 1)

# Create isolation score
wine$tree_score <- predict(wine_tree, newdata = wine)

# Histogram plot of the scores
hist(wine$tree_score, breaks = 40)

# Fit isolation forest
wine_forest <- iForest(wine, nt = 100, phi = 200)

# Create isolation score from forest
wine_score <- predict(wine_forest, wine)

# Append score to the wine data
wine$score <- wine_score

# View the contents of the wine scores
head(wine_scores)

# Score scatterplot 2000 vs 1000 trees 
plot(trees_2000 ~ trees_1000, data = wine_scores)

# Add reference line of equality
abline(a = 0, b = 1)

# Sequence of values for pH and alcohol
ph_seq <- seq(min(wine$pH),  max(wine$pH), length.out = 25)
alcohol_seq <- seq(min(wine$alcohol),  max(wine$alcohol), length.out = 25)

# Create a data frame of grid coordinates
wine_grid <- expand.grid(pH = ph_seq, alcohol = alcohol_seq)

# Visualise the grid using a scatterplot
plot(pH ~ alcohol, data = wine_grid, pch = 20)

# Calculate isolation score at grid locations
wine_grid$score <- predict(wine_forest, wine_grid)

# Contour plot of isolation scores
contourplot(score ~ alcohol + pH, wine_grid, region = TRUE)

#######################

### 4. Comparing performance

# View contents of thryoid data
head(thyroid)

# Tabulate the labels
table(thyroid$label)

# Proportion of thyroid cases
prop_disease <- 22/(978+22)

# Scatterplot showing TSH, T3 and anomaly labels
plot(TSH ~ T3, data = thyroid, pch = 20, col = label + 1)

# Scatterplot showing TT4, TBG and anomaly labels
plot(TT4 ~ TBG, data = thyroid, pch = 20, col = label + 1)

# Fit isolation forest
thyroid_forest <- iForest(thyroid[, -1], nt = 200, phi = 100)

# Anomaly score 
thyroid$iso_score <- predict(thyroid_forest, thyroid[, -1])

# Boxplot of the anomaly score against labels
boxplot(iso_score ~ label, thyroid, col = "olivedrab4")

# Scale the measurement columns of thyroid
scaled_thyroid_measurements <- scale(thyroid[, -1])

# Create a LOF score for the measurements
lof_score <- lof(scaled_thyroid_measurements, k = 10)
                 
# Calculate high threshold for 
high_lof <- quantile(lof_score, probs = 0.98)  

# Append binary LOF score to thyroid data
thyroid$binary_lof <- as.numeric(lof_score >= high_lof)
                 
# Calculate high threshold for iso_score
high_iso <- quantile(iso_score, probs = 0.98)  

# Append binary isolation score to thyroid data
thyroid$binary_iso <- as.numeric(iso_score >= high_iso)

# Tabulate agreement of label and binary isolation score 
table(thyroid$label, thyroid$binary_iso)

# Tabulate agreement of label and binary LOF score 
table(thyroid$label, thyroid$binary_lof)

# Proportion of binary_iso and label that agree
iso_prop <- sum(diag(table(thyroid$label, thyroid$binary_iso))) 
/ sum(table(thyroid$label, thyroid$binary_iso))

# Proportion of binary_lof and label that agree
lof_prop <- sum(diag(table(thyroid$label, thyroid$binary_lof))) 
/ sum(table(thyroid$label, thyroid$binary_lof))

# Tabulation for the binary isolation score
table(thyroid$label, thyroid$binary_iso)
# Precision for the isolation score
precision_iso <- 12 / (8 + 12)
# Recall for the isolation score
recall_iso <- 12 / (12 + 10)

# Tabulation for the binary lof score
table(thyroid$label, thyroid$binary_lof)
# Precision for the binary lof score
precision_lof <- 0 / (20 + 0)
# Recall for the binary lof score
recall_lof <- 0 / (22 + 0)

# Print the column classes in thyroid
sapply(X = thyroid, FUN = class)

# Convert column with character class to factor
thyroid$age <- as.factor(thyroid$age)
thyroid$sex <- as.factor(thyroid$sex)

# Check that all columns are factor or numeric
sapply(X = thyroid, FUN = class)

# Check the class of age column
class(thyroid$age)

# Check the class of sex column
class(thyroid$sex)

# Fit an isolation forest with 100 trees
thyroid_for <- iForest(thyroid[,-1], nt = 100)

# Calculate Gower's distance matrix
thyroid_dist <- daisy(thyroid[,-1], metric = "gower")

# Generate LOF scores for thyroid data
thyroid_lof <- lof(thyroid_dist, k = 10)

# Range of values in the distance matrix
range(as.matrix(thyroid_dist))
