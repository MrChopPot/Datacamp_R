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



