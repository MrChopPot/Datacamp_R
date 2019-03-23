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


