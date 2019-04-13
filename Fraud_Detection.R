### Fraud Detection in R

### 1. Introduction & Motivation

## Data Viz

# Print the first 6 rows of the dataset
head(transfers)

# Display the structure of the dataset
str(transfers)

# Determine fraction of legitimate and fraudulent cases
class_distribution <- prop.table(table(transfers$fraud_flag))
print(class_distribution)

# Make pie chart of column fraud_flag
df <- data.frame(class = c("no fraud", "fraud"), 
                 pct = as.numeric(class_distribution)) %>%
  mutate(class = factor(class, levels = c("no fraud", "fraud")),
         cumulative = cumsum(pct), midpoint = cumulative - pct / 2,
         label = paste0(class, " ", round(pct*100, 2), "%"))

ggplot(df, aes(x = 1, weight = pct, fill = class)) +
  scale_fill_manual(values = c("dodgerblue", "red")) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = midpoint, label = label)) +
  theme_nothing()

# Create vector predictions containing 0 for every transfer
predictions <- factor(rep.int(0, nrow(transfers)), levels = c(0, 1))

# Compute confusion matrix
confusionMatrix(data = predictions, reference = transfers$fraud_flag)

# Compute cost of not detecting fraud
cost <- sum(transfers$amount[transfers$fraud_flag == 1])
print(cost)

## Time Features

# Convert the plain text to hours
ts <- as.numeric(hms(timestamps)) / 3600

# Convert the data to class circular
ts <- circular(ts, units = "hours", template = "clock24")

# Plot a circular histogram
clock <- ggplot(data.frame(ts), aes(x = ts)) +
  geom_histogram(breaks = seq(0, 24), colour = "blue", fill = "lightblue") +
  coord_polar() + scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24))
plot(clock)

# Convert the plain text to hours
ts <- as.numeric(hms(timestamps)) / 3600

# Convert the data to class circular
ts <- circular(ts, units = "hours", template = "clock24")

# Estimate the periodic mean from the von Mises distribution 
# (generalized from Gaussian & only for unimodal)
estimates <- mle.vonmises(ts)

# Extract the periodic mean from the estimates
p_mean <- estimates$mu %% 24

# Add the periodic mean to the circular histogram
clock <- ggplot(data.frame(ts), aes(x = ts)) +
  geom_histogram(breaks = seq(0, 24), colour = "blue", fill = "lightblue") +
  coord_polar() + scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24)) +
  geom_vline(xintercept = as.numeric(p_mean), color = "red", linetype = 2, size = 1.5)

plot(clock)

# Estimate the periodic mean and concentration on the first 24 timestamps
p_mean <- estimates$mu %% 24
concentration <- estimates$kappa

# Estimate densities of all 25 timestamps
densities <- dvonmises(ts, mu = p_mean, kappa = concentration)

# Check if the densities are larger than the cutoff of 95%-CI
cutoff <- dvonmises(qvonmises((1 - alpha)/2, mu = p_mean, kappa = concentration), mu = p_mean, kappa = concentration)

# Define the variable time_feature
time_feature <- densities >= cutoff
print(cbind.data.frame(ts, time_feature))

## Frequency Features

# Frequency feature based on channel_cd
frequency_fun <- function(steps, channel) {
  n <- length(steps)
  frequency <- sum(channel[1:n] == channel[n + 1])
  return(frequency)
}

# Create freq_channel feature
freq_channel <- rollapply(trans_Bob$transfer_id, width = list(-1:-length(trans_Bob$transfer_id)), 
  partial = T, FUN = frequency_fun, trans_Bob$channel_cd)

# Print the features channel_cd, freq_channel and fraud_flag next to each other
freq_channel <- c(0, freq_channel)
cbind.data.frame(trans_Bob$channel_cd, freq_channel, trans_Bob$fraud_flag)

# Group the data by account
trans <- trans %>% group_by(account_name) %>%
  # Mutate the data to add a new feature
  mutate(freq_channel = c(0,
                       # Rollapply frequency_fun to the data
                       rollapply(transfer_id,
                                 width = list(-1:-length(transfer_id)),
                                 partial = TRUE,
                                 FUN = frequency_fun, channel_cd)))
# Print the features as columns next to each other
as.data.frame(trans %>% select(account_name, channel_cd, freq_channel, fraud_flag))

## Recency Features

# Create the recency function
recency_fun <- function(t, gamma, channel_cd, freq_channel) {
  n_t <- length(t)
  if (freq_channel[n_t] == 0) {
    return(0)
  } else {
    time_diff <- t[1] - max(t[2:n_t][channel_cd[(n_t-1):1] == channel_cd[n_t]])
    exponent <- -gamma * time_diff
    return(exp(exponent))
  }
}

# Group, mutate and rollapply
trans <- trans %>% group_by(account_name) %>%
  mutate(rec_channel = rollapply(timestamp, width = list(0:-length(transfer_id)), partial = TRUE,
                                 FUN = recency_fun, gamma, channel_cd, freq_channel))

# Print a new dataframe
as.data.frame(trans %>% select(account_name, channel_cd, timestamp, rec_channel, fraud_flag))

library(dplyr)

# Statistics of frequency & recency features of legitimate transactions:
summary(transfers %>% filter(fraud_flag == 0) %>% select(freq_channel, freq_auth, rec_channel, rec_auth))

# Statistics of frequency & recency features of fraudulent transactions:
summary(transfers %>% filter(fraud_flag == 1) %>% select(freq_channel, freq_auth, rec_channel, rec_auth))

#######################

### 2. Social network analytics

## Social network analytics

# Load the igraph library
library(igraph)

# Have a look at the data
head(transfers)

# Create an undirected network from the dataset
net <- graph_from_data_frame(transfers, directed = F)

# Plot the network with the vertex labels in bold and black
plot(net,
     vertex.label.color = "black",
     vertex.label.font = 2)

# Load igraph and create a network from the data frame
net <- graph_from_data_frame(edges, directed = FALSE)

# Plot the network with the multiple edges
plot(net, layout = layout_in_circle)

# Specify new edge attributes width and curved
E(net)$width <- count.multiple(net)
E(net)$curved <- FALSE

# Check the new edge attributes and plot the network with overlapping edges
edge_attr(net)
plot(net, layout = layout_in_circle)

## Fraud and social network analysis

# Add account_type as an attribute to the nodes of the network
V(net)$account_type <- account_info$type

# Have a look at the vertex attributes
print(vertex_attr(net))

# Check for homophily based on account_type
assortativity_nominal(net, types = V(net)$account_type, directed = FALSE)

# Each account type is assigned a color
vertex_colors <- c("grey", "lightblue", "darkorange")

# Add attribute color to V(net) which holds the color of each node depending on its account_type
V(net)$color <- vertex_colors[V(net)$account_type]

# Plot the network
plot(net)

## Social network based inference

# From data frame to graph
net <- graph_from_data_frame(transfers, directed = FALSE)

# Plot the network; color nodes according to isMoneyMule-variable
V(net)$color <- ifelse(account_info$isMoneyMule, "darkorange", "slateblue1")
plot(net, vertex.label.color = "black", vertex.label.font = 2, vertex.size = 18)

# Find the id of the money mule accounts
print(account_info$id[account_info$isMoneyMule == TRUE])

# Create subgraph containing node "I41" and all money mules nodes
subnet <- subgraph(net, v = c("I41", "I47", "I87", "I20"))

# Compute the money mule probability of node "I41" based on the neighbors
strength(subnet, v = "I41") / strength(net, v = "I41")

## Social network metrics

# Plot network kite
plot(kite)

# Find the degree of each node
degree(kite)

# Which node has the largest degree?
which.max(degree(kite))

# Plot kite with vertex.size proportional to the degree of each node
plot(kite, vertex.size = 6 * degree(kite))

# Find the closeness of each node
closeness(kite)

# Which node has the largest closeness?
which.max(closeness(kite))

# Plot kite with vertex.size proportional to the closeness of each node
plot(kite, vertex.size = 500 * closeness(kite))

# Find the betweenness of each node
betweenness(kite)

# Which node has the largest betweenness?
which.max(betweenness(kite))

# Plot kite with vertex.size proportional to the betweenness of each node
plot(kite, vertex.size = 5 * betweenness(kite))

# Plot network and print account info
plot(net)
legend("bottomleft", legend = c("known money mule", "legit account"), fill = c("darkorange", "lightblue"), bty = "n")
print(account_info)

# Degree
account_info$degree <- degree(net, normalized = TRUE)

# Closeness
account_info$closeness <- closeness(net, normalized = TRUE)

# Betweenness
account_info$betweenness <- betweenness(net, normalized = TRUE)

print(account_info)

#######################

### 3. Imbalanced class distributions

# Make a scatter plot
ggplot(transfers, aes(x = amount, y = orig_balance_before)) +
  geom_point(aes(color = fraud_flag, shape = fraud_flag)) +
  scale_color_manual(values = c('dodgerblue', 'red'))

# Load ROSE
library(ROSE)

# Calculate the required number of cases in the over-sampled dataset
n_new <- as.numeric(table(creditcard$Class))[1] / 0.6667

# Over-sample
oversampling_result <- ovun.sample(formula = Class ~ ., data = creditcard,method = "over", N = n_new, seed = 2018)

# Verify the Class-balance of the over-sampled dataset
oversampled_credit <- oversampling_result$data
prop.table(table(oversampled_credit$Class))

# Load ROSE
library(ROSE)

# Calculate the required number of cases in the over-sampled dataset
n_new <- as.numeric(table(creditcard$Class))[2] / 0.4

# Under-sample
undersampling_result <- ovun.sample(formula = Class ~ ., data = creditcard, method = "under", N = n_new, seed = 2018)

# Verify the Class-balance of the under-sampled dataset
undersampled_credit <- undersampling_result$data
prop.table(table(under_credit$Class))

# Load ROSE
library(ROSE)

# Specify the desired number of cases in the balanced dataset and the fraction of fraud cases
n_new <- 10000
fraud_fraction <- 0.3

# Combine ROS & RUS!
sampling_result <- ovun.sample(formula = Class ~ ., data = creditcard, method = "both", N = n_new,  p = fraud_fraction, seed = 2018)

# Verify the Class-balance of the re-balanced dataset
sampled_credit <- sampling_result$data
prop.table(table(sampled_credit$Class))

# Set the number of fraud and legitimate cases, and the desired percentage of legitimate cases
n0 <- nrow(creditcard[creditcard$Class==0,]); n1 <- nrow(creditcard[creditcard$Class==1,]); r0 <- 0.6

# Calculate the value for the dup_size parameter of SMOTE
ntimes <- ((1 - r0) / r0) * (n0 / n1) - 1

# Create synthetic fraud cases with SMOTE
smote_output <- SMOTE(X = creditcard[ , -c(1, 31, 32)], target = creditcard$Class, K = 5, dup_size = ntimes)

# Make a scatter plot of the original and over-sampled dataset
credit_smote <- smote_output$data
colnames(credit_smote)[30] <- "Class"
prop.table(table(credit_smote$Class))

ggplot(creditcard, aes(x = V1, y = V2, color = Class)) +
  geom_point() +
  scale_color_manual(values = c('dodgerblue2', 'red'))

ggplot(credit_smote, aes(x = V1, y = V2, color = Class)) +
  geom_point() +
  scale_color_manual(values = c('dodgerblue2', 'red'))

# Train the rpart algorithm on the original training set and the SMOTE-rebalanced training set
model_orig <- rpart(Class ~ ., data = train_original)
model_smote <- rpart(Class ~ ., data = train_oversampled)

# Predict the fraud probabilities of the test cases
scores_orig <- predict(model_orig, newdata = test, type = "prob")[, 2]
scores_smote <- predict(model_smote, newdata = test, type = "prob")[, 2]

# Convert the probabilities to classes (0 or 1) using a cutoff value
predicted_class_orig <- factor(ifelse(scores_orig > 0.5, 1, 0))
predicted_class_smote <- factor(ifelse(scores_smote > 0.5, 1, 0))

# Determine the confusion matrices and the model's accuracy
CM_orig <- confusionMatrix(data = predicted_class_orig, reference = test$Class)
CM_smote <- confusionMatrix(data = predicted_class_smote, reference = test$Class)
print(CM_orig$table)
print(CM_orig$overall[1])
print(CM_smote$table)
print(CM_smote$overall[1])

# Define the cost_model function to calculate cost
cost_model <- function(predicted.classes, true.classes, amounts, fixedcost) {
  library(hmeasure)
  predicted.classes <- relabel(predicted.classes)
  true.classes <- relabel(true.classes)
  cost <- sum(true.classes * (1 - predicted.classes) * amounts + predicted.classes * fixedcost)
  return(cost)
}

# Calculate the total cost of deploying the original model
cost_model(predicted_class_orig, test$Class, test$Amount, 10)

# Calculate the total cost of deploying the model using SMOTE
cost_model(predicted_class_smote, test$Class, test$Amount, 10)

#######################

### 4. Digit analysis and robust statistics

# Implement Benford's Law for first digit
benlaw <- function(d) log10(1 + 1 / d)

# Calculate expected frequency for d=5
benlaw(5)

# Create a dataframe of the 9 digits and their Benford's Law probabilities
df <- data.frame(digit = 1:9, probability = benlaw(1:9))

# Create barplot with expected frequencies
ggplot(df, aes(x = digit, y = probability)) + 
  geom_bar(stat = "identity", fill = "dodgerblue") + 
  xlab("First digit") + ylab("Expected frequency") + 
  scale_x_continuous(breaks = 1:9, labels = 1:9) + 
  ylim(0, 0.33) + theme(text = element_text(size = 25))

# Load package benford.analysis
library(benford.analysis)
data(census.2009)

# Check conformity
bfd.cen <- benford(census.2009$pop.2009, number.of.digits = 1) 
plot(bfd.cen, except = c("second order", "summation", "mantissa", 
  "chi squared","abs diff", "ex summation", "Legend"), multiple = F) 

# Multiply the data by 3 and check conformity again
data <- census.2009$pop.2009 * 3
bfd.cen3 <- benford(data, number.of.digits = 1)
plot(bfd.cen3, except = c("second order", "summation", "mantissa", 
  "chi squared","abs diff", "ex summation", "Legend"), multiple = F)

# Validate data against Benford's Law using first digit
bfd.ins <- benford(fireinsuranceclaims, number.of.digits = 1) 
plot(bfd.ins, except=c("second order", "summation", "mantissa", 
  "chi squared","abs diff", "ex summation", "Legend"), multiple = F)

# Validate data against Benford's Law using first-two digits
bfd.ins2 <- benford(fireinsuranceclaims, number.of.digits = 2)
plot(bfd.ins2, except=c("second order", "summation", "mantissa", 
  "chi squared","abs diff", "ex summation", "Legend"), multiple = F)

# Validate data against Benford's Law using first digit
bfd.exp <- benford(expensesCEO, number.of.digits = 1) 
plot(bfd.exp, except=c("second order", "summation", "mantissa", 
  "chi squared","abs diff", "ex summation", "Legend"), multiple = F)

# Validate data against Benford's Law using first-two digits
bfd.exp2 <- benford(expensesCEO, number.of.digits = 2) 
plot(bfd.exp2, except=c("second order", "summation", "mantissa", 
  "chi squared","abs diff", "ex summation", "Legend"), multiple = F)

# Get observations identified as fraud
which(transfers$fraud_flag == 1)

# Compute median and mean absolute deviation for `amount`
m <- median(transfers$amount)
s <- mad(transfers$amount)

# Compute robust z-score for each observation
robzscore <- abs((transfers$amount - m) / (s))

# Get observations with robust z-score higher than 3 in absolute value
which(abs(robzscore) > 3)

# Create boxplot
bp.thexp <- boxplot(thexp, col = "lightblue", main = "Standard boxplot", ylab = "Total household expenditure")

# Extract the outliers from the data
bp.thexp$out

# Create adjusted boxplot
adj.thexp <- adjbox(thexp, col = "lightblue", main = "Adjusted boxplot", ylab = "Total household expenditure")

# Create a scatterplot
plot(hailinsurance, xlab = "price house", ylab = "claim")

# Compute the sample mean and sample covariance matrix
clcenter <- colMeans(hailinsurance)
clcov <- cov(hailinsurance)

# Add 97.5% tolerance ellipsoid
rad <- sqrt(qchisq(0.975, 2))
ellipse(center = clcenter, shape = clcov, radius = rad, col = "blue", lty = 2)

# Create a scatterplot of the data
plot(hailinsurance, xlab = "price house", ylab = "claim")

# Compute robust estimates for location and scatter
mcdresult <- covMcd(hailinsurance)
robustcenter <- mcdresult$center
robustcov <- mcdresult$cov

# Add robust 97.5% tolerance ellipsoid
rad <- sqrt(qchisq(0.975, 2))
ellipse(center = robustcenter, shape = robustcov, radius = rad, col = "red")
