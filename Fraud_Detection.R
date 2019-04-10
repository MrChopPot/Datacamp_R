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
freq_channel <- rollapply(trans_Bob$transfer_id, width = list(-1:-length(trans_Bob$transfer_id)), partial = T, FUN = frequency_fun, trans_Bob$channel_cd)

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
























