### 1. Not Mtcars Again

# Print the cars2018 object
cars2018

# Plot the histogram
ggplot(cars2018, aes(x = MPG)) +
    geom_histogram(bins = 25) +
    labs(y = "Number of cars",
         x = "Fuel efficiency (mpg)")

# Deselect the 2 columns to create cars_vars
cars_vars <- cars2018 %>%
    select(-Model, -`Model Index`)

# Fit a linear model
fit_all <- lm(MPG ~ ., data = cars_vars)

# Print the summary of the model
summary(fit_all)

# Load caret
library(caret)

# Split the data into training and test sets
set.seed(1234)
in_train <- createDataPartition(cars_vars$Transmission, p = .8, list = FALSE)
training <- cars_vars[in_train, ]
testing <- cars_vars[-in_train, ]

# Load caret
library(caret)

# Train a linear regression model
fit_lm <- train(log(MPG) ~ ., method = "lm", data = training,
                trControl = trainControl(method = "none"))

# Print the model object
fit_lm

# Train a random forest model
fit_rf <- train(log(MPG) ~ ., method = "rf", data = training,
                trControl = trainControl(method = "none"))

# Print the model object
fit_rf

# Load yardstick
library(yardstick)

# Create the new columns
results <- training %>%
    mutate(`Linear regression` = predict(fit_lm, training),
           `Random forest` = predict(fit_rf, training))

# Evaluate the performance
metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)

# Create the new columns
results <- testing%>%
    mutate(`Linear regression` = predict(fit_lm, testing),
           `Random forest` = predict(fit_rf, testing))

# Evaluate the performance
metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)

# Fit the models with bootstrap resampling
cars_lm_bt <- train(log(MPG) ~ ., method = "lm", data = training,
                   trControl = trainControl(method = "boot"))
cars_rf_bt <- train(log(MPG) ~ ., method = "rf", data = training,
                   trControl = trainControl(method = "boot"))
                   
# Quick look at the models
cars_lm_bt
cars_rf_bt

results <- testing %>%
    mutate(`Linear regression` = predict(cars_lm_bt, testing),
           `Random forest` = predict(cars_rf_bt, testing))

metrics(results, truth = MPG, estimate = `Linear regression`)
metrics(results, truth = MPG, estimate = `Random forest`)

results %>%
    gather(Method, Result, `Linear regression`:`Random forest`) %>%
    ggplot(aes(log(MPG), Result, color = Method)) +
    geom_point(size = 1.5, alpha = 0.5) +
    facet_wrap(~Method) +
    geom_abline(lty = 2, color = "gray50") +
    geom_smooth(method = "lm")

#######################

### 2. 
