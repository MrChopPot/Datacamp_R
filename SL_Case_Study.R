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

### 2. Stack Overflow Developer Survey

# Print stackoverflow
stackoverflow

# First count for Remote
stackoverflow %>% 
    count(Remote, sort = TRUE)

# then count for Country
stackoverflow %>% 
    count(Country, sort = TRUE)

ggplot(stackoverflow, aes(x = Remote, y = YearsCodedJob)) +
    geom_boxplot() +
    labs(x = NULL,
         y = "Years of professional coding experience") 

# Build a simple logistic regression model
simple_glm <- stackoverflow %>%
        select(-Respondent) %>%
        glm(Remote ~ .,
            family = "binomial",
            data = .)

# Print the summary of the model
summary(simple_glm)

# Load caret
library(caret)

# Create stack_select dataset
stack_select <- stackoverflow %>%
    select(-Respondent)

# Split the data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(stack_select$Remote, p = .8, list = FALSE)
training <- stack_select[in_train,]
testing <- stack_select[-in_train,]

# Create the upsampled training set
up_train <- upSample(x = select(training, -Remote),
                     y = training$Remote,
                     yname = "Remote") %>%
    as_tibble()

# Count the number of each type of Remote employee
up_train %>%
    count(Remote)

# Build a logistic regression model
stack_glm <- train(Remote ~ ., method = "glm", family = "binomial",
                   data = training,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Print the model object 
stack_glm

# Build a random forest model
stack_rf <- train(Remote ~ ., method = "rf", 
                  data = training,
                  trControl = trainControl(method = "boot",
                                           sampling = "up"))

# Print the model object
stack_rf

# Set seed
set.seed(123)

# Confusion matrix for logistic regression model
confusionMatrix(predict(stack_glm, testing),
                testing$Remote)

# Set seed
set.seed(123)

# Confusion matrix for random forest model
confusionMatrix(predict(stack_rf, testing),
                testing$Remote)

# Load yardstick
library(yardstick)

# Predict values
testing_results <- testing %>%
    mutate(`Logistic regression` = predict(stack_glm, testing),
           `Random forest` = predict(stack_rf, testing))

## Calculate accuracy
accuracy(testing_results, truth = Remote, estimate = `Logistic regression`)
accuracy(testing_results, truth = Remote, estimate = `Random forest`)

## Calculate positive predict value
ppv(testing_results, truth = Remote, estimate = `Logistic regression`)
ppv(testing_results, truth = Remote, estimate = `Random forest`)



