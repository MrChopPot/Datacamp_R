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

#######################

### 3. Get out the vote

# Load tidyverse
library(tidyverse)

# Print voters
voters

# How many people voted?
voters %>%
    count(turnout16_2016)

# How do the reponses on the survey vary with voting behavior?
voters %>%
    group_by(turnout16_2016) %>%
    summarise(`Elections dont matter` = mean(RIGGED_SYSTEM_1_2016 <= 2),
              `Economy is getting better` = mean(econtrend_2016 == 1),
              `Crime is very important` = mean(imiss_a_2016 == 2))

# Visualize difference by voter turnout
voters %>%
    ggplot(aes(econtrend_2016, ..density.., fill = turnout16_2016)) +
    geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
    labs(title = "Overall, is the economy getting better or worse?")

# Remove the case_indetifier column
voters_select <- voters %>%
        select(-case_identifier)

# Build a simple logistic regression model
simple_glm <- glm(turnout16_2016 ~ .,  family = "binomial", 
                  data = voters_select)

# Print the summary                  
summary(simple_glm)

# Load caret
library(caret)

# Split data into training and testing sets
set.seed(1234)
in_train <- createDataPartition(voters_select$turnout16_2016, 
                p = .8, list = FALSE)
training <- voters_select[in_train,]
testing <- voters_select[-in_train,]

# Perform logistic regression with upsampling and no resampling
vote_glm <- train(turnout16_2016 ~ ., method = "glm", family = "binomial",
                  data = training,
                  trControl = trainControl(method = "none",
                                           sampling = "up"))

# Logistic regression
vote_glm <- train(turnout16_2016 ~ ., method = "glm", family = "binomial",
                  data = training,
                  trControl = trainControl(method = "repeatedcv",
                                           repeats = 2,
                                           sampling = "up"))

# Print vote_glm
vote_glm

# Random forest
vote_rf <- train(turnout16_2016 ~ ., method = "rf", 
                 data = training,
                 trControl = trainControl(method = "repeatedcv",
                                          repeats = 2,
                                          sampling = "up"))

# Print vote_rf
vote_rf

# Confusion matrix for logistic regression model on training data
confusionMatrix(predict(vote_glm, training),
                training$turnout16_2016)

# Confusion matrix for random forest model on training data
confusionMatrix(predict(vote_rf, training),
                training$turnout16_2016)

# Confusion matrix for logistic regression model on testing data
confusionMatrix(predict(vote_glm, testing),
    testing$turnout16_2016)

# Confusion matrix for random forest model on testing data
confusionMatrix(predict(vote_rf, testing),
    testing$turnout16_2016)

#######################

### 4. But what do the nuns think? (Survey Data)

# Load tidyverse
library(tidyverse)

# View sisters67
glimpse(sisters67)

# Plot the histogram
ggplot(sisters67, aes(x = age)) +
    geom_histogram(binwidth = 10)

# Print the structure of sisters67
glimpse(sisters67)

# Tidy the data set
tidy_sisters <- sisters67 %>%
    select(-sister) %>%
    gather(key, value, -age)

# Print the structure of tidy_sisters
glimpse(tidy_sisters)

# Overall agreement with all questions varied by age
tidy_sisters %>%
    group_by(age) %>%
    summarize(value = mean(value, na.rm = TRUE))

# Number of respondents agreed or disagreed overall
tidy_sisters %>%
    count(value)

# Visualize agreement with age
tidy_sisters %>%
    filter(key %in% paste0("v", 153:170)) %>%
    group_by(key, value) %>%
    summarize(age = mean(age, na.rm = TRUE)) %>%
    ggplot(aes(value, age, color = key)) +
    geom_line(show.legend = FALSE) +
    facet_wrap(~key, nrow = 3)

# Remove the sister column
sisters_select <- sisters67 %>% 
    select(-sister)

# Build a simple linear regression model
simple_lm <- lm(age ~ ., 
                data = sisters_select)

# Print the summary of the model
summary(simple_lm)

# Split the data into training and validation/test sets
set.seed(1234)
in_train <- createDataPartition(sisters_select$age, 
                                p = .6, list = FALSE)
training <- sisters_select[in_train, ]
validation_test <- sisters_select[-in_train, ]

# Split the validation and test sets
set.seed(1234)
in_test <- createDataPartition(validation_test$age, 
                               p = .5, list = FALSE)
testing <- validation_test[in_test, ]
validation <- validation_test[-in_test, ]

# Load caret
library(caret)

# Fit a CART model
sisters_cart <- train(age ~ ., method = "rpart", data = training)

# Print the CART model
sisters_cart

# Make predictions on the three models
modeling_results <- validation %>%
    mutate(CART = predict(sisters_cart, validation),
           XGB = predict(sisters_xgb, validation),
           GBM = predict(sisters_gbm, validation))

# View the predictions
modeling_results %>% 
    select(CART, XGB, GBM)

# Load yardstick
library(yardstick)

# Compare performace
metrics(modeling_results, truth = age, estimate = CART)
metrics(modeling_results, truth = age, estimate = XGB)
metrics(modeling_results, truth = age, estimate = GBM)

# Calculate RMSE
testing %>%
    mutate(prediction = predict(sisters_gbm, testing)) %>%
    rmse(truth = age, estimate = prediction)

