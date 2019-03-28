### Machine Learning Toolbox

### 1. Regression models: fitting them and evaluating their performance

# Fit lm model: model
model <- lm(price ~ ., data = diamonds)

# Predict on full data: p
p <- predict(model)

# Compute errors: error
error <- p - diamonds$price

# Calculate RMSE
sqrt(mean(error^2))

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows <- sample(nrow(diamonds))

# Randomly order data
diamonds <- diamonds[rows,]

# Determine row to split on: split
split <- round(nrow(diamonds) * .80)

# Create train
train <- diamonds[1:split,]

# Create test
test <- diamonds[(split + 1):nrow(diamonds), ]

# Fit lm model on train: model
model <- lm(price ~ ., data = train)

# Predict on test: p
p <- predict(model, newdata = test)

# Compute errors: error
error <- p - test$price

# Calculate RMSE
sqrt(mean(error^2))

# Fit lm model using 10-fold CV: model
model <- train(
  price ~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
model

# Fit lm model using 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = T
  )
)

# Print model to console
model

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)

# Print model to console
model

# Predict on full Boston dataset
predict(model, Boston)

#######################

### 2. Classification models: fitting them and evaluating their performance

# Shuffle row indices: rows
rows <- sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar <- Sonar[rows,]

# Identify row to split on: split
split <- round(nrow(Sonar) * .6)

# Create train
train <- Sonar[1:split, ]

# Create test
test <- Sonar[(split + 1):nrow(Sonar), ]

# Fit glm model: model
model <- glm(Class ~ ., train, family = "binomial")

# Predict on test: p
p <- predict(model, test, type = "response")

# If p exceeds threshold of 0.5, M else R: m_or_r
m_or_r <- ifelse(p > .5, "M", "R")

# Convert to factor: p_class
p_class <- as.factor(m_or_r)

# Create confusion matrix
confusionMatrix(p_class, test$Class)

# Predict on test: p
p <- predict(model, test, type = "response")

# Make ROC curve
colAUC(p, test$Class, plotROC = TRUE)

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model <- train(Class ~ ., Sonar, method = "glm", trControl = myControl)


# Print model to console
model

#######################

### 3. Tuning model parameters to improve performance

# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 1,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 3,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Plot model
plot(model)

# From previous step
tuneGrid <- data.frame(
  .mtry = c(2, 3, 7),
  .splitrule = "variance",
  .min.node.size = 5
)

# Fit random forest: model
model <- train(
  quality ~ .,
  tuneGrid = tuneGrid,
  data = wine, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE
  )
)

# Print model to console
model

# Plot model
plot(model)

# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T, # IMPORTANT!
  verboseIter = TRUE
)

# Fit glmnet model: model
model <- train(
  y ~ ., overfit,
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic
max(model$results$ROC)

# Train glmnet with custom trainControl and tuning: model
model <- train(
  y ~ ., overfit,
  tuneGrid = expand.grid(alpha = 0:1,
  lambda = seq(0.0001, 1, length = 20)),
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic
max(model$results$ROC)

#######################

### 4. Preprocessing your data

# Apply median imputation: model
model <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print model to console
model

# Apply KNN imputation: model2
model2 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print model to console
model2

# Update model with standardization
model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print updated model
model

# Update model with standardization
model <- train(
  x = breast_cancer_x, 
  y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print updated model
model

# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(bloodbrain_x, names = TRUE, 
                           freqCut = 2, uniqueCut = 20)

# Get all column names from bloodbrain_x: all_cols
all_cols <- names(bloodbrain_x)

# Remove from data: bloodbrain_x_small
bloodbrain_x_small <- bloodbrain_x[ , setdiff(all_cols, remove_cols)]

# Fit model on reduced data: model
model <- train(x = bloodbrain_x_small, y = bloodbrain_y, method = "glm")

# Print model to console
model

# Fit glm model using PCA: model
model <- train(
  x = bloodbrain_x, y = bloodbrain_y,
  method = "glm", preProcess = c("pca")
)

# Print model to console
model

#######################

### 5. Selecting models: a case study in churn prediction

# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)

# Fit random forest: model_rf
model_rf <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)

# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Create bwplot
bwplot(resamples, metric = "ROC")

# Create xyplot
xyplot(resamples, metric = "ROC")

# Create ensemble model: stack
stack <- caretStack(model_list, method = "glm")

# Look at summary
summary(stack)

