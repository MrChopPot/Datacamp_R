### Support Vector Machine in R

### 1. Introduction

# Load ggplot2
library(ggplot2)

# Print variable names
print(colnames(df))

# Plot sugar content along the x-axis
plot_df <- ggplot(data = df, aes(x = sugar_content, y = 0)) + 
    geom_point() + 
    geom_text(aes(label = sugar_content), size = 2.5, vjust = 2, hjust = 0.5)

# Display plot
plot_df

#The maximal margin separator is at the midpoint of the two extreme points in each cluster.
mm_separator <- (8.9 + 10)/2

#create data frame containing the maximum margin separator
separator <- data.frame(sep = mm_separator)

#add ggplot layer 
plot_sep <- plot_ + geom_point(data = separator, aes(x = sep, y = 0), color = "blue", size = 4)

#display plot
plot_sep

#set seed
set.seed(42)

#set number of data points. 
n <- 600

#Generate data frame with two uniformly distributed predictors lying between 0 and 1.
df <- data.frame(x1 = runif(600), 
                 x2 = runif(600))

#classify data points depending on location
df$y <- factor(ifelse(df$x2 - 1.4*df$x1 < 0, -1, 1), 
    levels = c(-1, 1))

#set margin
delta <- .07

# retain only those points that lie outside the margin
df1 <- df[abs(1.4*df$x1 - df$x2) > delta, ]

#build plot
plot_margins <- ggplot(data = df1, aes(x = x1, y = x2, color = y)) + geom_point() + 
    scale_color_manual(values = c("red", "blue")) + 
    geom_abline(slope = 1.4, intercept = 0)+
    geom_abline(slope = 1.4, intercept = delta, linetype = "dashed") +
    geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed")
 
#display plot 
plot_margins

### 2. Linear Kernel

#split train and test data in an 80/20 proportion
df[, "train"] <- ifelse(runif(nrow(df))<=.8, 1, 0)

#assign training rows to data frame trainset
trainset <- df[df$train == 1, ]
#assign test rows to data frame testset
testset <- df[df$train == 0, ]

#find index of "train" column
trainColNum <- grep("train", names(df))

#remove "train" column from train and test dataset
trainset <- trainset[, -trainColNum]
testset <- testset[, -trainColNum]

library(e1071)

#build svm model, setting required parameters
svm_model<- svm(y ~ ., 
                data = trainset, 
                type = "C-classification", 
                kernel = "linear", 
                scale = FALSE)

#list components of model
names(svm_model)

#list values of the SV, index and rho
svm_model$SV
svm_model$index
svm_model$rho

#compute training accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)

#compute test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#load ggplot
library(ggplot2)

#build scatter plot of training dataset
scatter_plot <- ggplot(data = trainset, aes(x = x1, y = x2, color = y)) + 
    geom_point() + 
    scale_color_manual(values = c("red", "blue"))
 
#add plot layer marking out the support vectors 
layered_plot <- 
    scatter_plot + geom_point(data = trainset[svm_model$index, ], aes(x = x1, y = x2), color = "purple", size = 4, alpha = 0.5)

#display plot
layered_plot

#calculate slope and intercept of decision boundary from weight vector and svm model
slope_1 <- -w[1]/w[2]
intercept_1 <- svm_model$rho/w[2]

#build scatter plot of training dataset
scatter_plot <- ggplot(data = trainset, aes(x = x1, y = x2, color = y)) + 
    geom_point() + scale_color_manual(values = c("red", "blue"))
#add decision boundary 
plot_decision <- scatter_plot + geom_abline(slope = slope_1, intercept = intercept_1) 
#add margin boundaries
plot_margins <- plot_decision + 
 geom_abline(slope = slope_1, intercept = intercept_1 - 1/w[2], linetype = "dashed")+
 geom_abline(slope = slope_1, intercept = intercept_1 + 1/w[2], linetype = "dashed")
#display plot
plot_margins

#load required library
library(e1071)

#build svm model
svm_model<- 
    svm(y ~ ., data = trainset, type = "C-classification", 
        kernel = "linear", scale = FALSE)

#plot decision boundaries and support vectors for the training data
plot(x = svm_model, data = trainset)

#load required library
library(e1071)

#build svm model
svm_model<- 
    svm(y ~ ., data = trainset, type = "C-classification", 
        kernel = "linear", scale = FALSE)

#plot decision boundaries and support vectors for the training data
plot(x = svm_model, data = trainset)

#build svm model, cost = 1
svm_model_1 <- svm(y ~ .,
                   data = trainset,
                   type = "C-classification",
                   cost = 1,
                   kernel = "linear",
                   scale = FALSE)

#print model details
svm_model_1

#build svm model, cost = 1
svm_model_100 <- svm(y ~ .,
                   data = trainset,
                   type = "C-classification",
                   cost = 100,
                   kernel = "linear",
                   scale = FALSE)

#print model details
svm_model_100

#add decision boundary and margins for cost = 1 to training data scatter plot
train_plot_with_margins <- train_plot + 
    geom_abline(slope = slope_1, intercept = intercept_1) +
    geom_abline(slope = slope_1, intercept = intercept_1-1/w_1[2], linetype = "dashed")+
    geom_abline(slope = slope_1, intercept = intercept_1+1/w_1[2], linetype = "dashed")

#display plot
train_plot_with_margins

#add decision boundary and margins for cost = 100 to training data scatter plot
train_plot_with_margins <- train_plot_100 + 
    geom_abline(slope = slope_100, intercept = intercept_100, color = "goldenrod") +
    geom_abline(slope = slope_100, intercept = intercept_100-1/w_100[2], linetype = "dashed", color = "goldenrod")+
    geom_abline(slope = slope_100, intercept = intercept_100+1/w_100[2], linetype = "dashed", color = "goldenrod")

#display plot 
train_plot_with_margins

#load library and build svm model
library(e1071)
svm_model<- 
    svm(y ~ ., data = trainset, type = "C-classification", 
        kernel = "linear", scale = FALSE)

#compute training accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)

#compute test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#plot
plot(svm_model, trainset)

for (i in 1:100){
    #assign 80% of the data to the training set
    iris[, "train"] <- ifelse(runif(nrow(iris)) < .8, 1, 0)
    trainColNum <- grep("train", names(iris))
    trainset <- iris[iris$train == 1, -trainColNum]
    testset <- iris[iris$train == 0, -trainColNum]
    #build model using training data
    svm_model <- svm(Species~ ., data = trainset, 
                     type = "C-classification", kernel = "linear")
    #calculate accuracy on test data
    pred_test <- predict(svm_model, testset)
    accuracy[i] <- mean(pred_test == testset$Species)
}
mean(accuracy) 
sd(accuracy)














