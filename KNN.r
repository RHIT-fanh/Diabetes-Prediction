rm(list = ls())  # Clear the workspace
cat("\014")  # Clear the console
library(readr)
dataset <- readr::read_csv("SampledDataset_10000.csv")

head(dataset)
library(class)


# Prepare the data
set.seed(123)
dataset <- na.omit(dataset)  # Remove rows with missing values
split <- sample(1:nrow(dataset), size = 0.7 * nrow(dataset))
train_data <- dataset[split, ]
test_data <- dataset[-split, ]

# Define features and target variable
train_features <- train_data[, -1]  # Exclude the target column
train_target <- train_data$Diabetes_012
test_features <- test_data[, -1]
test_target <- test_data$Diabetes_012

# Baseline model: Predict the most frequent class
baseline_prediction <- rep(which.max(table(train_target)), length(test_target))

# Evaluate baseline model
baseline_confusion_matrix <- table(Predicted = baseline_prediction, Actual = test_target)
print(baseline_confusion_matrix)

baseline_accuracy <- sum(diag(baseline_confusion_matrix)) / sum(baseline_confusion_matrix)
cat("Baseline Accuracy:", baseline_accuracy)



# Standardize/Normalize the features
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

train_features <- as.data.frame(lapply(train_features, normalize))
test_features <- as.data.frame(lapply(test_features, normalize))

# Apply PCA to reduce dimensionality
library(stats)

pca_result <- prcomp(train_features, center = TRUE, scale. = TRUE)
summary(pca_result)

# Select the number of principal components to retain (e.g., 95% variance explained)
explained_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
num_components <- which(explained_variance >= 0.95)[1]
cat("Number of components to retain:", num_components, "\n")

# Transform the features using the selected principal components
train_features_pca <- as.data.frame(pca_result$x[, 1:num_components])
test_features_pca <- as.data.frame(predict(pca_result, newdata = test_features)[, 1:num_components])

# Perform cross-validation to select the optimal K value
set.seed(123)
k_values <- seq(1, 50, by = 1)  # Range of K values to test
accuracy_results <- numeric(length(k_values))

for (i in seq_along(k_values)) {
    k <- k_values[i]
    predictions <- knn(train = train_features_pca, test = test_features_pca, cl = train_target, k = k)
    confusion_matrix <- table(Predicted = predictions, Actual = test_target)
    accuracy_results[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

# Find the optimal K value
optimal_k <- k_values[which.max(accuracy_results)]
cat("Optimal K:", optimal_k, "\n")

# Apply KNN classifier with the optimal K value
predictions <- knn(train = train_features, test = test_features, cl = train_target, k = optimal_k)

# Evaluate the model
confusion_matrix <- table(Predicted = predictions, Actual = test_target)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy with optimal K:", accuracy, "\n")

if (!requireNamespace("gmodels", quietly = TRUE)) {
    install.packages("gmodels")
}

library(gmodels)

CrossTable(predictions, test_target, prop.chisq = FALSE)
