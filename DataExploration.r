rm(list = ls())  # Clear the workspace
cat("\014")  # Clear the console
library(readr)
dataset <- readr::read_csv("SampledDataset_10000.csv")

head(dataset)
library(class)

library(ggplot2)
# Remove the first column 'Diabetes_012'
dataset <- dataset[, -1]

# Standardize the dataset
standardized_dataset <- scale(dataset)

# Compute the correlation matrix
correlation_matrix <- cor(standardized_dataset)

# Apply PCA to the standardized dataset
pca_result <- prcomp(standardized_dataset, center = TRUE, scale. = TRUE)

# Display the proportion of variance explained by each principal component
variance_explained <- summary(pca_result)$importance[2, ]
print(variance_explained)

# Display the correlation matrix
print(correlation_matrix)