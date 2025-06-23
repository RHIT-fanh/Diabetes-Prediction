rm(list = ls())  # Clear the workspace
cat("\014")  # Clear the console
library(readr)
dataset <- readr::read_csv("Dataset.csv")

set.seed(123)  # Set seed for reproducibility
sampled_data <- dataset[sample(nrow(dataset), 10000, replace = FALSE), ]
write_csv(sampled_data, "SampledDataset_10000.csv")
