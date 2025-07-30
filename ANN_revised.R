# Step 0: Clear environment and console
rm(list = ls())
cat("\014")

# Set working directory to your own path
setwd("D:/OneDrive - Rose-Hulman Institute of Technology/Rose-Hulman/course/CSSE/CSSE286/git_project/Diabetes-Prediction")

# Step 1: Read the dataset
dataset <- read.csv("SampledDataset_10000.csv")

# Step 2: Create label: treat class 1 and 2 as 1
dataset$label <- ifelse(dataset$Diabetes_012 == 0, 0, 1)

# Step 3: Convert categorical variables to factors
categorical_vars <- c("Age", "Education", "Income", "GenHlth")
dataset[categorical_vars] <- lapply(dataset[categorical_vars], as.factor)

# Step 4: One-hot encoding using caret::dummyVars (ensures consistency between train and test)
if (!require(caret)) install.packages("caret")
library(caret)

dummy <- dummyVars(label ~ ., data = dataset)
dataset_encoded <- predict(dummy, newdata = dataset)
dataset_final <- cbind(dataset_encoded, label = dataset$label)

# Step 5: Normalize all features
normalize <- function(x) {
  rng <- max(x) - min(x)
  if (rng == 0) return(rep(0, length(x)))  # 或 rep(0.5, length(x))
  return((x - min(x)) / rng)
}

dataset_norm <- as.data.frame(lapply(dataset_final, normalize))

# Step 6: Train-test split (75%/25%)
set.seed(12345)
n <- nrow(dataset_norm)
train_index <- 1:round(0.75 * n)
test_index <- (round(0.75 * n) + 1):n

train_data <- dataset_norm[train_index, ]
test_data <- dataset_norm[test_index, ]

# Step 7: Downsample majority class in training set
train_data$label <- as.factor(train_data$label)
downsampled <- downSample(x = train_data[, -which(names(train_data) == "label")],
                          y = train_data$label,
                          yname = "label")

# 将 factor 类型的 label 转换为 numeric
downsampled$label <- as.numeric(as.character(downsampled$label))

# Step 8: Build a 2-layer ANN with 5 nodes each, softplus activation
if (!require(neuralnet)) install.packages("neuralnet")
library(neuralnet)

softplus <- function(x) { log(1 + exp(x)) }

# Build neural net formula
formula_str <- paste("label ~", paste(setdiff(names(downsampled), "label"), collapse = " + "))
nn_formula <- as.formula(formula_str)

# Train the model
set.seed(12345)
ann_model <- neuralnet(nn_formula,
                       data = downsampled,
                       hidden = c(5, 5),
                       act.fct = softplus,
                       linear.output = FALSE,
                       stepmax = 1e+06)

# Step 9: Plot the network
plot(ann_model)

# Step 10: Predict on test data (ensure same columns as training)
test_data_fixed <- test_data[, names(downsampled)[-which(names(downsampled) == "label")]]

results <- compute(ann_model, test_data_fixed)
pred <- results$net.result
pred_class <- ifelse(pred > 0.5, 1, 0)
actual <- as.numeric(as.character(test_data$label))

# Step 11: Calculate recall
TP <- sum(pred_class == 1 & actual == 1)
FN <- sum(pred_class == 0 & actual == 1)
recall <- TP / (TP + FN)
print(paste("Recall:", round(recall, 4)))
