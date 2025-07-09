# 1. Load data & create binary label
df <- read.csv("Dataset.csv")
df$Diabetes_binary <- ifelse(df$Diabetes_012 == 0, 0, 1)

# 2. Convert categorical variables
cat_cols <- c(
    "HighBP", "HighChol", "CholCheck",
    "Smoker", "Stroke", "HeartDiseaseorAttack",
    "PhysActivity", "Fruits", "Veggies",
    "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
    "DiffWalk", "Sex", "Education", "Income"
)

df[cat_cols] <- lapply(df[cat_cols], factor)

# Keep only categorical features + label
df_cat <- df[, c("Diabetes_binary", cat_cols)]

# 3. Split data (80/20)
set.seed(42)
sample_idx <- sample(nrow(df_cat), 0.8 * nrow(df_cat))
train_data <- df_cat[sample_idx, ]
test_data  <- df_cat[-sample_idx, ]

# Convert label to factor for RIPPER
train_data$Diabetes_binary <- as.factor(train_data$Diabetes_binary)
test_data$Diabetes_binary  <- as.factor(test_data$Diabetes_binary)

# 4. Train RIPPER model
options(java.parameters = "-Xmx4g")  # Increase Java heap size
library(RWeka)

model_ripper <- JRip(
  Diabetes_binary ~ .,
  data = train_data
)

# 5. Predict on test set
pred_ripper <- predict(model_ripper, test_data)

# 6. Confusion matrix
library(gmodels)

CrossTable(
    x = test_data$Diabetes_binary,
    y = pred_ripper,
    prop.chisq = FALSE,
    prop.c = FALSE,
    prop.r = FALSE,
    dnn = c("Actual", "Predicted")
)

# 7. Calculate metrics
conf_matrix <- table(Actual = test_data$Diabetes_binary, Predicted = pred_ripper)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# True Positive Rate (TPR)
tpr <- conf_matrix["1", "1"] / sum(conf_matrix["1", ])

# False Negative Rate (FNR)
fnr <- conf_matrix["1", "0"] / sum(conf_matrix["1", ])

# Print results
cat("Accuracy:", accuracy, "\n")
cat("TPR (Sensitivity):", tpr, "\n")
cat("FNR:", fnr, "\n")
