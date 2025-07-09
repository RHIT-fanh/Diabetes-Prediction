
data <- read.csv("Dataset.csv")

# Convert categorical variables to factors
factor_vars <- c("HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", 
                 "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", 
                 "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", 
                 "GenHlth", "Sex", "Age", "Education", "Income")
data[factor_vars] <- lapply(data[factor_vars], factor)


data$Diabetes_binary <- ifelse(data$Diabetes_012 == 0, 0, 1)
print(table(data$Diabetes_binary))


set.seed(42)
train_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

##logistic regression
logit_model <- glm(Diabetes_binary ~ . -Diabetes_012, 
                   data = train_data, 
                   family = "binomial")
summary(logit_model)

predicted_probs <- predict(logit_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predicted_probs >= 0.3, 1, 0)
accuracy <- mean(predicted_class == test_data$Diabetes_binary)
cat(sprintf("Accuracy = %.2f%%\n", accuracy * 100))

## TPR and FPR 
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$Diabetes_binary)
print("Confusion Matrix:")
print(conf_matrix)

TN <- conf_matrix["0", "0"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]
TP <- conf_matrix["1", "1"]

TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)

cat(sprintf("TPR (Recall) = %.4f\n", TPR))
cat(sprintf("FPR (False Positive Rate) = %.4f\n", FPR))


