
data <- read.csv("Dataset.csv")

data$Diabetes_binary <- ifelse(data$Diabetes_012 == 0, 0, 1)

data$Age2 <- data$Age^2
data$BMI2 <- data$BMI^2
data$MentHlth2 <- data$MentHlth^2
data$PhysHlth2 <- data$PhysHlth^2

##interaction terms 
data$BP_BMI        <- data$HighBP * data$BMI
data$Age_Activity  <- data$Age * data$PhysActivity
data$Edu_Income    <- data$Education * data$Income
data$HealthyDiet   <- data$Fruits * data$Veggies
data$CardioEvent   <- data$HeartDiseaseorAttack * data$Stroke
data$SexAge        <- data$Sex * data$Age
data$WalkAge       <- data$DiffWalk * data$Age
data$Chol_BP       <- data$HighChol * data$HighBP

set.seed(42)
train_index <- sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data  <- data[-train_index, ]
weights <- ifelse(train_data$Diabetes_binary == 1, 7, 1)

##logistic regression model 
logit_model <- glm(Diabetes_binary ~ . -Diabetes_012 +
                     Age2 + BMI2 + MentHlth2 + PhysHlth2 +
                     BP_BMI + Age_Activity + Edu_Income + HealthyDiet +
                     CardioEvent + SexAge + WalkAge + Chol_BP,
                   data = train_data,
                   family = "binomial",
                   weights = weights)

##Make predictions 
probs <- predict(logit_model, newdata = test_data, type = "response")
pred <- ifelse(probs >= 0.5, 1, 0)  # Use default 0.5 threshold

##(accuracy, TPR, FPR) 
truth <- test_data$Diabetes_binary

accuracy <- mean(pred == truth)
TP <- sum(pred == 1 & truth == 1)
FP <- sum(pred == 1 & truth == 0)
FN <- sum(pred == 0 & truth == 1)
TN <- sum(pred == 0 & truth == 0)

TPR <- TP / (TP + FN)  # True Positive Rate (Recall)
FPR <- FP / (FP + TN)  # False Positive Rate

cat(sprintf("Accuracy = %.2f%%\n", accuracy * 100))
cat(sprintf("TPR (Recall) = %.4f\n", TPR))
cat(sprintf("FPR (False Positive Rate) = %.4f\n", FPR))

