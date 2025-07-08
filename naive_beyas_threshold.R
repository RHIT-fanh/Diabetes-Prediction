## with threshold


setwd("D:/OneDrive - Rose-Hulman Institute of Technology/Rose-Hulman/course/CSSE/CSSE286/git_project/Diabetes-Prediction")


library(naivebayes)

library(gmodels)  


df <- read.csv("Dataset.csv")

# let 1 and 2 both be true(have diabetes)
df$Diabetes_012 <- ifelse(df$Diabetes_012 == 2, 1, df$Diabetes_012)
df$Diabetes_012 <- factor(df$Diabetes_012)

# select cat type feature
cat_vars <- c(
  "HighBP", "HighChol", "CholCheck", "Smoker", "Stroke",
  "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
  "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
  "DiffWalk", "Sex", "Education", "Income", "GenHlth", "Age"
)

df[cat_vars] <- lapply(df[cat_vars], factor)

# divide test and train set
n_total <- nrow(df)
n_train <- floor(0.8 * n_total)  

train_data <- df[1:n_train, ]
test_data  <- df[(n_train + 1):n_total, ]

train_labels <- train_data$Diabetes_012
test_labels  <- test_data$Diabetes_012


nb_model <- naive_bayes(
  x = train_data[, cat_vars],
  y = train_labels,
  laplace = 1
)


nb_pred <- predict(nb_model, newdata = test_data[, cat_vars])


CrossTable(
  x = nb_pred,
  y = test_labels,
  prop.chisq = FALSE,
  prop.r = FALSE,
  prop.c = FALSE,
  dnn = c("Predicted", "Actual")
)


accuracy <- mean(nb_pred == test_labels)
cat("Accuracy: ", round(accuracy, 4), "\n")



# setting a threshold whcih decrease accuracy but increase recall
nb_prob <- predict(nb_model, newdata = test_data[, cat_vars], type = "prob")


# threshold
custom_threshold <- 0.3
nb_custom_pred <- ifelse(nb_prob[, "1"] >= custom_threshold, "1", "0")
nb_custom_pred <- factor(nb_custom_pred, levels = c("0", "1"))

cat("\n--- With custom threshold =", custom_threshold, "---\n")
CrossTable(
  x = nb_custom_pred,
  y = test_labels,
  prop.chisq = FALSE,
  prop.r = FALSE,
  prop.c = FALSE,
  dnn = c("Predicted", "Actual")
)

accuracy_custom <- mean(nb_custom_pred == test_labels)
cat("Accuracy with custom threshold: ", round(accuracy_custom, 4), "\n")

# recall
TP <- sum(nb_custom_pred == "1" & test_labels == "1")
FN <- sum(nb_custom_pred == "0" & test_labels == "1")

recall <- TP / (TP + FN)
cat("Recall with custom threshold: ", round(recall, 4), "\n")
