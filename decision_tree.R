
df <- read.csv("Dataset.csv")
df$Diabetes_binary <- ifelse(df$Diabetes_012 == 0, 0, 1)

# cat type
cat_cols <- c(
  "HighBP", "HighChol", "CholCheck",
  "Smoker", "Stroke", "HeartDiseaseorAttack",
  "PhysActivity", "Fruits", "Veggies",
  "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
  "DiffWalk", "Sex", "Education", "Income"
)

df[cat_cols] <- lapply(df[cat_cols], factor)


df_cat <- df[, c("Diabetes_binary", cat_cols)]

# 80-20 division
set.seed(42)
sample_idx <- sample(nrow(df_cat), 0.8 * nrow(df_cat))
train_data <- df_cat[sample_idx, ]
test_data  <- df_cat[-sample_idx, ]

train_data$Diabetes_binary <- as.factor(train_data$Diabetes_binary)
test_data$Diabetes_binary  <- as.factor(test_data$Diabetes_binary)

# cost matrix

cost_mat <- matrix(c(0, 1, 20, 0), nrow = 2,
                   dimnames = list(
                     Predicted = c("0", "1"),
                     Actual = c("0", "1")
                   ))

cost_mat


library(C50)

model_cost <- C5.0(
  Diabetes_binary ~ .,
  data = train_data,
  trials = 10,
  costs = cost_mat
)


pred_cost <- predict(model_cost, test_data)


library(gmodels)

CrossTable(
  x = test_data$Diabetes_binary,
  y = pred_cost,
  prop.chisq = FALSE,
  prop.c = FALSE,
  prop.r = FALSE,
  dnn = c("Actual", "Predicted")
)


