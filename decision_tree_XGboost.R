
df <- read.csv("Dataset.csv")

df$Diabetes_binary <- ifelse(df$Diabetes_012 == 0, 0, 1)

# Use numerical features
num_cols <- c(
  "BMI",
  "PhysHlth",
  "MentHlth",
  "GenHlth",
  "Age",
  "Income",
  "Education"
)


df_num <- df[, c("Diabetes_binary", num_cols)]

# 80-20 division
set.seed(42)  
train_idx <- sample(nrow(df_num), 0.8 * nrow(df_num))

train_data <- df_num[train_idx, ]
test_data  <- df_num[-train_idx, ]


train_matrix <- as.matrix(train_data[, num_cols])
test_matrix  <- as.matrix(test_data[, num_cols])


train_label <- train_data$Diabetes_binary
test_label  <- test_data$Diabetes_binary

# xgboost factor calculation, used for adding weight to true label
num_neg <- sum(train_label == 0)
num_pos <- sum(train_label == 1)
scale_pos_weight <- num_neg / num_pos
cat("scale_pos_weight = ", scale_pos_weight, "\n")


library(xgboost)

# convert to DMatrix, a special form for xgboost
dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest  <- xgb.DMatrix(data = test_matrix, label = test_label)


params <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  eta = 0.1,
  max_depth = 6,
  scale_pos_weight = scale_pos_weight # calculated before
)


bst <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  verbose = 1
)


library(gmodels)


pred_prob <- predict(bst, dtest)


pred_label <- ifelse(pred_prob > 0.5, 1, 0)


CrossTable(
  x = test_label,
  y = pred_label,
  prop.chisq = FALSE,
  prop.r = FALSE,
  prop.c = FALSE,
  dnn = c("Actual", "Predicted")
)


