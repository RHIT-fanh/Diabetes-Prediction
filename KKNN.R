library(kknn)
df <- read.csv("SampledDataset_10000.csv")

# turn diagnosis into factor（0=No, 1=Yes）
df$Diabetes_012 <- factor(df$Diabetes_012,
                          levels = c(0, 1),
                          labels  = c("No", "Yes"))

# select numeric features
vars <- c("BMI", "HighBP", "HighChol")
df_x <- scale(df[ , vars])            # stadardlization
df_y <- df$Diabetes_012

# split test and train 20 vs 80
n         <- nrow(df)
n_train   <- floor(0.8 * n)
train_id  <- 1:n_train
test_id   <- (n_train + 1):n

train_x <- df_x[train_id, ]
test_x  <- df_x[test_id, ]
train_y <- df_y[train_id]
test_y  <- df_y[test_id]

# Using kknn
knn_fit <- kknn(Diabetes_012 ~ .,
                train = data.frame(train_x, Diabetes_012 = train_y),
                test  = data.frame(test_x),
                k = 5,              
                distance = 2,       
                kernel = "triangular")

pred <- fitted(knn_fit)

# evaluation
cm <- table(Predicted = pred, Actual = test_y)
print(cm)

accuracy <- sum(diag(cm)) / sum(cm)
cat(sprintf("\nOverall accuracy: %.2f%%\n", 100 * accuracy))

# Accuracy is 98.04, but it is fake. The problem here is, we have too many no_diabete data, meaning a huge bias even with low k and triangular kernel
# So the model nearly only predicts false.


## with cross validation for best k, very slow for the whole dataset

library(kknn)

df <- read.csv("SampledDataset_10000.csv")

# turn diagnosis into factor（0=No, 1=Yes）
df$Diabetes_012 <- factor(df$Diabetes_012,
                          levels = c(0, 1),
                          labels  = c("No", "Yes"))

# select numeric features
vars <- c("BMI", "HighBP", "HighChol")
df_x <- scale(df[ , vars])            # stadardlization
df_y <- df$Diabetes_012

# split test and train 20 vs 80
n         <- nrow(df)
n_train   <- floor(0.8 * n)
train_id  <- 1:n_train
test_id   <- (n_train + 1):n

train_x <- df_x[train_id, ]
test_x  <- df_x[test_id, ]
train_y <- df_y[train_id]
test_y  <- df_y[test_id]

# find best k using cross-validation
train_df <- data.frame(train_x, Diabetes_012 = train_y)
cv_result <- train.kknn(Diabetes_012 ~ ., data = train_df, kmax = 15, kernel = "triangular")
best_k <- cv_result$best.parameters$k
cat(sprintf("Best k found by cross-validation: %d\n", best_k))

# Using kknn
knn_fit <- kknn(Diabetes_012 ~ .,
                train = data.frame(train_x, Diabetes_012 = train_y),
                test  = data.frame(test_x),
                k = best_k,              
                distance = 2,       
                kernel = "triangular")

pred <- fitted(knn_fit)

# evaluation
cm <- table(Predicted = pred, Actual = test_y)
print(cm)

accuracy <- sum(diag(cm)) / sum(cm)
cat(sprintf("\nOverall accuracy: %.2f%%\n", 100 * accuracy))

# Even with best k, still not predicting much true