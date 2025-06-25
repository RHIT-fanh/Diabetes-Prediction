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

# prepare data frames for kknn
train_df <- data.frame(train_x, Diabetes_012 = train_y)
test_df  <- data.frame(test_x)

# use kknn
knn_fit <- kknn(Diabetes_012 ~ .,
                train = train_df,
                test  = test_df,
                k = 3,
                distance = 2,
                kernel = "triangular")

# set a prob threshold for yes to increase its weight
prob_yes <- as.numeric(knn_fit$prob[, "Yes"])


threshold <- 0.3
pred <- ifelse(prob_yes > threshold, "Yes", "No")
pred <- factor(pred, levels = c("No", "Yes"))  # keep the order of the factor

# evaluation
cm <- table(Predicted = pred, Actual = test_y)
print(cm)

accuracy <- sum(diag(cm)) / sum(cm)
cat(sprintf("\nThreshold = %.2f → Overall Accuracy: %.2f%%\n", threshold, 100 * accuracy))

# This one makes things better a little bit, but it also cause too many TN. This can either be the problem of the 10000 random data we selected
# or the limit of KKNN itself.
