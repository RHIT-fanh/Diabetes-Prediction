
#rm(list = ls())        
#cat("\014")           
#setwd("D:/OneDrive - Rose-Hulman Institute of Technology/Rose-Hulman/course/CSSE/CSSE286/git_project/Diabetes-Prediction")


dataset <- read.csv("SampledDataset_10000.csv")

# set 1 and 2 to 1
dataset$label <- ifelse(dataset$Diabetes_012 == 0, 0, 1)

# change cat type to one hot
categorical_vars <- c("Age", "Education", "Income", "GenHlth")
dataset[categorical_vars] <- lapply(dataset[categorical_vars], as.factor)
dummies <- model.matrix(~ Age + Education + Income + GenHlth - 1, data = dataset)
dataset_num <- cbind(dataset[, !(names(dataset) %in% c("Diabetes_012", "Age", "Education", "Income", "GenHlth"))], dummies)

# normalization 
normalize <- function(x) { return((x - min(x)) / (max(x) - min(x))) }
dataset_norm <- as.data.frame(lapply(dataset_num, normalize))

# split
set.seed(12345)
n <- nrow(dataset_norm)
train_index <- 1:round(0.75 * n)
test_index <- (round(0.75 * n) + 1):n

train_data <- dataset_norm[train_index, ]
test_data <- dataset_norm[test_index, ]


if (!require(neuralnet)) install.packages("neuralnet")
library(neuralnet)

# define soft plus
softplus <- function(x) { log(1 + exp(x)) }

# 1 layer 3 nodes neural net
formula_str <- paste("label ~", paste(setdiff(names(train_data), "label"), collapse = " + "))
nn_formula <- as.formula(formula_str)

set.seed(12345)
ann_model <- neuralnet(nn_formula,
                       data = train_data,
                       hidden = 3,             
                       act.fct = softplus,
                       linear.output = FALSE)  


plot(ann_model)


results <- compute(ann_model, test_data[, setdiff(names(test_data), "label")])
pred <- results$net.result
cor(pred, test_data$label)

pred_class <- ifelse(pred > 0.5, 1, 0)
mean(pred_class == test_data$label)  # accuracy


pred_class <- ifelse(pred > 0.5, 1, 0)


actual <- test_data$label


TP <- sum(pred_class == 1 & actual == 1)  # True Positives
FN <- sum(pred_class == 0 & actual == 1)  # False Negatives

# Recall = TP / (TP + FN)
recall <- TP / (TP + FN)
print(paste("Recall:", round(recall, 4)))

