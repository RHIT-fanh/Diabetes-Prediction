# Load required package and dataset
if (!requireNamespace("readr", quietly = TRUE)) {
    install.packages("readr")
}
library(readr)
dataset <- readr::read_csv("Dataset.csv")

# Inspect dataset structure and summary
head(dataset)
summary(dataset)
colSums(is.na(dataset))
str(dataset)

# Boxplot for numeric columns
numeric_columns <- sapply(dataset, is.numeric)
boxplot(dataset[, numeric_columns], main = "Boxplot of Numeric Columns", las = 2, col = "lightblue")

# Histograms and boxplot for selected columns
selected_columns <- c("Age", "BMI", "MentHlth")
hist(dataset[[selected_columns[1]]], main = paste("Histogram of", selected_columns[1]), col = "lightblue", xlab = selected_columns[1], breaks = seq(1, 14, by = 1), xaxt = "n")
axis(1, at = seq(1, 14, by = 1), labels = seq(1, 14, by = 1))
boxplot(dataset[[selected_columns[2]]], main = paste("Boxplot of", selected_columns[2]), col = "lightblue")
hist(dataset[[selected_columns[3]]], main = paste("Histogram of", selected_columns[3]), col = "lightblue", xlab = selected_columns[3])

# Statistical calculations for Age, BMI, and MentHlth
mean_age <- mean(dataset$Age, na.rm = TRUE)
cat("Mean of Age:", mean_age, "\n")
median_bmi <- median(dataset$BMI, na.rm = TRUE)
cat("Median of BMI:", median_bmi, "\n")
range_menthlth <- range(dataset$MentHlth, na.rm = TRUE)
cat("Range of MentHlth:", range_menthlth, "\n")
iqr_age <- IQR(dataset$Age, na.rm = TRUE)
cat("IQR of Age:", iqr_age, "\n")
var_bmi <- var(dataset$BMI, na.rm = TRUE)
sd_bmi <- sd(dataset$BMI, na.rm = TRUE)
cat("Variance of BMI:", var_bmi, "\n")
cat("Standard Deviation of BMI:", sd_bmi, "\n")

# Frequency table for Age
age_table <- table(dataset$Age)
cat("Frequency Table for Age:\n")
print(age_table)

# Diabetes_012 distribution and baseline accuracy
if ("Diabetes_012" %in% colnames(dataset)) {
    diabetes_distribution <- table(dataset$Diabetes_012)
    cat("Distribution of Diabetes_012:\n")
    print(diabetes_distribution)
    baseline_prediction <- rep(names(which.max(diabetes_distribution)), nrow(dataset))
    actual_values <- dataset$Diabetes_012
    baseline_accuracy <- mean(baseline_prediction == actual_values, na.rm = TRUE)
    cat("Baseline Accuracy:", baseline_accuracy, "\n")
} else {
    cat("The target variable 'Diabetes_012' is not found in the dataset.\n")
}

# Additional summary statistics
cat("Structure of the dataset:\n")
str(dataset)
cat("\nSummary statistics of the dataset:\n")
summary(dataset)
cat("\nMean of Age column:\n")
mean(dataset$Age, na.rm = TRUE)
cat("\nMedian of BMI column:\n")
median(dataset$BMI, na.rm = TRUE)
cat("\nRange of MentHlth column:\n")
range(dataset$MentHlth, na.rm = TRUE)
cat("\nInterquartile Range (IQR) of Age column:\n")
IQR(dataset$Age, na.rm = TRUE)
cat("\nQuantiles of Age column:\n")
quantile(dataset$Age, na.rm = TRUE)
cat("\nVariance of BMI column:\n")
var(dataset$BMI, na.rm = TRUE)
cat("\nStandard Deviation of BMI column:\n")
sd(dataset$BMI, na.rm = TRUE)
cat("\nFrequency Table for Age column:\n")
table(dataset$Age)
