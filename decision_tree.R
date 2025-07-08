# ------------------------------------------------
# 1. 读入数据 & 创建二分类 Label
# ------------------------------------------------
df <- read.csv("Dataset.csv")
df$Diabetes_binary <- ifelse(df$Diabetes_012 == 0, 0, 1)

# ------------------------------------------------
# 2. 转换分类变量
# ------------------------------------------------
cat_cols <- c(
  "HighBP", "HighChol", "CholCheck",
  "Smoker", "Stroke", "HeartDiseaseorAttack",
  "PhysActivity", "Fruits", "Veggies",
  "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
  "DiffWalk", "Sex", "Education", "Income"
)

df[cat_cols] <- lapply(df[cat_cols], factor)

# 只保留 cat 特征 + label
df_cat <- df[, c("Diabetes_binary", cat_cols)]

# ------------------------------------------------
# 3. 按 80/20 随机划分
# ------------------------------------------------
set.seed(42)
sample_idx <- sample(nrow(df_cat), 0.8 * nrow(df_cat))
train_data <- df_cat[sample_idx, ]
test_data  <- df_cat[-sample_idx, ]

# C5.0 需要因变量是 factor
train_data$Diabetes_binary <- as.factor(train_data$Diabetes_binary)
test_data$Diabetes_binary  <- as.factor(test_data$Diabetes_binary)

# ------------------------------------------------
# 4. 定义 Cost Matrix
# ------------------------------------------------
# 行 = Predicted, 列 = Actual
# Cost Matrix:
# [ Predicted no | Predicted yes ]
# Actual no  → 正确 = 0
# Actual yes → FN = 4 (漏诊代价)
# FP = 1 (错杀代价)

cost_mat <- matrix(c(0, 1, 20, 0), nrow = 2,
                   dimnames = list(
                     Predicted = c("0", "1"),
                     Actual = c("0", "1")
                   ))

cost_mat

# ------------------------------------------------
# 5. 训练 C5.0 模型（带 Boosting + Cost Matrix）
# ------------------------------------------------
library(C50)

model_cost <- C5.0(
  Diabetes_binary ~ .,
  data = train_data,
  trials = 10,
  costs = cost_mat
)

# ------------------------------------------------
# 6. 在测试集上预测
# ------------------------------------------------
pred_cost <- predict(model_cost, test_data)

# ------------------------------------------------
# 7. 输出混淆矩阵
# ------------------------------------------------
library(gmodels)

CrossTable(
  x = test_data$Diabetes_binary,
  y = pred_cost,
  prop.chisq = FALSE,
  prop.c = FALSE,
  prop.r = FALSE,
  dnn = c("Actual", "Predicted")
)


