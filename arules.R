# 步骤 0：安装并加载必要的包
if (!require("arules")) install.packages("arules")
library(arules)
library(dplyr)

# 步骤 1：读取 CSV 数据
data <- read.csv("bread basket.csv", stringsAsFactors = FALSE)

# 可选：去除 NA 和空白项（必要）
data <- data %>%
  filter(Item != "", !is.na(Item))

# 可选：统一大小写（更便于合并）
data$Item <- tolower(trimws(data$Item))

# 步骤 2：聚合成事务格式列表
transaction_list <- data %>%
  group_by(Transaction) %>%
  summarise(items = list(Item)) %>%
  pull(items)

# 转换为 transactions 对象（用于 arules）
transactions <- as(transaction_list, "transactions")

# 步骤 3：事务集概要信息
summary(transactions)

# 步骤 4：可视化前 20 热门商品
itemFrequencyPlot(transactions, topN = 20, type = "relative",
                  col = "steelblue", main = "Top 20 Items")

# 步骤 5：Apriori 规则挖掘（适当调整支持度和置信度）
rules <- apriori(transactions,
                 parameter = list(supp = 0.003, conf = 0.25, minlen = 2))

# 规则概要
summary(rules)

# 步骤 6：查看提升度最高的前 10 条规则
inspect(sort(rules, by = "lift")[1:10])

# 步骤 7：筛选包含特定商品（如 coffee）的规则
coffee_rules <- subset(rules, items %in% "coffee")
inspect(coffee_rules)

# 步骤 8：将规则保存为 CSV 文件
write(rules, file = "breadbasket_rules.csv", sep = ",", quote = TRUE, row.names = FALSE)

# 步骤 9：如需进一步处理，可转为 data.frame
rules_df <- as(rules, "data.frame")
str(rules_df)
