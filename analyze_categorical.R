
setwd("D:/OneDrive - Rose-Hulman Institute of Technology/Rose-Hulman/course/CSSE/CSSE286/project")
data <- read.csv("Dataset.csv", stringsAsFactors = FALSE)

## catgorial variables
categorical_vars <- c(
  "Diabetes_binary", "HighBP", "HighChol", "CholCheck", "Smoker",
  "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies",
  "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth",
  "DiffWalk", "Sex", "Education", "Income"
)
vars_in_data <- intersect(categorical_vars, names(data))
if (length(vars_in_data) == 0) stop("not found in data")

## convert to factor
data[vars_in_data] <- lapply(data[vars_in_data], as.factor)


library(gmodels)
library(ggplot2)
library(rlang)

## folder for image output
 dir.create("pair_plots", showWarnings = FALSE)

## save string output to txt file
sink("analysis_output.txt", split = TRUE)   

cat("features\n")

## one-way table, percentage and central tendancy
for (v in vars_in_data) {
  cat("\n====================================================\n")
  cat("variable：", v, "\n", sep = "")
  
  freq_tab <- table(data[[v]])
  print(freq_tab)
  
  pct <- round(prop.table(freq_tab) * 100, 1)
  cat("\n-- percentage (%) --\n")
  print(pct)
  
  max_n <- max(freq_tab)
  modes <- names(freq_tab)[freq_tab == max_n]
  cat("\n-- central tendancy (count =", max_n, ") --\n")
  cat(paste(modes, collapse = ", "), "\n")
}

## scatter plot + two way cross tabulation
cat("\n\n========== plot + tabulation ==========\n")

for (pair in combn(vars_in_data, 2, simplify = FALSE)) {
  var1 <- pair[1]; var2 <- pair[2]
  
  cat("\n========================================\n",
      "Cross tabulation：", var1, " vs ", var2, "\n", sep = "")
  
  CrossTable(
    data[[var1]], data[[var2]],
    chisq      = TRUE,
    prop.chisq = TRUE,
    prop.r     = TRUE,
    prop.c     = TRUE,
    prop.t     = TRUE
  )
  
  # scater plot
  p <- ggplot(data, aes(x = !!sym(var1), y = !!sym(var2))) +
    geom_count(alpha = 0.7, show.legend = TRUE) +
    scale_size_area(max_size = 10) +
    labs(title = paste(var1, "vs", var2, "(count bubble plot)"),
         x = var1, y = var2, size = "Count") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(face = "bold", hjust = 0.5))
  
  print(p)   
  
  # svae the img file
   ggsave(filename = paste0("pair_plots/", var1, "_vs_", var2, ".png"),
          plot = p, width = 6, height = 5, dpi = 300)
}


sink()

cat("finished")

