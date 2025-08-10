setwd("C:/Users/18721/OneDrive - Rose-Hulman Institute of Technology/Rose-Hulman/course/CSSE/CSSE286/git_project/K-means")

credit <- read.csv("Customer Data.csv", stringsAsFactors = FALSE)  # Change to your file name
str(credit)
summary(credit)

## Data preparation 

# Remove customer ID 
credit$CUST_ID <- NULL

# Check for missing values
colSums(is.na(credit))

# Handle missing values and replace with column mean
for (col in names(credit)) {
  credit[[col]][is.na(credit[[col]])] <- mean(credit[[col]], na.rm = TRUE)
}

# Confirm missing values have been handled
colSums(is.na(credit))

## Standardize data
credit_z <- as.data.frame(scale(credit))

summary(credit$BALANCE)
summary(credit_z$BALANCE)

set.seed(1234)  
k <- 5          
credit_clusters <- kmeans(credit_z, centers = k)

credit_clusters$size       
credit_clusters$centers    


credit$cluster <- credit_clusters$cluster

head(credit[, c("cluster", "BALANCE", "PURCHASES", "CASH_ADVANCE")])

aggregate(BALANCE ~ cluster, data = credit, mean)

# Average purchases per cluster
aggregate(PURCHASES ~ cluster, data = credit, mean)

# Average cash advance per cluster
aggregate(CASH_ADVANCE ~ cluster, data = credit, mean)

# Average credit limit per cluster
aggregate(CREDIT_LIMIT ~ cluster, data = credit, mean)


library(factoextra)

fviz_cluster(credit_clusters, data = credit_z, geom = "point")


