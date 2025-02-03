# Load required libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071)

# Load the data
data <- read.csv("credit.csv")

# Check for missing values
sum(is.na(data))

# Handle missing values (impute with median or mean)
data$Amount[is.na(data$Amount)] <- median(data$Amount, na.rm = TRUE)

# Normalize numerical features (e.g., Amount, Time)
data$Amount <- scale(data$Amount)

# Handle class imbalance using SMOTE (if the 'Class' column has the fraud labels: 0=Legit, 1=Fraud)
set.seed(123)
data_smote <- SMOTE(Class ~ ., data = data, perc.over = 100, perc.under = 200)

# Check the distribution of the classes
table(data_smote$Class)

