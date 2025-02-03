# Load necessary libraries
library(ggplot2)
library(caret)
library(dplyr)

# 1. Simulate the example data for training and testing
set.seed(42)

train_data <- data.frame(
  GrLivArea = c(1710, 1262, 1786, 2198, 1362, 953),
  Neighborhood = factor(c("CollgCr", "Veenker", "CollgCr", "Crawfor", "NoRidge", "Mitchel")),
  OverallQual = c(7, 6, 7, 8, 8, 5),
  GarageArea = c(548, 460, 608, 642, 480, 234),
  SalePrice = c(208500, 181500, 223500, 250000, 200000, 145000)
)

test_data <- data.frame(
  GrLivArea = c(2000, 1500, 2500, 1000, 1200),
  Neighborhood = factor(c("CollgCr", "Veenker", "Crawfor", "Mitchel", "NoRidge")),
  OverallQual = c(8, 7, 9, 5, 6),
  GarageArea = c(550, 500, 600, 250, 400),
  SalePrice = NA  # We'll predict this
)

# 2. Data Preprocessing: Handle missing values (if any)
train_data <- train_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

test_data <- test_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Convert categorical variables to factors
train_data$Neighborhood <- as.factor(train_data$Neighborhood)
test_data$Neighborhood <- as.factor(test_data$Neighborhood)

# 3. Exploratory Data Analysis (EDA)
# Correlation between GrLivArea and SalePrice
ggplot(train_data, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", col = "blue") +
  ggtitle("Ground Living Area vs Sale Price") +
  theme_minimal()

# Box plot for Neighborhood and SalePrice
ggplot(train_data, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  ggtitle("Neighborhood vs Sale Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

# 4. Build the Model (Linear Regression)
model <- train(SalePrice ~ GrLivArea + Neighborhood + OverallQual + GarageArea,
               data = train_data,
               method = "lm")

# Print model summary
print(summary(model))

# 5. Make Predictions on the test data
test_data$Predicted_SalePrice <- predict(model, newdata = test_data)

# 6. Visualize Predicted vs Actual Sale Price
ggplot(test_data, aes(x = Predicted_SalePrice, y = SalePrice)) +
  geom_point(alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  ggtitle("Predicted vs Actual Sale Price") +
  labs(x = "Predicted Sale Price", y = "Actual Sale Price") +
  theme_minimal()

# 7. Output the predictions
print(test_data[, c("GrLivArea", "Neighborhood", "Predicted_SalePrice")])
